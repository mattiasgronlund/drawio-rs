#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import json
import os
from itertools import combinations
from pathlib import Path
from xml.etree import ElementTree as ET


def parse_style(style: str) -> dict[str, str]:
    parts = [p for p in style.split(";") if p]
    out: dict[str, str] = {}
    for part in parts:
        if "=" in part:
            key, value = part.split("=", 1)
            out[key] = value
    return out


def serialize_style(style: dict[str, str]) -> str:
    parts = [f"{k}={v}" for k, v in style.items()]
    return ";".join(parts) + ";"


def sanitize(value: str) -> str:
    return "".join(c if c.isalnum() or c in ("-", "_") else "_" for c in value)


def split_section_key(key: str) -> tuple[str, str]:
    if ":" not in key:
        return ("style", key)
    section, inner = key.split(":", 1)
    if section not in ("attributes", "style"):
        return ("style", key)
    return (section, inner)


def bool_values(values: list[str]) -> list[str]:
    if any(v in ("0", "1") for v in values):
        return ["0", "1"]
    return ["false", "true"]


def numeric_values(values: list[str]) -> list[str]:
    numbers = sorted(int(v) for v in values)
    if len(numbers) == 1:
        base = numbers[0]
        return [str(base - 1), str(base), str(base + 1)]
    if len(numbers) > 10:
        mid = numbers[len(numbers) // 2]
        return [str(numbers[0]), str(mid), str(numbers[-1])]
    return [str(v) for v in numbers]


def numeric_values_float(values: list[str]) -> list[str]:
    numbers = sorted(float(v) for v in values)
    if len(numbers) == 1:
        base = numbers[0]
        return [str(base - 1.0), str(base), str(base + 1.0)]
    if len(numbers) > 10:
        mid = numbers[len(numbers) // 2]
        return [str(numbers[0]), str(mid), str(numbers[-1])]
    return [str(v) for v in numbers]


def build_sweep_from_inventory(
    inv: dict[str, dict[str, dict[str, list[str]]]],
    skip_attributes: set[str],
) -> dict[str, list[str]]:
    sweep: dict[str, list[str]] = {}
    for section in ("attributes", "style"):
        entries = inv.get(section, {})
        for key, data in entries.items():
            if section == "attributes" and key in skip_attributes:
                continue
            values = data.get("values", [])
            if not values:
                continue
            value_type = data.get("type")
            if value_type == "bool":
                sweep[f"{section}:{key}"] = bool_values(values)
            elif value_type == "int":
                sweep[f"{section}:{key}"] = numeric_values(values)
            elif value_type == "float":
                sweep[f"{section}:{key}"] = numeric_values_float(values)
            else:
                sweep[f"{section}:{key}"] = list(values)
    return sweep


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Generate .drawio variants by sweeping pairs of attributes."
    )
    parser.add_argument("input", type=Path, help="Base .drawio file")
    parser.add_argument("--cell-id", required=True, help="mxCell id to edit")
    parser.add_argument(
        "--attributes-json",
        type=Path,
        required=True,
        help="edge_attributes.json or vertex_attributes.json inventory",
    )
    parser.add_argument(
        "--key",
        action="append",
        default=[],
        help="Attribute key to include (repeatable, e.g. style:edgeStyle)",
    )
    parser.add_argument(
        "--skip-attr",
        action="append",
        default=[],
        help="Attribute keys to skip (repeatable)",
    )
    parser.add_argument(
        "--out-dir",
        type=Path,
        default=Path("tmp/edge-style-sweep/pairs"),
        help="Output directory",
    )
    args = parser.parse_args()

    if len(args.key) < 2:
        raise SystemExit("provide at least two --key values for pairwise sweep")

    tree = ET.parse(args.input)
    root = tree.getroot()
    cell = root.find(f".//mxCell[@id='{args.cell_id}']")
    if cell is None:
        raise SystemExit(f"cell id {args.cell_id} not found")

    base_attribs = dict(cell.attrib)
    base_style = parse_style(base_attribs.get("style", ""))
    base_attrs = {k: v for k, v in base_attribs.items() if k != "style"}

    inv = json.loads(args.attributes_json.read_text())
    sweep = build_sweep_from_inventory(inv, set(args.skip_attr))

    requested = args.key
    missing = [key for key in requested if key not in sweep]
    if missing:
        raise SystemExit(f"missing keys in inventory: {', '.join(missing)}")

    cell_kind = "cell"
    if "edge_attributes" in args.attributes_json.stem:
        cell_kind = "edge"
    elif "vertex_attributes" in args.attributes_json.stem:
        cell_kind = "vertex"

    os.makedirs(args.out_dir, exist_ok=True)
    os.makedirs(args.out_dir / "drawio", exist_ok=True)
    csv_path = args.out_dir / "variants.csv"

    with csv_path.open("w", newline="") as csv_file:
        writer = csv.writer(csv_file)
        writer.writerow(
            [
                "file",
                "key",
                "base",
                "value",
                "key1",
                "value1",
                "key2",
                "value2",
                "edge_id",
                "cell_id",
                "cell_kind",
            ]
        )
        count = 0
        for key1, key2 in combinations(requested, 2):
            section1, inner1 = split_section_key(key1)
            section2, inner2 = split_section_key(key2)
            base1 = base_style.get(inner1) if section1 == "style" else base_attrs.get(inner1)
            base2 = base_style.get(inner2) if section2 == "style" else base_attrs.get(inner2)
            for value1 in sweep[key1]:
                for value2 in sweep[key2]:
                    if value1 == (base1 or "") and value2 == (base2 or ""):
                        continue
                    cell.attrib.clear()
                    cell.attrib.update(base_attribs)
                    style = dict(base_style)
                    if section1 == "style":
                        style[inner1] = value1
                    else:
                        cell.set(inner1, value1)
                    if section2 == "style":
                        style[inner2] = value2
                    else:
                        cell.set(inner2, value2)
                    if section1 == "style" or section2 == "style":
                        cell.set("style", serialize_style(style))

                    filename = (
                        f"{cell_kind}_{args.cell_id}__pair__"
                        f"{sanitize(key1)}__{sanitize(value1)}__"
                        f"{sanitize(key2)}__{sanitize(value2)}.drawio"
                    )
                    out_path = args.out_dir / "drawio" / filename
                    tree.write(out_path, encoding="utf-8", xml_declaration=False)
                    writer.writerow(
                        [
                            str(out_path),
                            f"{key1}+{key2}",
                            f"{base1 or ''}|{base2 or ''}",
                            f"{value1}|{value2}",
                            key1,
                            value1,
                            key2,
                            value2,
                            args.cell_id,
                            args.cell_id,
                            cell_kind,
                        ]
                    )
                    count += 1

    print(f"wrote {count} variants to {args.out_dir}")
    print(f"index: {csv_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
