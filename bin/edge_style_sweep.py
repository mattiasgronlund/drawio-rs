#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import json
import os
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


def build_variants(
    base_style: dict[str, str],
    base_attrs: dict[str, str],
    sweep: dict[str, list[str]],
) -> list[tuple[str, str, str]]:
    variants: list[tuple[str, str, str]] = []
    for key, values in sweep.items():
        section, inner_key = split_section_key(key)
        base = base_style.get(inner_key) if section == "style" else base_attrs.get(inner_key)
        for value in values:
            if base == value:
                continue
            variants.append((key, base or "", value))
    return variants


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


def split_section_key(key: str) -> tuple[str, str]:
    if ":" not in key:
        return ("style", key)
    section, inner = key.split(":", 1)
    if section not in ("attributes", "style"):
        return ("style", key)
    return (section, inner)


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Generate .drawio variants by sweeping edge or vertex attributes."
    )
    parser.add_argument("input", type=Path, help="Base .drawio file")
    parser.add_argument("--cell-id", required=True, help="mxCell id to edit")
    parser.add_argument(
        "--attributes-json",
        type=Path,
        help="edge_attributes.json or vertex_attributes.json inventory",
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
        default=Path("tmp/edge-style-sweep"),
        help="Output directory",
    )
    args = parser.parse_args()

    tree = ET.parse(args.input)
    root = tree.getroot()
    cell = root.find(f".//mxCell[@id='{args.cell_id}']")
    if cell is None:
        raise SystemExit(f"cell id {args.cell_id} not found")

    base_attribs = dict(cell.attrib)
    base_style = parse_style(base_attribs.get("style", ""))
    base_attrs = {k: v for k, v in base_attribs.items() if k != "style"}

    sweep: dict[str, list[str]]
    cell_kind = "cell"
    if args.attributes_json:
        inv = json.loads(args.attributes_json.read_text())
        sweep = build_sweep_from_inventory(inv, set(args.skip_attr))
        if "edge_attributes" in args.attributes_json.stem:
            cell_kind = "edge"
        elif "vertex_attributes" in args.attributes_json.stem:
            cell_kind = "vertex"
    else:
        sweep = {
            "style:edgeStyle": [
                "orthogonalEdgeStyle",
                "entityRelationEdgeStyle",
                "isometricEdgeStyle",
                "none",
            ],
            "style:elbow": ["horizontal", "vertical"],
            "style:rounded": ["0", "1"],
            "style:startArrow": ["none", "block", "classic"],
            "style:endArrow": ["none", "block", "classic"],
            "style:shape": ["flexArrow", "link", "arrow"],
            "style:strokeWidth": ["1", "2", "3"],
            "style:exitX": ["0", "0.5", "1"],
            "style:exitY": ["0", "0.5", "1"],
            "style:entryX": ["0", "0.5", "1"],
            "style:entryY": ["0", "0.5", "1"],
        }

    variants = build_variants(base_style, base_attrs, sweep)
    if not variants:
        raise SystemExit("no variants generated; check base style values")

    os.makedirs(args.out_dir, exist_ok=True)
    os.makedirs(args.out_dir / "drawio", exist_ok=True)
    os.makedirs(args.out_dir / "svg", exist_ok=True)
    csv_path = args.out_dir / "variants.csv"

    with csv_path.open("w", newline="") as csv_file:
        writer = csv.writer(csv_file)
        writer.writerow(["file", "key", "base", "value", "edge_id", "cell_id", "cell_kind"])
        for key, base, value in variants:
            cell.attrib.clear()
            cell.attrib.update(base_attribs)
            section, inner_key = split_section_key(key)
            if section == "attributes":
                cell.set(inner_key, value)
            else:
                cell.set("style", serialize_style({**base_style, inner_key: value}))
            filename = (
                f"{cell_kind}_{args.cell_id}__{sanitize(section)}__"
                f"{sanitize(inner_key)}__{sanitize(value)}.drawio"
            )
            out_path = args.out_dir / "drawio" / filename
            tree.write(out_path, encoding="utf-8", xml_declaration=False)
            writer.writerow(
                [str(out_path), key, base, value, args.cell_id, args.cell_id, cell_kind]
            )

    print(f"wrote {len(variants)} variants to {args.out_dir}")
    print(f"index: {csv_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
