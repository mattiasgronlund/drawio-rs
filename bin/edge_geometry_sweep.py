#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import json
import os
from pathlib import Path
from xml.etree import ElementTree as ET


def split_style(style: str) -> tuple[list[str], list[tuple[str, str]]]:
    tokens: list[str] = []
    kvs: list[tuple[str, str]] = []
    for part in [p for p in style.split(";") if p]:
        if "=" in part:
            key, value = part.split("=", 1)
            kvs.append((key, value))
        else:
            tokens.append(part)
    return tokens, kvs


def merge_style(
    style: str, add_tokens: list[str] | None = None, set_kv: dict[str, str] | None = None
) -> str:
    add_tokens = add_tokens or []
    set_kv = set_kv or {}
    tokens, kvs = split_style(style)
    token_set = set(tokens)
    for token in add_tokens:
        if token not in token_set:
            tokens.append(token)
            token_set.add(token)
    kv_dict = {k: v for k, v in kvs}
    kv_dict.update(set_kv)
    parts = tokens + [f"{k}={v}" for k, v in kv_dict.items()]
    return ";".join(parts) + ";"


def fmt_num(value: float) -> str:
    if abs(value - round(value)) < 1e-6:
        return str(int(round(value)))
    return f"{value:.6f}".rstrip("0").rstrip(".")


def apply_geometry(geom: ET.Element, base: dict[str, float], changes: dict[str, str | float]) -> None:
    for key in ("x", "y", "width", "height"):
        if key not in changes:
            geom.set(key, fmt_num(base[key]))
            continue
        raw = changes[key]
        if isinstance(raw, str) and (raw.startswith("+") or raw.startswith("-")):
            value = base[key] + float(raw)
        else:
            value = float(raw)
        geom.set(key, fmt_num(value))


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Generate geometry variants to make entry/exit parameters visible."
    )
    parser.add_argument("input", type=Path, help="Base .drawio file")
    parser.add_argument("--edge-id", required=True, help="mxCell id for the edge")
    parser.add_argument("--source-id", help="mxCell id for source vertex (optional)")
    parser.add_argument("--target-id", help="mxCell id for target vertex (optional)")
    parser.add_argument(
        "--out-dir",
        type=Path,
        default=Path("tmp/edge-style-sweep/geometry"),
        help="Output directory",
    )
    args = parser.parse_args()

    tree = ET.parse(args.input)
    root = tree.getroot()
    edge = root.find(f".//mxCell[@id='{args.edge_id}']")
    if edge is None:
        raise SystemExit(f"edge id {args.edge_id} not found")

    source_id = args.source_id or edge.get("source")
    target_id = args.target_id or edge.get("target")
    if not source_id or not target_id:
        raise SystemExit("source/target ids not found; pass --source-id/--target-id")

    source = root.find(f".//mxCell[@id='{source_id}']")
    target = root.find(f".//mxCell[@id='{target_id}']")
    if source is None or target is None:
        raise SystemExit("source/target cells not found")

    source_geom = source.find("mxGeometry")
    target_geom = target.find("mxGeometry")
    if source_geom is None or target_geom is None:
        raise SystemExit("source/target geometry not found")

    base_source_geom = {
        "x": float(source_geom.get("x", "0")),
        "y": float(source_geom.get("y", "0")),
        "width": float(source_geom.get("width", "0")),
        "height": float(source_geom.get("height", "0")),
    }
    base_target_geom = {
        "x": float(target_geom.get("x", "0")),
        "y": float(target_geom.get("y", "0")),
        "width": float(target_geom.get("width", "0")),
        "height": float(target_geom.get("height", "0")),
    }
    base_source_style = source.get("style", "")
    base_target_style = target.get("style", "")

    scenarios = [
        {
            "name": "baseline",
            "source": {},
            "target": {},
        },
        {
            "name": "vertical_gap",
            "source": {},
            "target": {"y": "+180"},
        },
        {
            "name": "diagonal_gap",
            "source": {},
            "target": {"x": "+160", "y": "+120"},
        },
        {
            "name": "swap_sizes",
            "source": {"width": 60, "height": 120},
            "target": {"width": 160, "height": 40},
        },
        {
            "name": "rotate_source_45",
            "source": {"style_kv": {"rotation": "45"}},
            "target": {},
        },
        {
            "name": "rotate_target_90",
            "source": {},
            "target": {"style_kv": {"rotation": "90"}},
        },
        {
            "name": "ellipse_vertices",
            "source": {"style_tokens_add": ["ellipse"]},
            "target": {"style_tokens_add": ["ellipse"]},
        },
    ]

    os.makedirs(args.out_dir, exist_ok=True)
    os.makedirs(args.out_dir / "drawio", exist_ok=True)
    csv_path = args.out_dir / "geometry_variants.csv"

    with csv_path.open("w", newline="") as csv_file:
        writer = csv.writer(csv_file)
        writer.writerow(["file", "scenario", "source", "target"])
        for scenario in scenarios:
            source_geom.attrib.clear()
            target_geom.attrib.clear()
            source_geom.attrib.update({"as": "geometry"})
            target_geom.attrib.update({"as": "geometry"})
            apply_geometry(source_geom, base_source_geom, scenario.get("source", {}))
            apply_geometry(target_geom, base_target_geom, scenario.get("target", {}))

            source_style = merge_style(
                base_source_style,
                add_tokens=scenario.get("source", {}).get("style_tokens_add"),
                set_kv=scenario.get("source", {}).get("style_kv"),
            )
            target_style = merge_style(
                base_target_style,
                add_tokens=scenario.get("target", {}).get("style_tokens_add"),
                set_kv=scenario.get("target", {}).get("style_kv"),
            )
            source.set("style", source_style)
            target.set("style", target_style)

            filename = f"geometry_{args.edge_id}__{scenario['name']}.drawio"
            out_path = args.out_dir / "drawio" / filename
            tree.write(out_path, encoding="utf-8", xml_declaration=False)
            writer.writerow(
                [
                    str(out_path),
                    scenario["name"],
                    json.dumps(scenario.get("source", {}), sort_keys=True),
                    json.dumps(scenario.get("target", {}), sort_keys=True),
                ]
            )

    print(f"wrote {len(scenarios)} variants to {args.out_dir}")
    print(f"index: {csv_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
