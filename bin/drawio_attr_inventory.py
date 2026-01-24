#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any
from xml.etree import ElementTree as ET


def parse_style(style: str) -> dict[str, str]:
    parts = [p for p in style.split(";") if p]
    out: dict[str, str] = {}
    for part in parts:
        if "=" in part:
            key, value = part.split("=", 1)
            out[key] = value
    return out


def classify_values(values: set[str]) -> dict[str, Any]:
    def is_int(v: str) -> bool:
        try:
            int(v)
            return True
        except ValueError:
            return False

    def is_float(v: str) -> bool:
        try:
            float(v)
            return True
        except ValueError:
            return False

    if values and all(v in ("0", "1", "true", "false") for v in values):
        return {"type": "bool", "values": sorted(values)}
    if values and all(is_int(v) for v in values):
        return {"type": "int", "values": sorted(values)}
    if values and all(is_float(v) for v in values):
        return {"type": "float", "values": sorted(values)}
    return {"type": "string", "values": sorted(values)}


def update_inventory(inv: dict[str, dict[str, Any]], key: str, value: str) -> None:
    entry = inv.setdefault(key, {"values": set()})
    entry["values"].add(value)


def finalize_inventory(inv: dict[str, dict[str, Any]]) -> dict[str, dict[str, Any]]:
    out: dict[str, dict[str, Any]] = {}
    for key, data in sorted(inv.items()):
        values = data.get("values", set())
        out[key] = classify_values(values)
    return out


def iter_drawio_files(paths: list[Path]) -> list[Path]:
    files: list[Path] = []
    for path in paths:
        if path.is_dir():
            files.extend(sorted(path.rglob("*.drawio")))
        else:
            files.append(path)
    return files


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Inventory drawio edge/vertex attributes for sweep planning."
    )
    parser.add_argument(
        "inputs",
        nargs="+",
        type=Path,
        help="Drawio files or directories to scan",
    )
    parser.add_argument(
        "--out-dir",
        type=Path,
        default=Path("tmp/edge-style-sweep"),
        help="Output directory for JSON reports",
    )
    args = parser.parse_args()

    edge_inv: dict[str, dict[str, Any]] = {}
    vertex_inv: dict[str, dict[str, Any]] = {}
    edge_style_inv: dict[str, dict[str, Any]] = {}
    vertex_style_inv: dict[str, dict[str, Any]] = {}

    for path in iter_drawio_files(args.inputs):
        try:
            tree = ET.parse(path)
        except ET.ParseError:
            continue
        for cell in tree.getroot().iter("mxCell"):
            is_edge = cell.get("edge") == "1"
            is_vertex = cell.get("vertex") == "1"
            if not is_edge and not is_vertex:
                continue
            inv = edge_inv if is_edge else vertex_inv
            style_inv = edge_style_inv if is_edge else vertex_style_inv
            for key, value in cell.attrib.items():
                if key in ["id", "parent", "source", "value", "visible", "style", "target"]:
                    continue
                update_inventory(inv, key, value)
            style = cell.get("style", "")
            for key, value in parse_style(style).items():
                if key in ["fillColor", "fontColor", "fontStyle", "gradientColor", "gradientDirection", "html", "image", "labelBackgroundColor", "points", "strokeColor", "whiteSpace"]:
                    continue
                if key in ["newEdgeStyle"]: # TODO, how to handle this...
                    continue
                update_inventory(style_inv, key, value)

    args.out_dir.mkdir(parents=True, exist_ok=True)
    edge_out = {
        "attributes": finalize_inventory(edge_inv),
        "style": finalize_inventory(edge_style_inv),
    }
    vertex_out = {
        "attributes": finalize_inventory(vertex_inv),
        "style": finalize_inventory(vertex_style_inv),
    }
    edge_path = args.out_dir / "edge_attributes.json"
    vertex_path = args.out_dir / "vertex_attributes.json"
    edge_path.write_text(json.dumps(edge_out, indent=2, sort_keys=True))
    vertex_path.write_text(json.dumps(vertex_out, indent=2, sort_keys=True))
    print(f"wrote {edge_path}")
    print(f"wrote {vertex_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
