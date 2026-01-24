#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import os
import subprocess
from pathlib import Path


def run(cmd: list[str]) -> None:
    subprocess.run(cmd, check=True)


def read_geometry_variants(csv_path: Path) -> list[Path]:
    with csv_path.open("r", newline="") as csv_file:
        reader = csv.DictReader(csv_file)
        return [Path(row["file"]) for row in reader]


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Run geometry + edge sweep pipeline and generate path reports."
    )
    parser.add_argument("base_drawio", type=Path, help="Base .drawio file")
    parser.add_argument("--edge-id", required=True, help="Edge cell id to sweep")
    parser.add_argument(
        "--attributes-json",
        type=Path,
        required=True,
        help="edge_attributes.json",
    )
    parser.add_argument(
        "--inventory-inputs",
        type=Path,
        nargs="*",
        help="Drawio files or directories to build inventory when attributes json is missing",
    )
    parser.add_argument(
        "--force-inventory",
        action="store_true",
        help="Rebuild inventory even if attributes json exists",
    )
    parser.add_argument(
        "--out-dir",
        type=Path,
        default=Path("tmp/edge-style-sweep/pipeline"),
        help="Output directory for the pipeline run",
    )
    parser.add_argument(
        "--drawio-bin",
        type=Path,
        default=Path("bin/drawio"),
        help="Drawio CLI binary",
    )
    args = parser.parse_args()

    out_dir = args.out_dir
    geometry_dir = out_dir / "geometry"
    os.makedirs(out_dir, exist_ok=True)

    attributes_json = args.attributes_json
    if args.force_inventory or not attributes_json.exists():
        inventory_dir = out_dir / "inventory"
        os.makedirs(inventory_dir, exist_ok=True)
        inventory_inputs = args.inventory_inputs or [args.base_drawio]
        run(
            [
                "python3",
                "bin/drawio_attr_inventory.py",
                *[str(path) for path in inventory_inputs],
                "--out-dir",
                str(inventory_dir),
            ]
        )
        fallback = inventory_dir / "edge_attributes.json"
        if not fallback.exists():
            raise SystemExit(f"attributes json not found: {attributes_json}")
        attributes_json = fallback

    run(
        [
            "python3",
            "bin/edge_geometry_sweep.py",
            str(args.base_drawio),
            "--edge-id",
            args.edge_id,
            "--out-dir",
            str(geometry_dir),
        ]
    )

    geometry_csv = geometry_dir / "geometry_variants.csv"
    geometry_files = read_geometry_variants(geometry_csv)

    for geom_path in geometry_files:
        geom_name = geom_path.stem
        sweep_dir = out_dir / f"sweep_{geom_name}"
        run(
            [
                "python3",
                "bin/edge_style_sweep.py",
                str(geom_path),
                "--cell-id",
                args.edge_id,
                "--attributes-json",
                str(attributes_json),
                "--skip-attr",
                "edge",
                "--skip-attr",
                "vertex",
                "--out-dir",
                str(sweep_dir),
            ]
        )
        svg_dir = sweep_dir / "svg"
        drawio_dir = sweep_dir / "drawio"
        print(f"svg_dir={svg_dir}")
        print(f"sweep_dir={sweep_dir}")
        os.makedirs(svg_dir, exist_ok=True)
        run(
            [
                str(args.drawio_bin),
                "--format",
                "svg",
                "--recursive",
                "--export",
                "-o",
                str(svg_dir),
                str(sweep_dir / "drawio"),
            ]
        )
        path_dir = sweep_dir / "paths"
        run(
            [
                "python3",
                "bin/edge_style_svg_paths.py",
                str(sweep_dir / "variants.csv"),
                "--svg-dir",
                str(svg_dir),
                "--out-dir",
                str(path_dir),
            ]
        )
        if list(path_dir.glob("variants_path_*.csv")):
            run(
                [
                    "python3",
                    "bin/edge_style_path_report.py",
                    "--dir",
                    str(path_dir),
                ]
            )
        else:
            print(f"no variants_path_*.csv files found in {path_dir}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
