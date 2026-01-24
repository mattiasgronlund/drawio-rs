#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
from collections import defaultdict
from pathlib import Path


BASE_COLUMNS = {"file", "key", "base", "value", "edge_id", "cell_id", "cell_kind"}


def load_rows(path: Path) -> list[dict[str, str]]:
    with path.open("r", newline="") as csv_file:
        reader = csv.DictReader(csv_file)
        return list(reader)


def path_columns(rows: list[dict[str, str]]) -> list[str]:
    if not rows:
        return []
    return [k for k in rows[0].keys() if k not in BASE_COLUMNS]


def varying_columns(rows: list[dict[str, str]], columns: list[str]) -> list[str]:
    varying: list[str] = []
    for col in columns:
        values = {row.get(col, "") for row in rows}
        if len(values) > 1:
            varying.append(col)
    return varying


def per_key_variations(rows: list[dict[str, str]], columns: list[str]) -> dict[str, list[str]]:
    by_key: dict[str, list[dict[str, str]]] = defaultdict(list)
    for row in rows:
        by_key[row.get("key", "")].append(row)
    summary: dict[str, list[str]] = {}
    for key, key_rows in by_key.items():
        varying: list[str] = []
        for col in columns:
            values = {row.get(col, "") for row in key_rows}
            if len(values) > 1:
                varying.append(col)
        summary[key] = varying
    return summary


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Report path attribute changes per sweep key from variants_path CSV."
    )
    parser.add_argument(
        "csv_paths",
        type=Path,
        nargs="*",
        help="variants_path_<n>.csv files (defaults to tmp/edge-style-sweep/variants_path_*.csv)",
    )
    parser.add_argument(
        "--dir",
        type=Path,
        help="Directory to search for variants_path_*.csv files",
    )
    args = parser.parse_args()

    paths = args.csv_paths
    if not paths:
        search_dir = args.dir or Path("tmp/edge-style-sweep")
        paths = sorted(search_dir.glob("variants_path_*.csv"))
    if not paths:
        raise SystemExit("no variants_path_*.csv files found")

    for path in paths:
        rows = load_rows(path)
        if not rows:
            print(f"{path.name}\n(no rows found)\n")
            continue

        columns = path_columns(rows)
        overall = varying_columns(rows, columns)
        per_key = per_key_variations(rows, columns)

        print(path.name)
        print("varying path attributes: " + (", ".join(overall) if overall else "(none)"))
        print()
        for key in sorted(per_key):
            attrs = per_key[key]
            label = ", ".join(attrs) if attrs else "(no path attr changes)"
            print(f"{key}: {label}")
        print()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
