#!/usr/bin/env bash
set -euo pipefail

if [[ "$#" -eq 0 ]]; then
  echo "Usage: $(basename "$0") <svg files...>" >&2
  exit 1
fi

python3 - <<'PY' "$@"
import re
import sys
from pathlib import Path

image_pattern = re.compile(
    r'<image\b[^>]*xlink:href="data:image/png[^\"]*"[^>]*/>'
)
switch_pattern = re.compile(r'<switch\b[^>]*>.*?</switch>', re.DOTALL)


def strip_switch_images(match: re.Match[str]) -> str:
    block = match.group(0)
    if "<foreignObject" not in block:
        return block
    return image_pattern.sub("", block)


for filename in sys.argv[1:]:
    path = Path(filename)
    content = path.read_text(encoding="utf-8")
    updated = switch_pattern.sub(strip_switch_images, content)
    if updated != content:
        path.write_text(updated, encoding="utf-8")
PY
