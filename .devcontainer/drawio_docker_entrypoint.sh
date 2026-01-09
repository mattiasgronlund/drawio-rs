#!/usr/bin/bash

# abort script if a command fails
set -e

export DISPLAY="${XVFB_DISPLAY:?}"
# shellcheck disable=SC2086
# shellcheck disable=SC2154
Xvfb "${XVFB_DISPLAY:?}" ${XVFB_OPTIONS} &

# prepend antora if command is not detected
if [ $# -eq 0 ] || [ "${1:0:1}" == '-' ] || [ -z "$(command -v "$1")" ] || [ -d "$(command -v "$1")" ] || [ ! -x "$(command -v "$1")" ]; then
  set -- drawio "$@"  --no-sandbox
  "$@" 2> >(grep -v -E '\[[0123456789]+:[0123456789]+/[0123456789]+\.[0123456789]+:(ERROR|WARNING):' >&2) | grep -F -v -e 'Found package-type: deb' -e 'Checking for beta autoupdate feature for deb/rpm distributions'
  exit $?
fi

exec "$@"