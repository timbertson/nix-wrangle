#!/usr/bin/env bash
set -eu
set -o pipefail
here="$(dirname "$0")"
set -x
"$here/../bin/nix-callpackage" --merge 'pythonPackages' nix-build --show-trace "$here/piep.nix"
