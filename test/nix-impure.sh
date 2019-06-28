#!/usr/bin/env bash
set -eux
set -o pipefail
here="$(dirname "$0")"
drv="$(nix-instantiate "$here/test.nix" --show-trace -A impure --quiet)"
"$here/../bin/nix-impure" "$drv"

