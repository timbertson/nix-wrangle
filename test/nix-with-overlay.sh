#!/usr/bin/env bash
set -eux
set -o pipefail
here="$(dirname "$0")"
drv="$("$here/../bin/nix-with-overlay" --add "$here/overlay.nix" nix-build --show-trace '<nixpkgs>' -A gup)"
grep 'Gup is a general purpose, recursive, top down software build system.' "$drv"
