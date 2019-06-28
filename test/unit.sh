#!/usr/bin/env bash
set -eux
nix-instantiate --show-trace --eval "$(dirname "$0")/unit.nix" --strict
