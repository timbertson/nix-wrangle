#!/usr/bin/env bash
set -eu
set -o pipefail
here="$(dirname "$0")"
# ensure default-nix is updated
cd "$here"
../bin/nix-wrangle default-nix
set -x

function expected_output {
	cat <<EOF
derivations
exportLocalGit
importDrv
importFrom
importJsonSrc
inject
internal
nixImpure
overlays
overlaysOfImport
override
overrideDerivation
overrideSrc
pkgs
pkgsOfImport
unpackArchive
EOF
}

function actual_output {
	nix-instantiate --read-write-mode --show-trace --eval -A apiMembers "default.nix" "$@" | tr ',' '\n' | tr -d '"'
}
function actual_output_github {
	actual_output
}

function actual_output_local {
	actual_output --arg 'nix-wrangle' '(import <nixpkgs> {}).callPackage ../nix {}'
}

diff <(expected_output) <(actual_output_local)
# diff <(expected_output) <(actual_output_github)
