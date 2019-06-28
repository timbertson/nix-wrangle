#!/usr/bin/env bash
set -eu
set -o pipefail
here="$(dirname "$0")"
set -x
drv="$(nix-instantiate "$here/test.nix" -A gup --quiet)"
outputDrv="$(nix-instantiate "$here/test.nix" -A importDrv --argstr drvPath "$drv" --quiet)"
if [ "$drv" != "$outputDrv" ]; then
	echo "Error! files differ"
	set -x
	diff <(nix show-derivation "$drv") <(nix show-derivation "$outputDrv")
else
	echo "OK"
fi
