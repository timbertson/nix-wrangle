#!/usr/bin/env bash
set -ux
here="$(dirname "$0")"
st=0
function run() {
	if ! "$here/$1"; then
		echo -e "*** FAILED: $1 ***\n"
		st=1
	else
		echo -e "Ok: $1\n"
	fi
}
run "unit.sh"
run "import-drv-check-identity.sh"
run "nix-impure.sh"
run "override-src.sh"
run "integration.sh"
exit "$st"
