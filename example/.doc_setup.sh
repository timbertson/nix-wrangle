set -eu
gup -u nix/default.nix
export PS4="\n$ "
exec > >(sed -E -e '/_REM/d' | cat -s | tee "$1")
exec 2>&1

function nix-build {
	# The first (quiet) build is to ensure we're not showing any build output
	{ env nix-build "$@" 2>&1; } > /dev/null 2>&1
	{ env nix-build "$@" 2>&1; } 2>/dev/null
}

function nix-wrangle {
	# Make sure we always use the latest built version
	{ env ../bin/nix-wrangle "$@"; } 2>/dev/null
}

function comment_REM {
	{ echo -e -n '#' "$@"; } 2>/dev/null
}
