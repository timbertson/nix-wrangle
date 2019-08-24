set -eu
gup -u nix/default.nix
export PS4="\n$ "
exec > >(sed -E -e '/_REM|echo/d' | cat -s | tee "$1")
exec 2>&1

function comment_REM {
	echo -e -n '#' "$@"
}
