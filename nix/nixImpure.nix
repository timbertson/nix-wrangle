{ stdenv, nix }:

attrs: script:
let
	defaults = {
		name = "nix-impure-script";
	};
	overrides = {
		buildCommand = ''
			echo "This derivation cannot be built, it is intended for nix-impure"
			exit 1
		'';
		shellHook = ''
			_outbase="$(mktemp -d)"
			out="$_outbase/$name"
			trap 'rm -rf "$_outbase"' EXIT
			exec 3>&1
			exec 1>&2
			${script} >&2
			${nix}/bin/nix-store --add "$out" >&3
		'';
	};
in
stdenv.mkDerivation (defaults // attrs // overrides)
