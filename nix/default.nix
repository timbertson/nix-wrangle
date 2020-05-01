{ stdenv, lib, git, callPackage, makeWrapper, fetchFromGitHub, haskellPackages, enableSplice }:
# ./wrangle.nix is the vanilla cabal2nix output, so we wrap it here:
let
	splice =
		let ret = if enableSplice then {
			flag = "+splice";
			filterDeps = x: true;
		} else {
			flag = "-splice";
			filterDeps = drv: if drv == null then false else
				builtins.trace "filtercheck: ${if drv == null then "NULL" else drv.pname}" (drv.pname != "hnix"); # could exclude more, but hnix is the big one
		}; in
		builtins.trace "splice flag: ${ret.flag}" ret;

		self = (haskellPackages.callPackage ./wrangle.nix {}).overrideAttrs (o: rec {
		buildInputs = lib.filter splice.filterDeps (o.buildInputs or []);
		nativeBuildInputs = (lib.filter splice.filterDeps (o.nativeBuldInputs or [])) ++ [makeWrapper];
		configureFlags = (o.configureFlags or []) ++ ["--flags=${splice.flag}"];
		installPhase = o.installPhase + ''
			mkdir -p "$out/share"
			cp -r "$src/nix" "$out/share/nix"
			wrapProgram $out/bin/nix-wrangle \
				--prefix PATH : ${git}/bin \
				--set NIX_WRANGLE_DATA "$out/share"
			$out/bin/nix-wrangle installcheck
		'';
		passthru = (o.passthru or {}) // {
			api = args: callPackage (../nix + "/api.nix") args;
		};
	});

in self
