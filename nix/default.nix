{ stdenv, lib, git, callPackage, makeWrapper, fetchFromGitHub, haskellPackages }:
# ./wrangle.nix is the vanilla cabal2nix output, so we wrap it here:
let self = (haskellPackages.callPackage ./wrangle.nix {}).overrideAttrs (o: rec {
	src = ../.;
	nativeBuildInputs = (o.nativeBuldInputs or []) ++ [makeWrapper];
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
}); in
self
