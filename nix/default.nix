{ stdenv, lib, git, callPackage, makeWrapper, fetchFromGitHub, haskellPackages }:
# ./wrangle.nix is the vanilla cabal2nix output, so we wrap it here:
(haskellPackages.callPackage ./wrangle.nix {}).overrideAttrs (o: rec {
	src = abort "`src` not overridden";
	nativeBuildInputs = (o.nativeBuldInputs or []) ++ [makeWrapper];
	installPhase = o.installPhase + ''
		cp -r "$src/nix" "$out/share/nix"
		wrapProgram $out/bin/nix-wrangle \
			--prefix PATH : ${git}/bin \
			--set NIX_WRANGLE_DATADIR "$out/share"
	'';
	passthru = (o.passthru or {}) // {
		api = args: callPackage (../nix + "/api.nix") args;
	};
})
