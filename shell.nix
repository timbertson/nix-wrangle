{ pkgs ? import <nixpkgs> {}}:
(pkgs.callPackage ./default.nix {}).env.overrideAttrs (o: {
	nativeBuildInputs = o.nativeBuildInputs ++ [
		pkgs.haskellPackages.cabal-install
		pkgs.haskellPackages.ghcid
		pkgs.cabal2nix
	];
})
