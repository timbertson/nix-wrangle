{ pkgs ? import <nixpkgs> {}}:
(pkgs.callPackage ./default.nix { args.enableSplice = false; }).env.overrideAttrs (o: {
	nativeBuildInputs = o.nativeBuildInputs ++ [
		pkgs.haskellPackages.cabal-install
		pkgs.haskellPackages.ghcid
		pkgs.cabal2nix
	];
})
