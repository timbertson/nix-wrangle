with import <nixpkgs> {};
with callPackage ../../nix {};
# TODO: this could definitely be more elegant...
(callPackage
	(
		wrangle.importFrom { basePath = ./.; }
	).nix-source-automation.nix {}
).wrangle.pkgs { basePath = ./.; }
