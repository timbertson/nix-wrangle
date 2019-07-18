{ lib, nix-wrangle }:
{
	apiMembers = lib.concatStringsSep "," (lib.sort (a: b: a < b) (lib.attrNames (nix-wrangle.api {})));
}
