{
	sources = [{
		wrangle.apiversion = 1;
		sources.version = import ./versionSrc.nix;
	}];
	nix = ({ pkgs, version, custom }:
		(pkgs.callPackage ./default.nix {}).overrideAttrs (o: { passthru = { inherit custom; }; }));
	args = { custom = "attr!"; };
}
