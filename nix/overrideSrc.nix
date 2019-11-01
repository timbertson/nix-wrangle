{ lib }:
{ src, drv, version ? null, warn ? true }:
let
	override = attrs: if attrs ? overrideAttrs then attrs.overrideAttrs else lib.overrideDerivation attrs;
	overrides = {inherit src; } // (if version == null then {} else { inherit version; });
in
	if lib.isDerivation drv
		then override drv (_: overrides)
		else (
			if warn then lib.warn "overrideSrc: ${builtins.typeOf drv} is not a derivation, ignoring src ${builtins.toString src}" drv
			else drv
		)
