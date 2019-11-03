{ lib }:
{ src, drv, version ? null, warn ? true }:
let
	optionalVersion = if version == null then {} else { inherit version; };
	override = if drv ? overrideAttrs
		then drv.overrideAttrs
		else lib.overrideDerivation drv;
in
	if lib.isDerivation drv
		then override (super: { inherit src; } // optionalVersion)
		else (
			if warn then lib.warn "overrideSrc: ${builtins.typeOf drv} is not a derivation, ignoring src ${builtins.toString src}" drv
			else drv
		)
