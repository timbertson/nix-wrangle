{ lib, importDrv ? (assert "importDrv required to override .drv files"; null) }:
{ src, drv, version ? null }:
let
	drvAttrs = if lib.isAttrs drv then drv else (importDrv drv); # if not an attrset, assume a .drv path
	override = attrs: if attrs ? overrideAttrs then attrs.overrideAttrs else lib.overrideDerivation attrs;
	overrides = {inherit src; } // (if version == null then {} else { inherit version; });
in
	if lib.isDerivation drvAttrs
		then override drvAttrs (_: overrides)
		else lib.warn "overrideSrc: ${builtins.typeOf drv} is not a derivation, ignoring src ${builtins.toString src}" drv
