{ nix, runCommand, lib }:

# Takes a nix derivation path (must be in the store), and
# converts it to a derivation expression.
# Note that the result is not a `stdenv.mkDerivation` with
# all the helpers that entails (like overrideAttrs), but
# is a low-level call to `derivation`
#
# TODO: this likely doesn't suppot multi-output drvs, we'd
# need more smarts around `outputs`.
drvPath:
	with builtins; with lib;
	let
		# TODO: this introduces import-from-derivation, is there some
		# way to shift this to eval time?
		jsonFile =
			assert (isStorePath drvPath);
			runCommand "drv.json" {} ''
			${nix}/bin/nix show-derivation ${toString drvPath} > "$out"
		'';

		drvJson = importJSON jsonFile;

		# The JSON has a single toplevel key of the .drv path
		rawDrv = getAttr (toString drvPath) drvJson;

		outputs = attrNames rawDrv.outputs;

		filteredEnv = filterAttrs (k: v:
			!(elem k outputs)
		) rawDrv.env;

		# We need to produce a derivation with the same inputSrcs and inputDrvs,
		# which we don't get just by copying the attributes.
		# We know there's always a `builder` attribute, so we manually build
		# a string with all the original context of the derivation:
		builderWithCtx = with lib;
		let
			getAllAttrs = src: attrs: map (name: getAttr name src) attrs;
			importInputs = attrs:
				concatLists (
					mapAttrsToList
						# each attr is an attrset with key = path-to-drv and value = list of outputs (attributes)
						(name: outputs: getAllAttrs (import name) outputs)
						attrs
				);
			addContextFrom = orig: dest:
				# warn ("Adding context from: ${orig}")
				(lib.addContextFrom orig dest);
		in
		foldr addContextFrom filteredEnv.builder ((importInputs rawDrv.inputDrvs) ++ (map storePath rawDrv.inputSrcs));

		drvAttrs = filteredEnv // {
			inherit outputs;
			inherit (rawDrv) args;
			builder = builderWithCtx;
		};
	in
	derivation drvAttrs
