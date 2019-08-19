{ lib, stdenv, dep }:
stdenv.mkDerivation {
	name="dep-user";
	passthru = {
		inherit (dep) depProvided;
	};
}
