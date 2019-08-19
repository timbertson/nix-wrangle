{lib, stdenv}:
stdenv.mkDerivation {
	name="dep";
	passthru.depProvided = true;
}
