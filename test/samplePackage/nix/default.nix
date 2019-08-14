{ stdenv }: stdenv.mkDerivation {
	name = "samplePackage";
	src = ../upstream-src;
	buildCommand = "cat $src > $out";
}
