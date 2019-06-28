{ stdenv, callPackage, fetchFromGitHub }:
stdenv.mkDerivation rec {
	src = fetchFromGitHub {
		owner = "timbertson";
		repo = "nix-source-automation";
		rev = "33d172284b34e904d697145b167d85c5be3a7cee";
		sha256 = "0my65kh7r3h19lkj1yjqm6xfyxxr9f8pdka6wzfwh773zbgdlqkd";
	};
	name="nix-source-automation";
	passthru = {
		api = args: callPackage "${src}/nix/api.nix" args;
	};
	buildCommand = "touch $out";
}
