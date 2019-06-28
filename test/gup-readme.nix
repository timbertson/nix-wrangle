{ stdenv, fetchFromGitHub, nix-update-source, lib, python, which, pychecker ? null }:
stdenv.mkDerivation rec {
  version = "TODO";
  name="gup-${version}";
  src = fetchFromGitHub rec {
    owner = "timbertson";
    repo = "gup";
    rev = "version-${meta.version}";
    sha256 = "1pwnmlq2pgkkln9sgz4wlb9dqlqw83bkf105qljnlvggc21zm3pv";
    meta = {
      version = "0.7.0";
      update = { current, version }: {
        rev = "version-${version}";
      };
      updateManual = { current, version }: {
        rev = fetchFromGitHub.impure (current // { sha256 = null; });
      };
    };
  };
  buildCommand = "cat $src/README.md > $out";
}
