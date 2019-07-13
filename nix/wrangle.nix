{ mkDerivation, aeson, aeson-pretty, base, bytestring, data-fix
, deepseq, directory, exceptions, filepath, hashable, hnix
, megaparsec, mtl, optparse-applicative, prettyprinter, process
, regex-tdfa, stdenv, text, unordered-containers
}:
mkDerivation {
  pname = "nix-wrangle";
  version = "0.0.0";
  src = ./..;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring data-fix deepseq directory
    exceptions filepath hashable hnix megaparsec mtl
    optparse-applicative prettyprinter process regex-tdfa text
    unordered-containers
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
