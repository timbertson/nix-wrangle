{ lib, fetchFromGitHub, buildPythonPackage, pygments, nose, nix-update-source }:
buildPythonPackage rec {
  version = "0.9.2";
  src = fetchFromGitHub {
    owner = "timbertson";
    repo = "piep";
    rev = "version-0.9.2";
    sha256 = "1q7lzi3lggw8by4pkh5ckkjv68xqbjahrkxgdw89jxdlyd0wq5in";
  };
  name = "piep-${version}";
  propagatedBuildInputs = [ pygments ];
  checkInputs = [ nose ];
}
