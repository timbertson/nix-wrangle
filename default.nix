let
  pkgs = import <nixpkgs> { };
  haskellPackages = pkgs.haskellPackages;
in
(haskellPackages.callPackage (
{ mkDerivation, base, lens, stdenv, cabal-install }:
mkDerivation rec {
  pname = "haskell-nix-demo";
  version = "0.1.0.0";
  # src = ./.;
  src = null;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ ];
  executableHaskellDepends = [ ];
  license = stdenv.lib.licenses.mit;
}) {}).overrideAttrs (o: rec {
  buildInputs = [ haskellPackages.cabal-install pkgs.zlib haskellPackages.ghcid ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH
    # needed on OSX, where `cc` is a wrapper and can't otherwise find its own libc++.so
    export LIBRARY_PATH=${pkgs.libcxx}/lib:$LIBRARY_PATH
    export LANG=en_US.UTF-8
  '';
})
