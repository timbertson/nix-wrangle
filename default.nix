let
  pkgs = import <nixpkgs> { };
  haskellPackages = pkgs.haskellPackages;
in
(haskellPackages.callPackage ./nix/wrangle.nix {}).overrideAttrs (o: {
  installPhase = o.installPhase + ''
    makeWrapper $out/bin/nix-wrangle --prefix PATH : ${pkgs.git}/bin
  '';
})
      
