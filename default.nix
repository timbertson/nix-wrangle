# Stripped down version of default-nix which doesn't dynamically fetch nix-wrangle
let
  systemNixpkgs = import <nixpkgs> {};
  fallback = val: dfl: if val == null then dfl else val;
in
{ pkgs ? null }@provided:
let
  _pkgs = fallback pkgs systemNixpkgs;
  _wrangle = _pkgs.callPackage ./nix {};
in
(_wrangle.api { pkgs = _pkgs; }).inject { inherit provided; path = ./.; }
