{ pkgs, git }:
# using pkgs.stdenv to get around the fact that
# an unbuildable `stdenv` is injected
pkgs.stdenv.mkDerivation {
	name="upstreamGit";
	passthru.git = git;
}
