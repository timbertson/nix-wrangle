{ stdenv, piep }:
stdenv.mkDerivation {
  name="sample.txt";
  src = ../.;
  buildCommand = ''
cat > "$out" <<EOF
Sample derivation, built with:
 - piep: ${piep}
 - src: $src
EOF
  '';
}
