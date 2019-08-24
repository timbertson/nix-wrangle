{ stdenv, piep }:
stdenv.mkDerivation {
  name="sample.txt";
  src = null;
  buildCommand = ''
    cat > "$out" <<EOF
      Sample derivation, built from:
       - self: $src
       - piep: ${piep}
    EOF
  '';
}
