{ stdenv }:
path:
	stdenv.mkDerivation {
		name = "unpack-1";
		buildCommand = ''
			srctmp="$out/__nix_unpack_src"
			mkdir -p "$srctmp"
			cd "$srctmp"
			echo "Unpacking source archive: ${path}"
			runOneHook unpackCmd "${path}"
			numdirs="$(ls -1 | wc -l)"
			[ "$numdirs" -eq 1 ] || (echo "expected exactly one directory" >&2; exit 1)
			chmod -R u+w -- "$out"
			ls -1 | while read root; do
				ls -1 "$root" | while read f; do
					mv "$root/$f" "$out/"
				done
			done
			cd "$out"
			rm -r "$srctmp"
		'';
		allowSubstitutes = false;
	}
