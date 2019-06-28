{ bash, lib, stdenv, git, nixImpure ? (abort "workingChanges not supported without nixImpure") }:

# Impure derivation which returns a .tgz or directory export of a git workspace.
# If `workingChanges` is `true`, returns a nix-impure script which will
# create the appropriate aritfact including changes to tracked files.

{ path, commit ? null, ref ? null, workingChanges ? false, unpack ? true }:
	let
		prefix = "#!${bash}/bin/bash\n" + ''
			export PATH="${git}/bin:$PATH";
		'';
		
		# commit: pure derivation
		exportCommitScript = commit:
			let
				archiveArgs = "git archive \"${commit}\" --format=tar.gz";
				archiveCommand = if unpack then ''
					mkdir -p "$out"
					${archiveArgs} | tar xz -C "$out"
				'' else ''
					${archiveArgs} --prefix="git-export/" > "$out"
				'';
			in
			''
				echo "Exporting git revision ${commit}"
				cd "${toString path}"
				${archiveCommand}
			''
		;
		
		# ref: uses readFile to extract the commit
		exportRefScript = ref:
			let
				candidates = [
					"refs/heads/"
					"refs/tags/"
					"refs/remotes/"
					""
				];
				prefix = "${path}/.git";
				fullPath = scope: "${prefix}/${scope}${ref}";
				refScope = lib.findFirst (candidate: builtins.pathExists (fullPath candidate)) null candidates;
				refPath = assert refScope != null; fullPath refScope;
				indirectPrefix = "ref: ";
				resolve = refPath:
					let result = lib.removeSuffix "\n" (builtins.readFile refPath); in
					builtins.trace "Dereferenced git ref ${refPath} to ${result}" (
						if (lib.hasPrefix indirectPrefix result)
							then resolve ("${prefix}/${lib.removePrefix indirectPrefix result}")
							else result
					);
			in
			exportCommitScript (resolve refPath)
		;

		# workingChanges: produces an impure derivation
		exportWorkingChangesScript = ''
			cd "${toString path}"
			commit="$(env \
				GIT_AUTHOR_NAME="nobody" \
				GIT_AUTHOR_EMAIL="nobody@example.org" \
				GIT_AUTHOR_DATE='1970-01-01T00:00:00Z' \
				GIT_COMMITTER_NAME="nobody" \
				GIT_COMMITTER_EMAIL="nobody@example.org" \
				GIT_COMMITTER_DATE='1970-01-01T00:00:00Z' \
				git stash create || true)"
			if [ -z "$commit" ]; then
				# no working changes
				commit="$(git rev-parse HEAD)"
			fi
			${exportCommitScript "$commit"}
		'';

		name = (if unpack then "git-export" else "git-export.tar.gz");
		commitRequired = assert "exactly one of commit, ref or workingChanges required"; null;

		commitDrv = stdenv.mkDerivation { inherit name; buildCommand = prefix + exportCommitScript commit; };
		refDrv = stdenv.mkDerivation { inherit name; buildCommand = prefix + exportRefScript ref; };
		workspaceDrv = nixImpure { inherit name; } (prefix + exportWorkingChangesScript);
	in
	(lib.findFirst (x: x.condition == true) { drv = commitRequired; } [
		{ condition = commit != null && ref == null && !workingChanges; drv = commitDrv; }
		{ condition = commit == null && ref != null && !workingChanges; drv = refDrv; }
		{ condition = commit == null && ref == null && workingChanges;  drv = workspaceDrv; }
	]).drv
