{ bash, lib, stdenv, git }:
{ path, commit ? null, ref ? null, workingChanges ? false }:
	let
		pathStr = toString path; in
	let
		path = pathStr; # shadow, so we can't accidentally import `path`
		resolveRef = ref: let
			candidates = [
				"refs/heads/"
				"refs/tags/"
				"refs/remotes/"
				""
			];
			prefix = "${path}/.git";
			fullPath = scope: "${prefix}/${scope}${ref}";
			fullCandidates = map fullPath candidates;
			refPath =
				let found = lib.findFirst builtins.pathExists null fullCandidates; in
				if found == null
					then abort "No git ref candidates found in ${builtins.toJSON fullCandidates}"
					else found;
			indirectPrefix = "ref: ";
			resolve = refPath:
				let result = lib.removeSuffix "\n" (builtins.readFile refPath); in
				builtins.trace "Dereferenced git ref ${refPath} to ${result}" (
					if (lib.hasPrefix indirectPrefix result)
						then resolve ("${prefix}/${lib.removePrefix indirectPrefix result}")
						else result
				);
		in resolve refPath;

		commitRequired = abort "exactly one of commit, ref or workingChanges required";

		commitDrv = builtins.fetchGit { url = path; rev = commit; };
		refDrv = builtins.fetchGit { url = path; rev = (resolveRef ref); inherit ref; };
		workspaceDrv = builtins.fetchGit { url = path; };
	in
	(lib.findFirst (x: x.condition == true) { drv = commitRequired; } [
		{ condition = commit != null && ref == null && !workingChanges; drv = commitDrv; }
		{ condition = commit == null && ref != null && !workingChanges; drv = refDrv; }
		{ condition = commit == null && ref == null && workingChanges;  drv = workspaceDrv; }
	]).drv
