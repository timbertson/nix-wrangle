{ pkgs, lib, stdenv, fetchFromGitHub, fetchurl, fetchgit }:
with lib;
let
	_nixpkgs = pkgs;
	infoLn = msg: ret: builtins.trace ("[wrangle] " + msg) ret;
	debugLn = if builtins.getEnv "WRANGLE_DEBUG" == "true" then infoLn else (msg: ret: ret);
	utils = rec {
		# sub-tools implemented in their own nix files
		exportLocalGit = _nixpkgs.callPackage ./exportLocalGit.nix { };
		overrideSrc = _nixpkgs.callPackage ./overrideSrc.nix { };
	};

	ensureFunction = nixPath: if isFunction nixPath
		then nixPath
		else (
			let imported = import nixPath; in
			if isFunction imported
				then imported
				else abort "Imported expression ${nixPath} is not a function"
		);

	augmentGitLocalArgs = { path, commit ? null, ref ? null }@args:
		if commit == null && ref == null then (args // { workingChanges = true; }) else args;

	# exposed for testing
	internal = with api; rec {

		expandRelativePath = base: relative:
			if base == null
				then abort "relativePath only supported when using `inject` with a path"
				# toPath normalizes `/some-path/.` into simply `/some-path`
				else with builtins; toPath "${base}/${relative}";

		expandRelativePathWithoutImporting = base: relative:
			with builtins;
			# both toStrings are needed to prevent depending
			# on the base / final paths (which would import
			# them into the store)
			toString (expandRelativePath (toString base) relative);

		makeFetchers = { path }:
			let withRelativePath = fn: args:
				# inject support for `relativePath` as long as
				# we were invoked with a base path
				if args ? relativePath then (
					# if we're fetching anything from a path which is already in the store,
					# short-circuit and just use that path
					let
						fromStore = isStorePath path;
						expandPath = if fromStore
							then expandRelativePath
							else expandRelativePathWithoutImporting;
						fullPath = expandPath path args.relativePath;
					in
					if fromStore
						then debugLn "Imported from store, resolved ${args.relativePath} to ${fullPath}" fullPath
						else debugLn "Calling fetcher with resolved path ${fullPath} " (
							fn ({ path = fullPath; } // (filterAttrs (n: v: n != "relativePath") args))
						)
				) else (
					fn args
				);
			in {
			github = fetchFromGitHub;
			url = fetchurl;
			git = fetchgit;
			git-local = withRelativePath (args: exportLocalGit (augmentGitLocalArgs args));
			path = withRelativePath ({ path }: "${path}");
			_passthru = arg: arg; # used in tests
		};

		importScope = pkgs: attrs: let
			sources = mapAttrs (name: node: node.src) attrs;
			nodes = attrValues attrs;
			derivations = mapAttrs (name: node: node.drv) attrs;
			# This is a bit weird: for each invocation of callPackage, if it's a known
			# wrangle it gets `self` injected as appropriate.
			newScope = scope: let call = pkgs.newScope scope; in
				pkg: args: let
					pkgFn = ensureFunction pkg;
					node = findFirst (node: node.nix == pkg) null nodes;
					selfNeeded = (functionArgs pkgFn) ? "self"  && (! args ? "self");
					overrideSelf = if node == null
					then debugLn
						"Importing a function accepting `self`, but it does not match a known wrangle path"
						args
					else debugLn
						"Injecting node-specific `self`"
						(args // { self = node.src; });
					callArgs = if selfNeeded then overrideSelf else args;
				in call pkgFn callArgs;
		in
		lib.makeScope newScope (self: pkgs // derivations);

		makeImport = { settings, pkgs }: name: attrs:
			let
				fetchers = makeFetchers settings;
				fetcher = attrs.type;
				fetchArgs = attrs.fetch;
				src = if builtins.hasAttr fetcher fetchers
					then (builtins.getAttr fetcher fetchers) fetchArgs
					else abort "Unknown fetcher: ${fetcher}"
				;
				nixAttr = attrs.nix or null;
				nix = if nixAttr == null then null else (
					if isString nixAttr then "${src}/${nixAttr}"
					else nixAttr # used only in tests
				);
				version = attrs.version or (fetchArgs.ref or null);

				defaultCall = { pkgs, path }: pkgs.callPackage path {};
				callImpl = attrs.call or defaultCall;

				callWith = args:
					# If attrs.nix == null, we return the source instead of a derivation
					if nix == null
						then src
						else overrideSrc {
							inherit src version;
							drv = callImpl args;
						};
				nixDesc = if (isString nix || builtins.isPath nix) then nix else src;
				drv = debugLn "calling ${nixDesc}" (callWith { inherit pkgs; path = nix; });
			in
			infoLn "${if nix == null then "Providing source" else "Importing derivation"} ${name} (${fetcher}) from ${nixDesc}"
				{ inherit name attrs src version nix drv; };

		importsOfJson = settings: json:
			# build a package scope with all imported packages present,
			# allowing packages in the set to depend on each other
			let
				imports = (mapAttrs (makeImport {
					inherit settings;
					pkgs = importScope pkgs imports;
				}) json);
			in
			imports;
	};

	api = with internal; with utils; utils // (rec {
		inherit internal;

		importJsonSrc = path:
			let attrs = if isAttrs path
				then path
				else debugLn "Loading ${path}" (importJSON path);
			in
			assert attrs.wrangle.apiversion == 1; attrs;

		importFrom = {
			path ? null,
			sources ? null,
			extend ? null,
		}:
		let
			jsonList = map importJsonSrc (
				if sources != null then sources else (
					if path == null
						then (abort "path or sources required")
						else (
							let
								p = builtins.toString path;
								candidates = [
									"${p}/nix/wrangle.json"
									"${p}/nix/wrangle-local.json"
								];
								present = filter builtins.pathExists candidates;
							in
							if (length present == 0)
								then lib.warn "No files found in candidates:\n - ${concatStringsSep "\n - " candidates}" present
								else present
						)
				)
			);
			# For convenience we drop everything but `sources` at this stage.
			# We could return those at the toplevel, but this lets us add more
			# attributes later if needed.
			jsonSourcesList = map (j: j.sources) jsonList;
			jsonSources = lib.foldl (a: b: a // b) {} jsonSourcesList;
			jsonSourcesExtended = if extend == null then jsonSources else (
				# extend only acts on `sources`, not the full attrset
				recursiveUpdate jsonSources (extend jsonSources)
			);
		in
		{
			# persist sourcesAttrs for testing / debug purposes
			sourceAttrs = jsonSourcesExtended;
			# map `sources` into imports instead of plain attrs
			sources = importsOfJson { inherit path; } jsonSourcesExtended;
		};

		derivations = args: mapAttrs (name: node: node.drv) (importFrom args).sources;

		inject = {
			# inject args
			nix ? null, # optional if `path` given
			provided ? {},

			# callPackage args
			args ? {},

			# importFrom args
			path ? null, # required if `nix` not given
			sources ? null,
			extend ? null,
		}:
			let
				pathStr =
					# if `path` is not already in the store, we coerce it to a string
					# so we don't auto-import it. Requires `--option build-use-chroot false`
					if path == null then path else (if isStorePath path then path else builtins.toString path);
				nixPath = (if nix != null then nix else "${pathStr}/nix");
				imports = importFrom { inherit path sources extend; };

				# pull args out of provided
				_args = if (args == {} && provided ? args) then provided.args else args;
				_provided = filterAttrs (n: v: ! elem n ["args"]) provided;

				# If arguments are explicitly provided, use them in preference to
				# local sources. This is used in recursive wrangle, to
				# override a dependency. Note that `provided` defaults pkgs & nix-wrangle to `null`
				extantProvided = filterAttrs (n: v: v != null) _provided;
				mergedPkgs = (internal.importScope pkgs imports.sources) // extantProvided;
				nixFunction = ensureFunction nixPath;
				base = mergedPkgs.callPackage nixPath _args;
				selfSrc = imports.sources.self or null;
			in
			(if selfSrc == null then base else
				overrideSrc {
					inherit (selfSrc) src version;
					drv = base;
					# Skip warning if function explicitly accepts `self`, it
					# probably knows what it's doing.
					warn = ! ((lib.functionArgs nixFunction) ? "self");
				}
			);
	});
in api
