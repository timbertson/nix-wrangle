{ pkgs, lib, stdenv, fetchFromGitHub, fetchurl, fetchgit }:
with lib;
let
	_nixpkgs = pkgs;
	utils = rec {
		# sub-tools implemented in their own nix files
		importDrv = _nixpkgs.callPackage ./importDrv.nix {};
		exportLocalGit = _nixpkgs.callPackage ./exportLocalGit.nix { };
		overrideSrc = _nixpkgs.callPackage ./overrideSrc.nix { inherit importDrv; };
	};

	augmentGitLocalArgs = { path, commit ? null, ref ? null }@args:
		if commit == null && ref == null then (args // { workingChanges = true; }) else args;

	isPath = p: builtins.typeOf p == "path";

	# exposed for testing
	internal = with api; rec {
		makeFetchers = { path }:
			let withRelativePath = fn: args:
				# inject support for `relativePath` as long as
				# we were invoked with a base path
				let
					fromStore = isStorePath path;
					basePath = if fromStore then path else builtins.toString path;
					joinPath = relativePath:
						if path == null
							then abort "relativePath only supported when using `inject` with a path"
							else "${basePath}/${relativePath}";
				in
				if args ? relativePath then (
					# if we're fetching anything from a path which is already in the store,
					# short-circuit and just use that path
					let fullPath = joinPath args.relativePath; in
					if fromStore
						then fullPath
						else fn ({ path = fullPath; } // (filterAttrs (n: v: n != "relativePath") args))
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

		importScope = pkgs: attrs:
			lib.makeScope pkgs.newScope (self: mapAttrs (name: node: node.drv) attrs);

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
					if src == null then nixAttr # used in tests only
					else "${src}/${nixAttr}"
				);
				version = attrs.version or (fetchArgs.ref or null);

				defaultCall = { pkgs, path }: pkgs.callPackage path {};
				callImpl = attrs.call or defaultCall;

				callWith = args:
					# If attrs.nix == null, we return the source instead of a derivation
					if nix == null
						then builtins.trace "[wrangle] Providing ${name} (source-only) from ${src}" src
						else
							let drv = callImpl args; src = drv.src or null; in
							if isPath src && isStorePath src
								then drv
								else builtins.trace "[wrangle] Importing ${name} from ${nix}" (
									overrideSrc { inherit src version drv; }
								);
				drv = callWith { inherit pkgs; path = nix; };
			in
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
				else builtins.trace "[wrangle] Loading ${path}" (importJSON path);
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
				jsonSources // (extend jsonSources)
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
				base = mergedPkgs.callPackage nixPath _args;
				selfSrc = imports.sources.self or null;
			in
			(if selfSrc == null then base else
				builtins.trace "[wrangle] injecting src from `self` dependency" (
				overrideSrc {
					inherit (selfSrc) src version;
					drv = base;
				})
			);
	});
in api
