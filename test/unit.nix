with import <nixpkgs> {};
with builtins;
with lib;
let
	api = (callPackage ../nix/api.nix {});
	internal = api.internal;

	wrangleHeader = { apiversion = 1; };
	addHeader = j: j // { wrangle = wrangleHeader; };
	eq = name: a: b:
		[ name (a == b) ("${toJSON a} != ${toJSON b}") ];

	testFailure = test:
		let
			result = if elemAt test 1 then [] else [ "${name}${suffix}" ];
			name = elemAt test 0;
			# third element is an optional message
			suffix = if length test == 2 then "" else ":\n  ${elemAt test 2}";
			testOnly = builtins.getEnv "TEST_ONLY";
			runTest = if testOnly == "" then true else name == testOnly;
			logTest =
				if builtins.getEnv "WRANGLE_DEBUG" == "1" then
				lib.warn "Executing test case: ${elemAt test 0}"
				else result: result;
		in
		if runTest then logTest result else [];

	versionSrc = import samplePackage/versionSrc.nix;

	version = addHeader {
		sources = {
			version = versionSrc;
		};
	};

	versionNoImport = {
		type = "github";
		fetch = {
			"owner" = "timbertson";
			"repo" = "version";
			"rev" = "version-0.13.1";
			"sha256" = "056l8m0xxl1h7x4fd1hf754w8q4sibpqnwgnbk5af5i66399az61";
		};
	};

	fakeNixpkgs = addHeader {
		sources = {
			nixpkgs = {
				type = "path";
				fetch = { path = (toString ./.); };
				nix = "fakeNixpkgs.nix";
			};
		};
	};

	makeImport = internal.makeImport {
		settings = { path = null; };
		inherit pkgs;
	};

	tests = [
		["implPath is path" (isString (makeImport "name" versionSrc).nix)]

		((result: eq "returns source if nix is unset" result.src result.drv)
			(makeImport "name" versionNoImport))

		["nix is modifiable" (hasSuffix "/foo.nix" (makeImport "name" (versionSrc // {nix = "foo.nix";})).nix)]

		["src is derivation" (isDerivation (makeImport "name" versionSrc).src)]

		(eq "passthru name" (makeImport "name" versionSrc).name "name")

		(eq "passthru attrs" (makeImport "name" versionSrc).attrs versionSrc)

		["importScope produces valid derivations"
			(isDerivation (internal.importScope pkgs {
				versionOverride = (makeImport "versionOverride" versionSrc);
			}).versionOverride)]

		(
			let myPkgs = internal.importScope pkgs {
				myVersion = makeImport "myVersion" versionSrc;
			}; in
			eq "importScope provides imports to callPackage invocations"
			(myPkgs.callPackage ({ myVersion, curl, pkgs }: [myVersion.name curl.name myPkgs.curl.name pkgs.curl.name]) {})
			[myPkgs.myVersion.name curl.name curl.name curl.name]
		)

		["makes derivations" (isDerivation (api.derivations { sources = [ version ]; }).version)]

		(eq "provides inputs to be used by other packages" (
			(api.importFrom { sources = [
				(addHeader { sources = {
					dep = {
						type="_passthru";
						fetch = null;
						nix = ./samplePackage/dep.nix;
					};
					dep-user = {
						type="_passthru";
						fetch = null;
						nix = ./samplePackage/dep-user.nix;
					};
				}; })
			]; }).sources.dep-user.drv.depProvided)
			true)

		(eq "doesn't override imports globally"
			# import a bunch of named sources that `git` depends on,
			# then assert that the resulting git is equal to the
			# upstream one, because its dependencies haven't changed
			(api.importFrom { sources =
				let impl = {
						type="_passthru";
						fetch = null;
						nix = ./samplePackage/unbuildable.nix;
					}; in
				[(addHeader { sources = {
					curl = impl;
					openssl = impl;
					zlib = impl;
					openssh = impl;
					stdenv = impl;
					upstreamGit = {
						type="_passthru";
						fetch = null;
						nix = ./samplePackage/exposeGit.nix;
					};
				}; })];
			}).sources.upstreamGit.drv.git
			pkgs.git)

		(eq "normalizes non-store paths without adding them to the store" (
			internal.expandRelativePathWithoutImporting ./. "."
		) ("${builtins.getEnv "PWD"}/test") )

		(eq "normalizes store paths" (
			internal.expandRelativePath curl "."
		) curl.outPath )

		(eq "imports from git when path is not a store path" (
			let result = ((internal.makeFetchers { path = ./storeSrc; })
				.git-local { relativePath = "../.."; ref="HEAD"; }); in
			result
		) (builtins.fetchGit { url = ../.; ref="HEAD"; }) )

		(eq "uses store path directly path is a store path" (
			# "${x}" copies path x into the store
			let result = ((internal.makeFetchers { path = "${./storeSrc}"; })
				.git-local { relativePath = "."; ref="HEAD"; }); in
			[(typeOf result) (isDerivation result) result]
		) ["string" false "${./storeSrc}"])

		(eq "importFrom merges packages, not recursively" (
			(api.importFrom { sources = [
				(addHeader { sources = {
					first = { type="git"; key = "value1"; };
					both = { type="git"; key1 = "key1value1"; key2 = "key2value1";};
				}; })
				(addHeader { sources = {
					both = { type="git"; key2 = "key2value2"; key3 = "key3value2"; };
					second = { type="git"; key = "value2"; };
				}; })
			]; }).sourceAttrs)
			{
				first = { type="git"; key = "value1"; };
				both = { type="git"; key2 = "key2value2"; key3 = "key3value2"; };
				second = { type="git"; key = "value2"; };
			})

		(eq "allows overriding of individual package invocations" "injected" (api.derivations {
			sources = [ version ];
			extend = nodes: {
				version = {
					call = { pkgs, path }: ((pkgs.callPackage path {}).overrideAttrs (o: {
						passthru = { extra = "injected"; };
					}));
				};
			};
		}).version.extra)

		(eq "inject works with just a path" ./samplePackage/upstream-src
			(api.inject { path = ./samplePackage; }).src)

		(eq "inject works with an attrset and no `self`" ./samplePackage/upstream-src (
			api.inject {
				sources = [ version ];
				nix = ({ pkgs, version }: pkgs.callPackage ./samplePackage/nix {});
			}
		).src)

		(eq "inject overrides src if `self` is given" "${./samplePackage/local-src}" (
			api.inject {
				sources = [ version (addHeader {
					sources.self = { type = "path"; fetch = { path = ./samplePackage/local-src; }; };
				})];
				nix = ({ pkgs, version }: pkgs.callPackage ./samplePackage/nix {});
			}
		).src)

		(eq "inject uses provides packages in callPackage scope" (
			api.inject {
				sources = [];
				nix = ({ foo, self }: foo);
				provided = { foo = "hello from FOO!"; };
			})
			("hello from FOO!")
		)

		(eq "makeImport provides sources individually via `self`" (
			let imports = internal.importsOfJson {path = null;} {
				version = versionSrc // {
					nix = ./return-self-1.nix;
				};
				curl = {
					type = "_passthru";
					fetch = curl.src;
					nix = ./return-self-2.nix;
				};
			}; in [imports.version.drv imports.curl.drv])
			[ (makeImport "name" versionSrc).src curl.src ])

	];
	failures = concatMap testFailure tests;
in
if (length failures) == 0 then "OK" else abort (lib.concatStringsSep "\n" failures)
