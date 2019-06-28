with import <nixpkgs> {};
with builtins;
with lib;
let api = (callPackage ../nix/api.nix {}); internal = api.internal; in
let
	wrangleHeader = { apiversion = 1; };
	addHeader = j: j // { wrangle = wrangleHeader; };
	eq = msg: a: b: [
		"${msg}: ${toJSON a} != ${toJSON b}" (a == b)
	];

	versionSrc = import samplePackage/versionSrc.nix;

	version = addHeader {
		sources = {
			version = versionSrc;
		};
	};

	versionNoImport = {
		source = ["github" {
			"owner" = "timbertson";
			"repo" = "version";
			"rev" = "version-0.13.1";
			"sha256" = "056l8m0xxl1h7x4fd1hf754w8q4sibpqnwgnbk5af5i66399az61";
		}];
	};

	fakeNixpkgs = addHeader {
		sources = {
			nixpkgs = {
				source = [ "path" { path = (toString ./.); } ];
				nix = "fakeNixpkgs.nix";
			};
		};
	};

	makeImport = internal.makeImport { path = null; };

	checks = [
		(eq "implAttrset with no explicit path"
			(internal.implAttrset { attrs = {}; name = "foo"; } 1)
			{ foo = 1; })

		(eq "implAttrset with multiple paths"
			(internal.implAttrset { attrs = { attrPaths = ["foo" "bar.baz"]; }; } 1)
			{ foo = 1; bar = { baz = 1; }; })

		["implPath is path" (isString (makeImport "name" versionSrc).nix)]

		["nix defaults to default.nix" (hasSuffix "/default.nix" (makeImport "name" versionNoImport).nix)]

		["nix is modifiable" (hasSuffix "/foo.nix" (makeImport "name" (versionSrc // {nix = "foo.nix";})).nix)]

		["src is derivation" (isDerivation (makeImport "name" versionSrc).src)]

		(eq "passthru name" (makeImport "name" versionSrc).name "name")

		(eq "passthru attrs" (makeImport "name" versionSrc).attrs versionSrc)

		["overlay is valid"
			(isDerivation ((makeImport "pythonPackages.versionOverride" versionSrc).overlay
				{inherit callPackage;} # self
				{} # super
			).pythonPackages.versionOverride)]

		["makes derivations" (isDerivation (api.derivations { sources = [ version ]; }).version)]

		(eq "overlay merges up to attr path"
			(
				let result = ((makeImport "a.b.c.version" versionSrc).overlay
					{inherit callPackage;} # self
					{ # super
						a.b.c = {
							d = lib.const 1;
							version = {
								e = "super";
								src = lib.warn "evaluating `const 1`" (const 1);
							};
						};
					}
				); in
			(attrNames result.a.b.c) ++ ([result.a.b.c.version.src.drvPath]))
			["d" "version" ((makeImport "name" versionSrc).src.drvPath)]
		)

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
			(api.inject ./samplePackage).src)

		(eq "inject works with a path which is an attrset of args" "attr!"
			(api.inject ./samplePackage/attrs.nix).custom)

		(eq "inject works with an attrset and no `self`" ./samplePackage/upstream-src (
			api.inject {
				sources = [ version ];
				nix = ({ pkgs, version }: pkgs.callPackage ./samplePackage/default.nix {});
			}
		).src)

		(eq "inject overrides src if `self` is given" ./samplePackage/local-src (
			api.inject {
				sources = [ version (addHeader {
					sources.self = { source = [ "path" { path = ./samplePackage/local-src; } ]; };
				})];
				nix = ({ pkgs, version }: pkgs.callPackage ./samplePackage/default.nix {});
			}
		).src)

	];
	failures = concatMap (test:
		if elemAt test 1 then [] else [(elemAt test 0)]
	) checks;
in
if (length failures) == 0 then "OK" else abort (lib.concatStringsSep "\n" failures)
