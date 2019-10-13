<!-- NOTE: README.md is generated from README.md.gup, do not manually edit -->
# nix-wrangle

## Purpose:

Nix-wrangle aims to be a swiss-army knife for working with nix dependencies.
It works best with dependencies that include their own nix derivations, although using it for fetching plain archives works too.

### Goals:

* Simple usage should be _idiomatic_ and portable, with no specific references to nix-wrangle's API
* Keeping sources updated (and seeing their current state) should be trivial
* Support local development across multiple related repositories

## Get it:

```bash

$ nix-build --expr 'import (builtins.fetchTarball "https://github.com/timbertson/nix-wrangle/archive/v1.tar.gz")'
trace: [wrangle] Importing self (git-local) from /nix/store/1d2h4gqmryw30wydxad8ynjcxpkd9avb-w2bvbxf0xrk1fcml8976vrr2g9i019q9-source/default.nix
/nix/store/3ksik1ydgzvwjd2dgh4sj4l2ry1nzd04-nix-wrangle-0.0.0

$ result/bin/nix-wrangle --help
Nix-wrangle - source & dependency manager for Nix projects

Usage: nix-wrangle COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  init                     Initialize nix-wrangle
  add                      Add a source
  rm                       Remove one or more sources
  update                   Update one or more sources
  splice                   Splice current `self` source into a .nix document
  show                     Show source details
  ls                       list sources
  default-nix              Generate default.nix
```

## Basic functionality:

nix-wrangle maintains a set of sources, which are typically dependencies. The following example shows the basic setup:

```bash

# Here's our derivation, nix/default.nix. It expects a \`piep\` argument, and uses a relative path for `src`:
$ cat nix/default.nix
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

# Initialize nix/wrangle.json (pass `--pkgs nixos-unstable` to pin nixpkgs version):
$ nix-wrangle init
Adding "nix-wrangle" // PackageSpec {sourceSpec = Github (GithubSpec {ghOwner = "timbertson", ghRepo = "nix-wrangle", ghRef = Template "v1"}), fetchAttrs = fromList [], packageAttrs = fromList [("nix","nix")]}
fetching Github (GithubSpec {ghOwner = "timbertson", ghRepo = "nix-wrangle", ghRef = Template "v1"})
Resolved nix-wrangle -> v1 -> 2066dd8a382ee974cdbfd109f37a9be1d04f8481
 - sha256:13gfwk6lx8yn0gfxyfrfkqiz1yzicg6qq5219l5fb517n3da5chq
Writing: nix/wrangle.json
Writing: default.nix

# Now provide the `piep` dependency from a github repo, and build it:
$ nix-wrangle add piep timbertson/piep --nix nix/
Adding "piep" // PackageSpec {sourceSpec = Github (GithubSpec {ghOwner = "timbertson", ghRepo = "piep", ghRef = Template "master"}), fetchAttrs = fromList [], packageAttrs = fromList [("nix","nix/")]}
fetching Github (GithubSpec {ghOwner = "timbertson", ghRepo = "piep", ghRef = Template "master"})
Resolved piep -> master -> d805330386553c5784ac7ef48ff38aea716575dc
 - sha256:1q7lzi3lggw8by4pkh5ckkjv68xqbjahrkxgdw89jxdlyd0wq5in
Writing: nix/wrangle.json

$ nix-build
trace: [wrangle] Importing piep (github) from /nix/store/ax68rn4b8dc4lrcfqq4rhx2fcwdr807a-source/nix/
these derivations will be built:
  /nix/store/cwb0s7mgz3mdnv716vdilkhba12s7dpw-sample.txt.drv
building '/nix/store/cwb0s7mgz3mdnv716vdilkhba12s7dpw-sample.txt.drv'...
/nix/store/xgwk9aw9q58xdvfnsyy95rd1bnv6i68r-sample.txt

# And here's the result, with injected source and `piep` dependency:
$ cat result
Sample derivation, built with:
 - piep: /nix/store/6w23yj4hyabxzwcgnl0d3xjr261ywrvr-python2.7-piep-0.8.1
 - src: /nix/store/dsb8l9v2a9l6cxwv9kkbwngb3niak4fy-example
```

Note that the `piep` dependency is built (by using `pkgs.callPackage` on the nix path within the source), which gives you the actual derivation, not simply the source code. This is one important difference compared to [niv][].

Sources are typically used for project dependencies, but there are three special sources:

 - 'nix-wrangle': (added automatically) used for bootstrapping in 'default.nix'
 - 'self': used to override the toplevel derivation's 'src' attribute when building 'default.nix'. Automatically added by `nix-wrangle init` as a `git-local` source if there's a `.git` directory present.
 - 'pkgs': (optional) used to pin the exact version of 'nixpkgs'. Added automatically by `nix-wrangle init` if you pass `--pkgs nixpkgs-unstable` (or any other branch name from https://github.com/NixOS/nixpkgs-channels)

# Features:

Nix-wrangle is purpose built to solve a range of specific use cases which come up when developing with nix:

## Update from specification

e.g. when using a git-based dependency, you can specify a branch instead of a specific commit. When you `update`, that branch will be re-resolved:

```bash

# Time to update the `piep` dependency (this re-resolves `master`, or whatever ref is configured)
$ nix-wrangle update piep
Updating nix/wrangle.json ...
 - updating "piep"...
fetching Github (GithubSpec {ghOwner = "timbertson", ghRepo = "piep", ghRef = Template "master"})
Resolved piep -> master -> d805330386553c5784ac7ef48ff38aea716575dc
 - sha256:1q7lzi3lggw8by4pkh5ckkjv68xqbjahrkxgdw89jxdlyd0wq5in
Writing: nix/wrangle.json
```

You can also make use of templating, e.g. for a URL dependency you can use the URL `'http://example.com/libfoo/libfoo-<version>.tgz'`. When updating, you can pass `--version NEW_VERSION` to update it.

## 'src' injection

When building, a `self` dependency (if present) will be used to provide the `src` for the toplevel derivation. This lets you use nix-wrangle's various source types (e.g git-local, which isn't available as a `nixpkgs` builtin) and automatic generation (e.g. sha256 digests).

nix-wrangle will also inject the relevant `src` into each of your dependencies. Let's say you import commit `x` of the `piep` dependency, with a nix expression in it. It would be impossible for commit `x` of `piep` to refer to commit `x` as its `src` attribute. The best it could do is to refer to the parent of commit `x`, although it may often just refer to the most recently released version. Both of these would be counter-productive - you'd be importing the `derivation` at `x`, but building source code from _some other version_ out of your control. `nix-wrangle` automatically overrides the `src` of imported dependencies so that the version you _import_ is also the source code you _build_.

## Local overrides

When working on both a library and an application that uses it, it's common to want to try out working changes before publishing them. This is easy with local sources:

```bash

# Let's develop against my local checkout of `piep`.
# Local overrides are stored in nix/wrangle-local.json, which you shouldn't commit
$ nix-wrangle add --local piep --type git-local --path /home/tim/dev/python/piep --nix nix/
Adding "piep" // PackageSpec {sourceSpec = GitLocal (GitLocalSpec {glPath = FullPath "/home/tim/dev/python/piep", glRef = Nothing}), fetchAttrs = fromList [], packageAttrs = fromList [("nix","nix/")]}
fetching GitLocal (GitLocalSpec {glPath = FullPath "/home/tim/dev/python/piep", glRef = Nothing})
Writing: nix/wrangle-local.json

$ nix-build
trace: [wrangle] Importing piep (git-local) from /nix/store/ba4y245q521hk8nyf4f272zgnb7js5v7-source/nix/
these derivations will be built:
  /nix/store/y0gjhrcidms9k62sb4wcsq8x14y094p7-sample.txt.drv
building '/nix/store/y0gjhrcidms9k62sb4wcsq8x14y094p7-sample.txt.drv'...
/nix/store/jhcdh3fc5nr41v660xw50pyb6xa48hzi-sample.txt

$ cat result
Sample derivation, built with:
 - piep: /nix/store/zlnz9al4hykj9f1mx9018l1xl4fd5x6w-python2.7-piep-0.9.2
 - src: /nix/store/5099nhjdbz5bb5yhc0bd34ady2ymp14m-example
```

This uses the local version of a dependency for building, but kept separate from the "public" version of your dependency specificaion.

## Splicing `src` to produce a self-contained derivation

nix-wrangle was built so that your base derivation (`nix/default.nix`) can be idiomatic - it doesn't need to reference `nix-wrangle` at all, and its dependencies are injected as arguments, just like regular derivations in `nixpkgs`. The one way in which they aren't idiomatic is the `src` attribute, since in nixpkgs this typically refers to a remote repository or tarball.

So there's also the `splice` command. This injects the current value of a fetched source (defaulting to `public`) into an existing nix file to create a self-contained derivation. This is perfect for promoting your in-tree derivation (with source provided by nix-wrangle) into a derivation suitable for inclusion in `nixpkgs`, where it includes its own `src` and all dependencies are provided by the caller.

```bash

# Splice the `nix-wrangle` source into nix/default.nix
$ nix-wrangle splice nix/default.nix --name nix-wrangle --output nix/public.nix
Updating nix/wrangle.json ...
 - updating "nix-wrangle"...
fetching Github (GithubSpec {ghOwner = "timbertson", ghRepo = "nix-wrangle", ghRef = Template "v1"})
   ... (unchanged)
Writing: nix/wrangle.json
Updating nix/wrangle-local.json ...
Writing: nix/wrangle-local.json
Writing: nix/public.nix

$ cat nix/public.nix
{ stdenv, piep }:
stdenv.mkDerivation {
  name="sample.txt";
  src = fetchFromGitHub {
    rev = "2066dd8a382ee974cdbfd109f37a9be1d04f8481";
    sha256 = "13gfwk6lx8yn0gfxyfrfkqiz1yzicg6qq5219l5fb517n3da5chq";
    repo = "nix-wrangle";
    owner = "timbertson";
  };
  buildCommand = ''
cat > "$out" <<EOF
Sample derivation, built with:
 - piep: ${piep}
 - src: $src
EOF
  '';
}
```

# Source types

 - `url`: any archive URL. May contain `<version>` which is resolved on `update`.
 - `git`: takes a `url` and `rev` (which can be a branch, tag or commit). `rev` is resolved to a concrete commit on initial add, and on `update`.
 - `github`: takes an `owner`, `repo` and `rev` (as for `git`)
 - `git-local`: takes a path (can be relative) and an optional `rev` (can be a branch, tag or `HEAD`). `rev` is resolved _at evaluation time_. If `rev` is not provided, you'll get the _working changes_ in the given workspace (but not any excluded or untracked files).
 - `path`: path (can be relative or absolute)

----

# Hacking

There are two options for development:

From within 'nix-shell', use 'cabal v1-build'

Alternatively, from within 'nix-shell -p haskellPackages.cabal-install' you can use 'cabal new-build'. This bypasses the nix infrastructure entirely and fetches its own copy of dependencies, but may be convenient when adjusting packages or pinning dependencies to specific versions.

# Similar tools

### niv:

nix-wrangle was heavily inspired by [niv][] (the command line tool was even based off the niv source code).
The main differences are:

 - nix-wrangle dependencies are derivations, not just source code.
 - nix-wrangle attempts to let you write idiomatic nix without explicitly referencing any files or functions provided by nix-wrangle.
 - nix-wrangle has a number of extra features not provided by niv: `splice`, `self` injection and local overlays.
 - nix-wrangle is more featureful, but also more complex

### nix-flakes:

nix-wrangle (like [niv][]) is similar in spirit to [nix flakes](https://gist.github.com/edolstra/40da6e3a4d4ee8fd019395365e0772e7), but there's no actual implementation of flakes yet. My hope is that any standard solution would be able to support nix-wrangle style workflows.

[niv]: https://github.com/nmattia/niv

