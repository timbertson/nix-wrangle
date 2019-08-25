
# nix-wrangle

## Purpose:

Nix-wrangle aims to be a swiss-army knife for working with nix packages (or dependencies).
It works best with dependencies that include their own nix derivations, although using it for fetching plain archives works too.

## Basic functionality:

nix-wrangle maintains a set of sources, which are typically dependencies. The following example shows the basic setup:

```bash

# Here's our derivation, nix/default.nix. It expects a \`piep\` argument, and doesn't have its own `src`:
$ cat nix/default.nix
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

# Initialize nix/wrangle.json (pass `--pkgs nixos-unstable` to pin nixpkgs version):
$ nix-wrangle init
Adding "nix-wrangle" // PackageSpec {sourceSpec = Github (GithubSpec {ghOwner = "timbertson", ghRepo = "nix-wrangle", ghRef = Template "v1"}), fetchAttrs = fromList [], packageAttrs = fromList [("nix","default.nix")]}
fetching Github (GithubSpec {ghOwner = "timbertson", ghRepo = "nix-wrangle", ghRef = Template "v1"})
Resolved nix-wrangle -> v1 -> 1583199d2c4ff5533780e48f483488b6bef2f238
 - sha256:1xb87hcqpc60pjjfjnk10n1clrnzl9z8xjzrjk9h5dgkmih7x5gb
Writing: nix/wrangle.json
Writing: default.nix

# Add `self`, which will replace `src` in the toplevel derivation.

# The path is `..` because we're in a subdirectory - normally this would just be `.`
$ nix-wrangle add self --type git-local ..
Reading sources: nix/wrangle.json
Adding "self" // PackageSpec {sourceSpec = GitLocal (GitLocalSpec {glPath = RelativePath "..", glRef = Template "HEAD"}), fetchAttrs = fromList [], packageAttrs = fromList [("nix","default.nix")]}
fetching GitLocal (GitLocalSpec {glPath = RelativePath "..", glRef = Template "HEAD"})
Writing: nix/wrangle.json

# Now provide the `piep` dependency from a github repo:
$ nix-wrangle add piep timbertson/piep --nix nix/
Reading sources: nix/wrangle.json
Adding "piep" // PackageSpec {sourceSpec = Github (GithubSpec {ghOwner = "timbertson", ghRepo = "piep", ghRef = Template "master"}), fetchAttrs = fromList [], packageAttrs = fromList [("nix","nix/")]}
fetching Github (GithubSpec {ghOwner = "timbertson", ghRepo = "piep", ghRef = Template "master"})
Resolved piep -> master -> d805330386553c5784ac7ef48ff38aea716575dc
 - sha256:1q7lzi3lggw8by4pkh5ckkjv68xqbjahrkxgdw89jxdlyd0wq5in
Writing: nix/wrangle.json

# When building, we need to disable build-use-chroot due to the `git-local` source
$ nix-build --option build-use-chroot false
trace: [wrangle] Loading /nix/store/fgz707z3n7mrrajlzfwx98d0ad4ajry0-source/nix/wrangle.json
trace: [wrangle] injecting src from `self` dependency
trace: [wrangle] Loading /home/tim/dev/nix/nix-wrangle/example/nix/wrangle.json
trace: [wrangle] injecting src from `self` dependency
trace: [wrangle] Importing piep from /nix/store/ax68rn4b8dc4lrcfqq4rhx2fcwdr807a-source/nix/
trace: Dereferenced git ref /home/tim/dev/nix/nix-wrangle/example/../.git/HEAD to ref: refs/heads/v1
trace: Dereferenced git ref /home/tim/dev/nix/nix-wrangle/example/../.git/refs/heads/v1 to 1583199d2c4ff5533780e48f483488b6bef2f238
these derivations will be built:
  /nix/store/sx0gd8wrwdb56xx4mi6d3hwgfsd9gjql-sample.txt.drv
building '/nix/store/sx0gd8wrwdb56xx4mi6d3hwgfsd9gjql-sample.txt.drv'...
/nix/store/bj1xpqnw8qfnrzyj3cppid5w6ydlbhcc-sample.txt

# And here's the result, with injected source and `piep` dependency:
$ cat result
  Sample derivation, built from:
   - self: /nix/store/6h3kaq6wqgcmkv9vwqavanpwc5fy4mgj-git-export
   - piep: /nix/store/7fysz0cm3686f7hkhdk76kws4p8rswa2-python2.7-piep-0.8.1
```

Note that the `piep` dependency is built (by using `pkgs.callPackage` on the nix path within the source), which gives you the actual derivation, not simply the source code. This is one important difference compared to [niv](https://github.com/nmattia/niv).

Sources are typically used for project dependencies, but there are three special sources:

 - 'self': (optional) used to override the toplevel derivation's 'src' attribute when building 'default.nix'
 - 'pkgs': (optional) used to pin the exact version of 'nixpkgs'
 - 'nix-wrangle': (added automatically) used for bootstrapping in 'default.nix'

# Features:

Nix-wrangle is purpose built to solve a range of specific use cases which come up when developing with nix:

## Update from specification

e.g. when using a git-based dependency, you can specify a branch instead of a specific commit. When you `update`, that branch will be re-resolved:

```bash

# Time to update the `piep` dependency (this re-resolves `master`, or whatever ref is configured)
$ nix-wrangle update piep
Reading sources: nix/wrangle.json
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
Adding "piep" // PackageSpec {sourceSpec = GitLocal (GitLocalSpec {glPath = FullPath "/home/tim/dev/python/piep", glRef = Template "HEAD"}), fetchAttrs = fromList [], packageAttrs = fromList [("nix","nix/")]}
fetching GitLocal (GitLocalSpec {glPath = FullPath "/home/tim/dev/python/piep", glRef = Template "HEAD"})
Writing: nix/wrangle-local.json

$ nix-build --option build-use-chroot false
trace: [wrangle] Loading /nix/store/hxf7m95k4y4ycjwg0qxk6ny202z2q2zw-source/nix/wrangle.json
trace: [wrangle] injecting src from `self` dependency
trace: [wrangle] Loading /home/tim/dev/nix/nix-wrangle/example/nix/wrangle.json
trace: [wrangle] Loading /home/tim/dev/nix/nix-wrangle/example/nix/wrangle-local.json
trace: [wrangle] injecting src from `self` dependency
trace: Dereferenced git ref /home/tim/dev/python/piep/.git/HEAD to ref: refs/heads/master
trace: Dereferenced git ref /home/tim/dev/python/piep/.git/refs/heads/master to 3b493de55e6c29ee173013c43e79ccddbcf6e1e5
trace: [wrangle] Importing piep from /nix/store/7gln9xv5cxzj22i1dnh3ysvk3jp2r5ql-git-export/nix/
trace: Dereferenced git ref /home/tim/dev/nix/nix-wrangle/example/../.git/HEAD to ref: refs/heads/v1
trace: Dereferenced git ref /home/tim/dev/nix/nix-wrangle/example/../.git/refs/heads/v1 to 1583199d2c4ff5533780e48f483488b6bef2f238
these derivations will be built:
  /nix/store/af2bsjlqiby72jw60ax5s7bsvkbbpbph-git-export.drv
  /nix/store/hymb95nxw6qnin4kcr3mpyisvkh8a7g0-sample.txt.drv
building '/nix/store/af2bsjlqiby72jw60ax5s7bsvkbbpbph-git-export.drv'...
Exporting git revision 1583199d2c4ff5533780e48f483488b6bef2f238
building '/nix/store/hymb95nxw6qnin4kcr3mpyisvkh8a7g0-sample.txt.drv'...
/nix/store/9y313glsfb3filf08wyknxn72jz479ca-sample.txt

$ cat result
  Sample derivation, built from:
   - self: /nix/store/6h3kaq6wqgcmkv9vwqavanpwc5fy4mgj-git-export
   - piep: /nix/store/ylmmn3i3fjk7zqvik9isdpbbxvfv3jxm-python2.7-piep-0.9.2
```

This uses the local version of a dependency for building, but kept separate from the "public" version of your dependency specificaion.

## Splicing `src` to produce a self-contained derivation

nix-wrangle was built so that your base derivation (`nix/default.nix`) can be idiomatic - it doesn't need to reference `nix-wrangle` at all, and its dependencies are injected as arguments, just like regular derivations in `nixpkgs`. The one way in which they aren't idiomatic is the `src` attribute. I typically set this to `null` to make it clear that it's externally managed.

So there's also the `splice` command. This injects the current value of a fetched source (defaulting to `self`) into an existing nix file to create a self-contained derivation. This is perfect for promoting your in-tree derivation (with source provided by nix-wrangle) into a derivation suitable for inclusion in `nixpkgs`, where it includes its own `src` and all dependencies are provided by the caller.

```bash

# Splice the `nix-wrangle` source into nix/default.nix
$ nix-wrangle splice nix/default.nix --name nix-wrangle --output nix/public.nix
Reading sources: nix/wrangle.json
Writing: nix/public.nix

$ cat nix/public.nix
{ stdenv, piep }:
stdenv.mkDerivation {
  name="sample.txt";
  src = fetchFromGitHub {
    rev = "9664079e09a29139edc29cb2851328f0e22120ac";
    sha256 = "1kbj7pfhsjdvzdmjjhj7a82licjpvba6a7phj7bb40gvndgl7als";
    repo = "nix-wrangle";
    owner = "timbertson";
  };
  buildCommand = ''
    cat > "$out" <<EOF
      Sample derivation, built from:
       - self: $src
       - piep: ${piep}
    EOF
  '';
}
```

# Source types

 - `url`: any archive URL. May contain `<version>` which is resolved on `update`.
 - `git`: takes a `url` and `rev` (which can be a branch, tag or commit). `rev` is resolved to a concrete commit on initial add, and on `update`.
 - `github`: takes an `owner`, `repo` and `rev` (as for `git`)
 - `git-local`: takes a path (can be relative) and a `rev` (can be a branch, tag, commit or `HEAD`). `rev` is resolved _at evaluation time_, and must be built with `--option build-use-chroot false`
 - `path`: path (can be relative) and `rev`

----

### 'build-use-chroot' caveat:

The 'git-local' source type is "morally pure" - it uses a git commit SHA in the derivation, so (barring git commit collisions), it will be cached / rebuilt just like any other nix derivation. But it's implemented with some impurity -- the source derivation runs 'git-export' in your workspace.

Unfortunately this requires that you pass '--option build-use-chroot false' when building these kinds of sources.

(TODO how does this work for multi-user setup? It probably doesn't.)

In order to not disable the chroot for an entire build, you can run 'nix-wrangle prebuild' to specifically prebuild all such sources with chroot disabled. Then the main build will work in a chroot, using these prebuilt sources.

(TODO this probably doesn't work if you have pinned nixpkgs (different .drv))

# Hacking

There are two options for development:

From within 'nix-shell', use 'cabal v1-build'

Alternatively, from within 'nix-shell -p haskellPackages.cabal-install' you can use 'cabal new-build'. This bypasses the nix infrastructure entirely and fetches its own copy of dependencies, but may be convenient when adjusting packages or pinning dependencies to specific versions.

# Similar tools

### niv:

nix-wrangle was heavily inspired by [niv](https://github.com/nmattia/niv) (the command line tool was even based off the niv source code).
The main differences are:

 - nix-wrangle dependencies are derivations, not just source code.
 - nix-wrangle attempts to let you write idiomatic nix without explicitly referencing any files or functions provided by nix-wrangle.
 - nix-wrangle has a number of extra features not provided by niv: `splice`, `self` injection and local overlays.

### nix-flakes:

nix-wrangle (like niv) is similar in spirit to [nix flakes](https://gist.github.com/edolstra/40da6e3a4d4ee8fd019395365e0772e7), but there's no actual implementation of flakes yet. My hope is that any standard solution would be able to support nix-wrangle style workflows.

