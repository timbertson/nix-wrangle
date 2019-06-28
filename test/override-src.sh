#!/bin/bash
set -eux
here="$(dirname "$0")"

gitHeadExport="$("$here/../bin/nix-export-git" --ref HEAD)"
[ -n "$gitHeadExport" ]
gitShaExport="$("$here/../bin/nix-export-git" --commit 0fa3135a596c76cb5c3bae1f9521ea84ef84fef5)"
[ -n "$gitShaExport" ]
gitMasterExport="$("$here/../bin/nix-export-git" --ref master --unpack)"
[ -n "$gitMasterExport" ]
gitWorkingCopyExport="$("$here/../bin/nix-export-git" --working-changes)"
[ -n "$gitWorkingCopyExport" ]

built="$("$here/../bin/nix-override-src" --src "$gitMasterExport" -A gup "$here/test.nix")"
grep -q "just thoughts right now" "$built"
