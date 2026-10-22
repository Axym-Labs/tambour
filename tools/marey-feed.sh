#!/usr/bin/env bash
# Packs a commit of Marey into the local feed that nuget.config names.
#
#   tools/marey-feed.sh [marey-checkout] [ref]
#
# Without a ref it packs the commit pinned in Directory.Packages.props and checks that the pinned
# version is the one that commit packs as. With a ref (say `main`) it packs that commit and prints
# the two lines to put in Directory.Packages.props. The version is 1.0.0-dev.<n>, where n counts
# the commits the packed one reaches, so versions order as main's commits do.
#
# The checkout itself is never touched: the commit is packed from a temporary worktree.
set -euo pipefail

here=$(cd "$(dirname "$0")/.." && pwd)
marey=${1:-${MAREY_DIR:-$here/../marey}}
feed=$here/artifacts/marey-feed
props=$here/Directory.Packages.props

pinned_commit=$(sed -n 's:.*<MareyCommit>\(.*\)</MareyCommit>.*:\1:p' "$props")
pinned_version=$(sed -n 's:.*<MareyVersion>\(.*\)</MareyVersion>.*:\1:p' "$props")

ref=${2:-$pinned_commit}
sha=$(git -C "$marey" rev-parse --verify "$ref^{commit}")
version=1.0.0-dev.$(git -C "$marey" rev-list --count "$sha")

if [ -z "${2:-}" ] && [ "$version" != "$pinned_version" ]; then
  echo "error: $sha packs as $version, but Directory.Packages.props pins $pinned_version" >&2
  echo "       (a shallow clone counts too few commits; fetch the whole history)" >&2
  exit 1
fi

if [ -f "$feed/Marey.$version.nupkg" ]; then
  echo "Marey $version is already in $feed"
else
  work=$(mktemp -d)
  trap 'git -C "$marey" worktree remove --force "$work/marey" 2>/dev/null || true; rm -rf "$work"' EXIT
  git -C "$marey" worktree add --quiet --detach "$work/marey" "$sha"
  dotnet build "$work/marey/Marey.slnx" -c Release -p:Version="$version" -nologo -v quiet
  dotnet pack "$work/marey/Marey.slnx" -c Release --no-build -p:Version="$version" -o "$feed" -nologo -v quiet
  echo "packed Marey $version ($sha) into $feed"
fi

if [ -n "${2:-}" ]; then
  echo
  echo "Pin it in Directory.Packages.props:"
  echo "    <MareyVersion>$version</MareyVersion>"
  echo "    <MareyCommit>$sha</MareyCommit>"
fi
