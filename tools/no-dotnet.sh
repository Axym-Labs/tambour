#!/usr/bin/env bash
# Runs a command on this machine as if no .NET were installed: a bubblewrap sandbox with an empty
# directory over every place .NET installs, the user's NuGet cache hidden, and no network. A
# self-contained publish of Tambour runs in it; anything that reaches for an SDK fails.
#
#   tools/no-dotnet.sh artifacts/publish/linux-x64/tambour --check-analysis tests/.../unit-error.fsx
set -euo pipefail

if ! command -v bwrap >/dev/null; then
  echo "no-dotnet.sh needs bubblewrap (bwrap)" >&2
  exit 2
fi

empty=$(mktemp -d)
trap 'rmdir "$empty"' EXIT

# The GPU stays, as a user has one; only .NET is taken away.
args=(--bind / / --dev /dev --dev-bind-try /dev/dri /dev/dri --proc /proc --unshare-net --die-with-parent)
for d in /usr/share/dotnet /usr/lib/dotnet /usr/lib64/dotnet /usr/local/share/dotnet /opt/dotnet \
         "$HOME/.dotnet" "$HOME/.nuget"; do
  if [ -e "$d" ]; then args+=(--bind "$empty" "$d"); fi
done

bwrap "${args[@]}" --unsetenv DOTNET_ROOT --unsetenv DOTNET_HOST_PATH -- "$@"
