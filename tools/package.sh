#!/usr/bin/env bash
# Builds Tambour's packages into artifacts/package: a self-contained publish per runtime, as a
# tarball on Linux, which every Linux package wraps (an AppImage of it is made where appimagetool
# is on the path), and a zip on Windows. Each carries the licence, the third-party notices and the
# README.
#
#   tools/package.sh [version]        # the version defaults to git describe
set -euo pipefail
cd "$(dirname "$0")/.."

version=${1:-$(git describe --tags --always --dirty)}
out=artifacts/package
rm -rf "$out" && mkdir -p "$out"

publish() {
  local rid=$1 dir=artifacts/publish/$1
  rm -rf "$dir"
  dotnet publish src/Tambour -c Release -r "$rid" --self-contained -o "$dir" \
    -p:InformationalVersion="$version" -p:DebugType=none -p:GenerateDocumentationFile=false >/dev/null
  cp LICENSE THIRD-PARTY-NOTICES.md README.md src/Tambour/Fonts/OFL-JetBrainsMono.txt "$dir"/
  echo "$dir"
}

# Linux: one relocatable directory in a tarball.
linux=$(publish linux-x64)
name=tambour-$version-linux-x64
tar -C "$(dirname "$linux")" --transform "s,^linux-x64,$name," -czf "$out/$name.tar.gz" linux-x64
echo "$out/$name.tar.gz"

# The AppImage wraps the same directory.
appdir=$out/Tambour.AppDir
mkdir -p "$appdir/usr/lib"
cp -r "$linux" "$appdir/usr/lib/tambour"
cp assets/tambour.png "$appdir/tambour.png"
cat > "$appdir/tambour.desktop" <<DESKTOP
[Desktop Entry]
Type=Application
Name=Tambour
Comment=Measured data, figures and F# scripting
Exec=tambour %F
Icon=tambour
Categories=Science;Education;
DESKTOP
cat > "$appdir/AppRun" <<'APPRUN'
#!/bin/sh
here="$(dirname "$(readlink -f "$0")")"
exec "$here/usr/lib/tambour/tambour" "$@"
APPRUN
chmod +x "$appdir/AppRun"
if command -v appimagetool >/dev/null; then
  ARCH=x86_64 appimagetool --no-appstream "$appdir" "$out/tambour-$version-x86_64.AppImage" >/dev/null
  echo "$out/tambour-$version-x86_64.AppImage"
else
  echo "no appimagetool on the path: the AppDir is at $appdir" >&2
fi

# Windows: a zip of the publish, under a folder of its name.
windows=$(publish win-x64)
name=tambour-$version-win-x64
staging=$(mktemp -d)
cp -r "$windows" "$staging/$name"
(cd "$staging" && python3 -m zipfile -c "$OLDPWD/$out/$name.zip" "$name")
rm -rf "$staging"
echo "$out/$name.zip"
