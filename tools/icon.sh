#!/usr/bin/env bash
# Renders Tambour's icon from assets/tambour.svg: assets/tambour.png at 256 px, the window's and
# the AppImage's, and assets/tambour.ico, the Windows executable's, with assets/tambour-small.svg
# drawn for its 16 and 24 px images. Needs rsvg-convert and Python with Pillow; the outputs are
# committed, so a build needs neither.
set -euo pipefail
cd "$(dirname "$0")/.."

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
for size in 16 24; do
  rsvg-convert -w $size -h $size assets/tambour-small.svg -o "$work/$size.png"
done
for size in 32 48 64 128 256; do
  rsvg-convert -w $size -h $size assets/tambour.svg -o "$work/$size.png"
done
cp "$work/256.png" assets/tambour.png
# Pillow keeps each image PNG-compressed, where ImageMagick writes the larger ones as bitmaps.
python3 -I - "$work" assets/tambour.ico <<'PY'
import sys
from PIL import Image
images = [Image.open(f"{sys.argv[1]}/{size}.png") for size in (256, 128, 64, 48, 32, 24, 16)]
images[0].save(sys.argv[2], sizes=[i.size for i in images], append_images=images[1:])
PY
echo assets/tambour.png assets/tambour.ico
