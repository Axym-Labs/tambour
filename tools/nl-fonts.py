"""JetBrains Mono NL from the bundled JetBrains Mono: the same font without `calt`, which holds
all its ligatures, and named apart so both can be loaded. The editor takes it when ligatures are
turned off in Preferences. The OFL allows the change; the font names no Reserved Font Name.

    python3 tools/nl-fonts.py
"""

from pathlib import Path

from fontTools import subset
from fontTools.ttLib import TTFont

fonts = Path(__file__).resolve().parent.parent / "src" / "Tambour" / "Fonts"

for weight in ["Regular", "Bold"]:
    source = fonts / f"JetBrainsMono-{weight}.ttf"
    target = fonts / f"JetBrainsMonoNL-{weight}.ttf"
    font = TTFont(source)
    options = subset.Options()
    options.glyph_names = True
    options.name_IDs = ["*"]
    options.name_languages = ["*"]
    options.name_legacy = True
    options.notdef_outline = True
    options.legacy_kern = True
    options.hinting = True
    tags = {r.FeatureTag for table in ("GSUB", "GPOS") for r in font[table].table.FeatureList.FeatureRecord}
    options.layout_features = sorted(tags - {"calt"})
    sub = subset.Subsetter(options)
    sub.populate(unicodes=font.getBestCmap().keys(), glyphs=font.getGlyphOrder())
    sub.subset(font)

    for record in font["name"].names:
        text = record.toUnicode()
        if record.nameID in (1, 3, 4, 16, 18, 21):
            record.string = text.replace("JetBrains Mono", "JetBrains Mono NL")
        elif record.nameID in (6, 20):
            record.string = text.replace("JetBrainsMono", "JetBrainsMonoNL")

    font.save(target)
    print(target.name)
