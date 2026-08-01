# Jazz visual identity

The Jazz identity is a precise editorial score in deep ink and warm brass. Its
primary symbol is an original saxophone silhouette shaped as the letter `J`.
The restrained geometry remains legible from the favicon through the oversized
homepage motif.

## Asset roles

- `jazz-mark.svg` and `jazz-mark-dark.svg` are the standalone light- and
  dark-surface marks.
- `jazz-wordmark.svg` and `jazz-wordmark-dark.svg` pair the mark with original
  geometric `azz` lettering. The wordmark contains no converted font outlines.
- `favicon.svg` is a deliberately simplified mark on a deep-ink field.
- `editors/vscode-jazz/icon.png` is the deterministic 128×128 editor raster
  generated from the canonical light-surface `jazz-mark.svg`.
- `social-card.svg` is the editable 1200×630 source; `social-card.png` is its
  deterministic rendered counterpart.

Use the light variant on paper or pale surfaces and the dark variant on deep
ink. Do not recolor the mark, apply filters, or add gradients, shadows, gloss,
or key details. Decorative instances in React must use empty alternative text;
standalone informative images should keep the descriptions embedded here.

## Palette

- Deep ink: `#171824`
- Warm brass: `#D49A35`
- Warm paper: `#F3EDDF`
- Score line: `#2A2C3A`

Warm brass is the single accent. Paper and score-line colors are neutral
contrast surfaces, not additional accents.

## Typography provenance

The social-card tagline is derived from the Latin variable WOFF2 in the pinned
`@fontsource-variable/manrope@5.3.0` package. Manrope is copyright 2019 The
Manrope Project Authors and distributed under the SIL Open Font License 1.1;
the complete license is installed at
`website/node_modules/@fontsource-variable/manrope/LICENSE` by `npm ci`.

The renderer verifies the font asset SHA-256, uses pinned `fontkit@2.0.4` to
lay out the tagline, and replaces the paths between the
`manrope-tagline` markers in `social-card.svg`. Sharp receives only that
path-based SVG, so host fonts and host font configuration cannot affect the
PNG. The expected font SHA-256 is
`a30ddcd349703aff7464c34bef3fffdff405ee50c113440d7c8693c02d210972`.

## Regenerating the social card

From the repository root, install the pinned website dependencies and render:

```bash
npm --prefix website ci
npm --prefix website run render:brand
```

The renderer synchronizes the licensed Manrope outlines in `social-card.svg`,
writes `social-card.png` and the editor icon through Sharp, and fails unless
their dimensions and formats match their contracts. `npm --prefix website run
test:brand` validates the outline source, transparent asset padding, favicon
legibility, and the editor icon's exact provenance.
