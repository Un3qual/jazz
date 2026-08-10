# Jazz visual identity

The Jazz identity pairs deep ink with a yellow-to-orange brass gradient. Its
primary symbol is a wide saxophone tube shaped as the letter `J`, with a bold
edge and three deliberate key details. The same centerline and stroke ratio
carry from the favicon through the oversized homepage motif.

## Asset roles

- `jazz-mark.svg` and `jazz-mark-dark.svg` are the standalone light- and
  dark-surface versions of the balanced wide-bold mark.
- `jazz-wordmark.svg` and `jazz-wordmark-dark.svg` pair that mark with the
  approved custom `JAZZ` lettering at matching optical height and weight. The
  wordmark contains no converted font outlines.
- `favicon.svg` fits the same Bellhook geometry to a deep-ink field.
- `editors/vscode-jazz/icon.png` is the deterministic 128×128 editor raster
  generated from the canonical light-surface `jazz-mark.svg`.
- `social-card.svg` is the editable 1200×630 source; `social-card.png` is its
  deterministic rendered counterpart.

Use the light variant on paper or pale surfaces and the dark variant on deep
ink. Keep the centerline, 120:72 edge-to-tube stroke ratio, gradient stops, and
wordmark geometry unchanged. Do not apply filters, shadows, or gloss.
Decorative instances in React must use empty alternative text; standalone
informative images should keep the descriptions embedded here.

## Palette

- Logo ink: `#24182C`
- Stage ink: `#171824`
- Brass highlight: `#FFE66A`
- Brass midtone: `#FFC43D`
- Brass finish: `#F47A32`
- Warm paper: `#F3EDDF`
- Score line: `#2A2C3A`

The three brass colors form one continuous tube treatment. Paper and score-line
colors are neutral contrast surfaces, not additional accents.

## Typography provenance

The social-card tagline is derived from the Latin variable WOFF2 in the pinned
`@fontsource-variable/manrope@5.3.0` package. Manrope is copyright 2019 The
Manrope Project Authors and distributed under the SIL Open Font License 1.1;
the complete license is installed at
`website/node_modules/@fontsource-variable/manrope/LICENSE` by
`pnpm --dir website install --frozen-lockfile`.

The renderer verifies the font asset SHA-256, uses pinned `fontkit@2.0.4` to
lay out the tagline, and replaces the paths between the
`manrope-tagline` markers in `social-card.svg`. Sharp receives only that
path-based SVG, so host fonts and host font configuration cannot affect the
PNG. The expected font SHA-256 is
`a30ddcd349703aff7464c34bef3fffdff405ee50c113440d7c8693c02d210972`.

## Regenerating the social card

From the repository root, install the pinned website dependencies and render:

```bash
pnpm --dir website install --frozen-lockfile
pnpm --dir website run render:brand
```

The renderer synchronizes the licensed Manrope outlines in `social-card.svg`,
writes `social-card.png` and the editor icon through Sharp, and fails unless
their dimensions and formats match their contracts. `pnpm --dir website run
test:brand` validates the outline source, transparent asset padding, favicon
legibility, and the editor icon's exact provenance.
