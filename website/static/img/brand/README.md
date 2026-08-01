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

## Regenerating the social card

From the repository root, install the pinned website dependencies and render:

```bash
npm --prefix website ci
npm --prefix website run render:brand
```

The renderer reads `social-card.svg`, writes `social-card.png` through Sharp,
and fails unless both source and output are exactly 1200×630 pixels.
