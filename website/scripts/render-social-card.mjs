import {createHash} from 'node:crypto';
import {mkdtemp, readFile, rename, rm, writeFile} from 'node:fs/promises';
import {fileURLToPath} from 'node:url';
import path from 'node:path';

import * as fontkit from 'fontkit';
import sharp from 'sharp';

const scriptPath = fileURLToPath(import.meta.url);
const scriptDirectory = path.dirname(scriptPath);
const websiteDirectory = path.resolve(scriptDirectory, '..');
const imageDirectory = path.join(websiteDirectory, 'static/img');
const sourcePath = path.join(imageDirectory, 'social-card.svg');
const outputPath = path.join(imageDirectory, 'social-card.png');
const defaultFontPath = path.join(
  websiteDirectory,
  'node_modules/@fontsource-variable/manrope/files/manrope-latin-wght-normal.woff2',
);

const tagline = 'A statically typed functional language with practical syntax';
const fontDigest = 'a30ddcd349703aff7464c34bef3fffdff405ee50c113440d7c8693c02d210972';
const taglineStart = '<!-- manrope-tagline:start -->';
const taglineEnd = '<!-- manrope-tagline:end -->';
const requiredWidth = 1200;
const requiredHeight = 630;

function coordinate(value) {
  return Number(value.toFixed(3)).toString();
}

async function generatedTagline(fontPath) {
  const fontBytes = await readFile(fontPath);
  const digest = createHash('sha256').update(fontBytes).digest('hex');
  if (digest !== fontDigest) {
    throw new Error(`Manrope font digest mismatch for ${fontPath}`);
  }

  const font = fontkit.create(fontBytes);
  const run = font.layout(tagline);
  const fontSize = 31;
  const letterSpacing = 0.2;
  const scale = fontSize / font.unitsPerEm;
  const letterSpacingUnits = letterSpacing / scale;
  let cursor = 0;
  const paths = run.glyphs.map((glyph, index) => {
    const position = run.positions[index];
    const x = cursor + position.xOffset;
    const y = position.yOffset;
    cursor += position.xAdvance + letterSpacingUnits;
    return `    <path d="${glyph.path.toSVG()}" transform="translate(${coordinate(x)} ${coordinate(y)})"/>`;
  });

  return [
    `  ${taglineStart}`,
    `  <g transform="translate(96 487) scale(${coordinate(scale)} ${coordinate(-scale)})" fill="#F3EDDF" stroke="#F3EDDF" stroke-width="35" stroke-linejoin="round" aria-label="${tagline}" data-font-source="@fontsource-variable/manrope@5.3.0">`,
    ...paths,
    '  </g>',
    `  ${taglineEnd}`,
  ].join('\n');
}

export async function synchronizeSocialCardSource(
  source,
  fontPath = process.env.JAZZ_BRAND_FONT_PATH
    ? path.resolve(process.env.JAZZ_BRAND_FONT_PATH)
    : defaultFontPath,
) {
  const startMarker = `  ${taglineStart}`;
  const endMarker = `  ${taglineEnd}`;
  const start = source.indexOf(startMarker);
  const end = source.indexOf(endMarker, start + startMarker.length);
  if (
    start < 0
    || end < 0
    || source.indexOf(startMarker, start + startMarker.length) >= 0
    || source.indexOf(endMarker, end + endMarker.length) >= 0
  ) {
    throw new Error('social-card.svg is missing the Manrope tagline markers');
  }
  const generated = await generatedTagline(fontPath);
  return `${source.slice(0, start)}${generated}${source.slice(end + endMarker.length)}`;
}

export async function renderSocialCard({
  svgPath = sourcePath,
  pngPath = outputPath,
  fontPath = process.env.JAZZ_BRAND_FONT_PATH
    ? path.resolve(process.env.JAZZ_BRAND_FONT_PATH)
    : defaultFontPath,
} = {}) {
  const source = await readFile(svgPath, 'utf8');
  const synchronized = await synchronizeSocialCardSource(source, fontPath);
  if (synchronized !== source) await writeFile(svgPath, synchronized, 'utf8');

  const svg = Buffer.from(synchronized);
  const sourceMetadata = await sharp(svg).metadata();
  if (sourceMetadata.width !== requiredWidth || sourceMetadata.height !== requiredHeight) {
    throw new Error(
      `social-card.svg must be ${requiredWidth}x${requiredHeight}; received ${sourceMetadata.width ?? 'unknown'}x${sourceMetadata.height ?? 'unknown'}`,
    );
  }

  const temporaryDirectory = await mkdtemp(
    path.join(path.dirname(pngPath), `.${path.basename(pngPath)}-`),
  );
  const temporaryPath = path.join(temporaryDirectory, path.basename(pngPath));
  try {
    await sharp(svg, {density: 72})
      .png({
        adaptiveFiltering: false,
        compressionLevel: 9,
        effort: 10,
        palette: false,
      })
      .toFile(temporaryPath);

    const output = await sharp(temporaryPath).metadata();
    if (output.width !== requiredWidth || output.height !== requiredHeight) {
      throw new Error(
        `rendered social card must be ${requiredWidth}x${requiredHeight}; received ${output.width ?? 'unknown'}x${output.height ?? 'unknown'}`,
      );
    }

    await rename(temporaryPath, pngPath);
    console.log(`Rendered ${path.relative(process.cwd(), pngPath)} (${requiredWidth}x${requiredHeight}).`);
  } finally {
    await rm(temporaryDirectory, {recursive: true, force: true});
  }
}

if (process.argv[1] && path.resolve(process.argv[1]) === scriptPath) {
  await renderSocialCard();
}
