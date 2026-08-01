import {rename, rm} from 'node:fs/promises';
import {fileURLToPath} from 'node:url';
import path from 'node:path';

import sharp from 'sharp';

const scriptDirectory = path.dirname(fileURLToPath(import.meta.url));
const imageDirectory = path.resolve(scriptDirectory, '../static/img');
const sourcePath = path.join(imageDirectory, 'social-card.svg');
const outputPath = path.join(imageDirectory, 'social-card.png');
const temporaryPath = path.join(imageDirectory, '.social-card.tmp.png');

const requiredWidth = 1200;
const requiredHeight = 630;

const source = await sharp(sourcePath).metadata();
if (source.width !== requiredWidth || source.height !== requiredHeight) {
  throw new Error(
    `social-card.svg must be ${requiredWidth}x${requiredHeight}; received ${source.width ?? 'unknown'}x${source.height ?? 'unknown'}`,
  );
}

try {
  await sharp(sourcePath, {density: 72})
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

  await rename(temporaryPath, outputPath);
  console.log(`Rendered ${path.relative(process.cwd(), outputPath)} (${requiredWidth}x${requiredHeight}).`);
} finally {
  await rm(temporaryPath, {force: true});
}
