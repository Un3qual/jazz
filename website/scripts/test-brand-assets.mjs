import assert from 'node:assert/strict';
import {mkdtemp, readFile, rm} from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import {spawnSync} from 'node:child_process';
import {fileURLToPath} from 'node:url';
import test from 'node:test';

import sharp from 'sharp';

const scriptDirectory = path.dirname(fileURLToPath(import.meta.url));
const websiteDirectory = path.resolve(scriptDirectory, '..');
const imageDirectory = path.join(websiteDirectory, 'static/img');
const rendererPath = path.join(scriptDirectory, 'render-social-card.mjs');

async function transparentBounds(assetName) {
  const {data, info} = await sharp(path.join(imageDirectory, assetName), {density: 72})
    .ensureAlpha()
    .raw()
    .toBuffer({resolveWithObject: true});
  let minX = info.width;
  let minY = info.height;
  let maxX = -1;
  let maxY = -1;
  for (let y = 0; y < info.height; y += 1) {
    for (let x = 0; x < info.width; x += 1) {
      if (data[(y * info.width + x) * info.channels + 3] === 0) continue;
      minX = Math.min(minX, x);
      minY = Math.min(minY, y);
      maxX = Math.max(maxX, x);
      maxY = Math.max(maxY, y);
    }
  }
  return {minX, minY, maxX, maxY, width: info.width, height: info.height};
}

test('standalone marks and wordmarks retain transparent canvas padding', async () => {
  for (const assetName of [
    'jazz-mark.svg',
    'jazz-mark-dark.svg',
    'jazz-wordmark.svg',
    'jazz-wordmark-dark.svg',
  ]) {
    const bounds = await transparentBounds(assetName);
    assert.ok(
      bounds.maxX >= 0 && bounds.maxY >= 0,
      `${assetName} has no visible pixels`,
    );
    assert.ok(bounds.minX >= 4, `${assetName} clips or crowds its left edge`);
    assert.ok(bounds.minY >= 4, `${assetName} clips or crowds its top edge`);
    assert.ok(bounds.maxX <= bounds.width - 5, `${assetName} clips or crowds its right edge`);
    assert.ok(bounds.maxY <= bounds.height - 5, `${assetName} clips or crowds its bottom edge`);
  }
});

test('concurrent social-card renders isolate their temporary output', async () => {
  const temporaryDirectory = await mkdtemp(
    path.join(os.tmpdir(), 'jazz-social-card-concurrency-'),
  );
  const pngPath = path.join(temporaryDirectory, 'social-card.png');
  try {
    const {renderSocialCard} = await import('./render-social-card.mjs');
    await Promise.all(
      Array.from({length: 4}, () => renderSocialCard({pngPath})),
    );
    const metadata = await sharp(pngPath).metadata();
    assert.equal(metadata.width, 1200);
    assert.equal(metadata.height, 630);
  } finally {
    await rm(temporaryDirectory, {recursive: true, force: true});
  }
});

test('committed social card matches the deterministic SVG raster', async () => {
  const temporaryDirectory = await mkdtemp(
    path.join(os.tmpdir(), 'jazz-social-card-committed-'),
  );
  const renderedPath = path.join(temporaryDirectory, 'social-card.png');
  try {
    const {renderSocialCard} = await import('./render-social-card.mjs');
    await renderSocialCard({pngPath: renderedPath});
    const [rendered, committed] = await Promise.all([
      readFile(renderedPath),
      readFile(path.join(imageDirectory, 'social-card.png')),
    ]);
    assert.deepEqual(
      committed,
      rendered,
      'website/static/img/social-card.png is stale; run npm run render:brand',
    );
  } finally {
    await rm(temporaryDirectory, {recursive: true, force: true});
  }
});

test('favicon remains legible at representative raster sizes', async () => {
  const favicon = path.join(imageDirectory, 'favicon.svg');
  for (const size of [16, 32, 64]) {
    const {data, info} = await sharp(favicon).resize(size, size).raw().toBuffer({resolveWithObject: true});
    assert.equal(info.width, size);
    assert.equal(info.height, size);
    const brassPixels = Array.from({length: size * size}, (_, index) => index)
      .filter((index) => {
        const offset = index * info.channels;
        return data[offset] > 170 && data[offset + 1] > 100 && data[offset + 1] < 190;
      }).length;
    assert.ok(brassPixels >= size, `favicon loses its brass J at ${size}px`);
  }
});

test('social card uses generated Manrope outlines and requires its local font asset', async () => {
  const socialCard = await readFile(path.join(imageDirectory, 'social-card.svg'), 'utf8');
  assert.doesNotMatch(socialCard, /<text\b/);
  assert.match(socialCard, /<!-- manrope-tagline:start -->/);
  assert.match(socialCard, /<!-- manrope-tagline:end -->/);

  const manropePackage = path.join(websiteDirectory, 'node_modules/@fontsource-variable/manrope');
  const license = await readFile(path.join(manropePackage, 'LICENSE'), 'utf8');
  assert.match(license, /SIL OPEN FONT LICENSE Version 1\.1/);
  const {synchronizeSocialCardSource} = await import('./render-social-card.mjs');
  assert.equal(await synchronizeSocialCardSource(socialCard), socialCard);

  const missingFont = path.join(websiteDirectory, 'static/fonts/missing-manrope.woff2');
  const result = spawnSync(process.execPath, [rendererPath], {
    cwd: websiteDirectory,
    encoding: 'utf8',
    env: {...process.env, JAZZ_BRAND_FONT_PATH: missingFont},
  });
  assert.notEqual(result.status, 0, 'renderer ignored the requested local font asset');
  assert.match(result.stderr, /missing-manrope\.woff2/);
});
