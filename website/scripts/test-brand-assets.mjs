import assert from 'node:assert/strict';
import {mkdtemp, readFile, rm, writeFile} from 'node:fs/promises';
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
const bellhookCenterline = 'M106 112C153 88 187 124 222 111C258 97 274 64 323 64C368 64 393 90 393 138V302C393 375 351 417 288 417C228 417 190 382 190 327V295';
const wordmarkPaths = [
  'M535 92H615V300C615 362 577 396 519 396C499 396 481 392 466 386L480 326C491 330 502 332 513 332C528 332 535 321 535 300Z',
  'M635 390L710 92H780L857 390H783L771 337H702L690 390ZM715 278H758L737 183Z',
  'M865 92H1008V154L939 327H1011V390H857V329L929 157H865Z',
  'M1018 92H1161V154L1092 327H1164V390H1010V329L1082 157H1018Z',
];
const vectorSurfaceColors = new Map([
  ['jazz-mark.svg', '#24182C'],
  ['jazz-mark-dark.svg', '#F3EDDF'],
  ['jazz-wordmark.svg', '#24182C'],
  ['jazz-wordmark-dark.svg', '#F3EDDF'],
  ['favicon.svg', '#F3EDDF'],
  ['social-card.svg', '#F3EDDF'],
]);
const wordmarkContracts = new Map([
  ['jazz-wordmark.svg', {
    surfaceColor: '#24182C',
    width: 1200,
    height: 480,
    wrapperTransform: null,
  }],
  ['jazz-wordmark-dark.svg', {
    surfaceColor: '#F3EDDF',
    width: 1200,
    height: 480,
    wrapperTransform: null,
  }],
  ['social-card.svg', {
    surfaceColor: '#F3EDDF',
    width: 1200,
    height: 630,
    wrapperTransform: 'translate(108 90) scale(.82)',
  }],
]);

function wordmarkGroupParts(source, assetName) {
  const groupMatches = [
    ...source.matchAll(/<g\b([^>]*\bdata-role="wordmark"[^>]*)>([\s\S]*?)<\/g>/g),
  ];
  assert.equal(
    groupMatches.length,
    1,
    `${assetName} must expose exactly one rendered wordmark group`,
  );
  const [, attributes, contents] = groupMatches[0];
  return {attributes, contents};
}

function assertWordmarkGroupAttributes(attributes, assetName, surfaceColor) {
  assert.match(
    attributes,
    new RegExp(`\\bfill="${surfaceColor}"`),
    `${assetName} uses the wrong wordmark surface color`,
  );
  assert.match(
    attributes,
    /\btransform="translate\(-39 0\)"/,
    `${assetName} must keep the approved wordmark placement`,
  );
  assert.doesNotMatch(
    attributes,
    /\b(?:display|visibility|opacity|style|filter|mask|clip-path)=/,
    `${assetName} must not hide or restyle its wordmark group`,
  );
}

function wordmarkPathAttributes(contents, assetName) {
  const pathElements = [...contents.matchAll(/<path\b([^>]*)\/>/g)];
  assert.equal(
    contents.replace(/<path\b[^>]*\/>/g, '').trim(),
    '',
    `${assetName} wordmark group must contain only approved letter paths`,
  );
  assert.equal(
    pathElements.length,
    wordmarkPaths.length,
    `${assetName} must render exactly four wordmark letters`,
  );
  return pathElements.map(([, attributes]) => attributes);
}

function assertApprovedWordmarkPath(source, assetName, pathAttributes, wordmarkPath) {
  assert.equal(
    source.split(wordmarkPath).length - 1,
    1,
    `${assetName} must contain each approved letter path exactly once`,
  );
  const matchingPaths = pathAttributes.filter((attributes) =>
    attributes.includes(`d="${wordmarkPath}"`),
  );
  assert.equal(
    matchingPaths.length,
    1,
    `${assetName} must render each approved letter path exactly once`,
  );
  assert.doesNotMatch(
    matchingPaths[0],
    /\b(?:fill|stroke|transform|display|visibility|opacity|style|filter|mask|clip-path)=/,
    `${assetName} letter paths must inherit the visible wordmark treatment`,
  );
}

function assertApprovedWordmarkGroup(source, assetName, surfaceColor) {
  const {attributes, contents} = wordmarkGroupParts(source, assetName);
  assertWordmarkGroupAttributes(attributes, assetName, surfaceColor);
  const pathAttributes = wordmarkPathAttributes(contents, assetName);
  for (const wordmarkPath of wordmarkPaths) {
    assertApprovedWordmarkPath(source, assetName, pathAttributes, wordmarkPath);
  }
}

async function assertRenderedWordmark(source, assetName, contract) {
  const pathSource = wordmarkPaths
    .map((wordmarkPath, index) =>
      `<path d="${wordmarkPath}"${index === 1 ? ' fill-rule="evenodd"' : ''}/>`)
    .join('');
  const wordmarkGroup =
    `<g transform="translate(-39 0)" fill="${contract.surfaceColor}">${pathSource}</g>`;
  const referenceContents = contract.wrapperTransform === null
    ? wordmarkGroup
    : `<g transform="${contract.wrapperTransform}">${wordmarkGroup}</g>`;
  const referenceSvg = Buffer.from(
    `<svg xmlns="http://www.w3.org/2000/svg" width="${contract.width}" height="${contract.height}" viewBox="0 0 ${contract.width} ${contract.height}">${referenceContents}</svg>`,
  );
  const [reference, rendered] = await Promise.all([
    sharp(referenceSvg).ensureAlpha().raw().toBuffer({resolveWithObject: true}),
    sharp(Buffer.from(source)).ensureAlpha().raw().toBuffer({resolveWithObject: true}),
  ]);
  assert.equal(rendered.info.width, contract.width);
  assert.equal(rendered.info.height, contract.height);

  const expectedColor = [
    Number.parseInt(contract.surfaceColor.slice(1, 3), 16),
    Number.parseInt(contract.surfaceColor.slice(3, 5), 16),
    Number.parseInt(contract.surfaceColor.slice(5, 7), 16),
  ];
  let opaqueReferencePixels = 0;
  let mismatchedPixels = 0;
  for (let offset = 0; offset < reference.data.length; offset += reference.info.channels) {
    if (reference.data[offset + 3] !== 255) continue;
    opaqueReferencePixels += 1;
    if (
      rendered.data[offset] !== expectedColor[0]
      || rendered.data[offset + 1] !== expectedColor[1]
      || rendered.data[offset + 2] !== expectedColor[2]
      || rendered.data[offset + 3] !== 255
    ) {
      mismatchedPixels += 1;
    }
  }
  assert.ok(opaqueReferencePixels > 80000, `${assetName} reference wordmark is incomplete`);
  assert.equal(
    mismatchedPixels,
    0,
    `${assetName} does not visibly render the approved wordmark geometry and color`,
  );
}

async function copySynchronizedSocialCard(renderer, directory) {
  const committedPath = path.join(imageDirectory, 'social-card.svg');
  const source = await readFile(committedPath, 'utf8');
  assert.equal(
    await renderer.synchronizeSocialCardSource(source),
    source,
    'website/static/img/social-card.svg is stale; run npm run render:brand',
  );
  const copiedPath = path.join(directory, 'social-card.svg');
  await writeFile(copiedPath, source, 'utf8');
  return copiedPath;
}

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

test('every vector logo uses the approved balanced wide-bold Bellhook geometry', async () => {
  for (const [assetName, surfaceColor] of vectorSurfaceColors) {
    const source = await readFile(path.join(imageDirectory, assetName), 'utf8');
    assert.equal(
      source.split(bellhookCenterline).length - 1,
      2,
      `${assetName} must contain the approved centerline exactly twice`,
    );
    assert.match(source, /stroke-width="120"/);
    assert.match(source, /stroke-width="72"/);
    assert.match(source, /stop-color="#FFE66A"/);
    assert.match(source, /stop-color="#FFC43D"/);
    assert.match(source, /stop-color="#F47A32"/);
    assert.ok(
      source.includes(
        `d="${bellhookCenterline}" fill="none" stroke="${surfaceColor}" stroke-width="120"`,
      ),
      `${assetName} uses the wrong surface-specific Bellhook edge`,
    );
    assert.doesNotMatch(source, /stroke-width="38"|#D49A35/);
    assert.doesNotMatch(
      source,
      /<(?:filter|mask|image|script|style|symbol|text|use)\b|href=|\b(?:display|visibility|opacity|style|filter|mask|clip-path)=/,
    );
  }
});

test('wordmark surfaces render the approved matched Jazz lettering', async () => {
  for (const [assetName, contract] of wordmarkContracts) {
    const source = await readFile(path.join(imageDirectory, assetName), 'utf8');
    assertApprovedWordmarkGroup(source, assetName, contract.surfaceColor);
    await assertRenderedWordmark(source, assetName, contract);
  }
});

test('wordmark validation rejects extra children outside the approved letters', async () => {
  const source = await readFile(path.join(imageDirectory, 'jazz-wordmark.svg'), 'utf8');
  const firstLetter = `<path d="${wordmarkPaths[0]}"/>`;
  const malformedSource = source.replace(
    firstLetter,
    `<circle cx="1180" cy="460" r="8"/>${firstLetter}`,
  );
  assert.notEqual(malformedSource, source, 'wordmark mutation did not apply');
  assert.throws(
    () => assertApprovedWordmarkGroup(malformedSource, 'malformed-wordmark.svg', '#24182C'),
    /must contain only approved letter paths/,
  );
});

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
    const renderer = await import('./render-social-card.mjs');
    const svgPath = await copySynchronizedSocialCard(renderer, temporaryDirectory);
    await Promise.all(
      Array.from(
        {length: 4},
        () => renderer.renderSocialCard({svgPath, pngPath}),
      ),
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
    const renderer = await import('./render-social-card.mjs');
    const svgPath = await copySynchronizedSocialCard(renderer, temporaryDirectory);
    await renderer.renderSocialCard({svgPath, pngPath: renderedPath});
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

test('VS Code icon is a deterministic 128px raster of the canonical Jazz mark', async () => {
  const renderer = await import('./render-social-card.mjs');
  assert.equal(typeof renderer.renderEditorIcon, 'function');

  const temporaryDirectory = await mkdtemp(path.join(os.tmpdir(), 'jazz-editor-icon-'));
  const firstOutput = path.join(temporaryDirectory, 'first.png');
  const secondOutput = path.join(temporaryDirectory, 'second.png');
  try {
    await renderer.renderEditorIcon({pngPath: firstOutput});
    await renderer.renderEditorIcon({pngPath: secondOutput});

    const [firstBytes, secondBytes, checkedInBytes] = await Promise.all([
      readFile(firstOutput),
      readFile(secondOutput),
      readFile(path.resolve(websiteDirectory, '../editors/vscode-jazz/icon.png')),
    ]);
    assert.deepEqual(firstBytes, secondBytes);
    assert.deepEqual(checkedInBytes, firstBytes);

    const metadata = await sharp(firstBytes).metadata();
    assert.equal(metadata.format, 'png');
    assert.equal(metadata.width, 128);
    assert.equal(metadata.height, 128);
    assert.equal(metadata.hasAlpha, true);
  } finally {
    await rm(temporaryDirectory, {recursive: true, force: true});
  }
});

test('concurrent VS Code icon renders isolate their temporary output', async () => {
  const renderer = await import('./render-social-card.mjs');
  const temporaryDirectory = await mkdtemp(
    path.join(os.tmpdir(), 'jazz-editor-icon-concurrency-'),
  );
  const pngPath = path.join(temporaryDirectory, 'icon.png');
  try {
    await Promise.all(
      Array.from({length: 4}, () => renderer.renderEditorIcon({pngPath})),
    );
    const metadata = await sharp(pngPath).metadata();
    assert.equal(metadata.width, 128);
    assert.equal(metadata.height, 128);
  } finally {
    await rm(temporaryDirectory, {recursive: true, force: true});
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
