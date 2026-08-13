import assert from 'node:assert/strict';
import {execFileSync} from 'node:child_process';
import {mkdtempSync, mkdirSync, rmSync, writeFileSync} from 'node:fs';
import {tmpdir} from 'node:os';
import path from 'node:path';
import test from 'node:test';

const checker = path.join(import.meta.dirname, 'check-built-search.mjs');

function buildFixture(setup) {
  const buildRoot = mkdtempSync(path.join(tmpdir(), 'jazz-pagefind-build-'));
  setup(buildRoot);
  return buildRoot;
}

function writeGeneratedIndex(buildRoot, {runtime = true, wasm = true, fragment = 'index'} = {}) {
  const pagefindRoot = path.join(buildRoot, 'pagefind');
  mkdirSync(path.join(pagefindRoot, 'fragment'), {recursive: true});
  writeFileSync(
    path.join(pagefindRoot, 'pagefind-entry.json'),
    JSON.stringify({
      version: '1.5.2',
      languages: {en: {hash: 'docs', wasm: 'en', page_count: 1}},
    }),
  );
  writeFileSync(path.join(pagefindRoot, 'pagefind.en_docs.pf_meta'), 'metadata');
  if (runtime) {
    writeFileSync(path.join(pagefindRoot, 'pagefind.js'), 'export {}');
  }
  if (wasm) {
    writeFileSync(path.join(pagefindRoot, 'wasm.en.pagefind'), 'wasm');
  }
  writeFileSync(path.join(pagefindRoot, 'fragment', 'docs.pf_fragment'), fragment);
}

function runChecker(buildRoot) {
  try {
    execFileSync(process.execPath, [checker, buildRoot], {encoding: 'utf8'});
    return {ok: true, output: ''};
  } catch (error) {
    return {
      ok: false,
      output: `${error.stdout ?? ''}${error.stderr ?? ''}`,
    };
  }
}

test('search artifact checker rejects a missing browser runtime', () => {
  const buildRoot = buildFixture((directory) =>
    writeGeneratedIndex(directory, {runtime: false}),
  );
  try {
    const result = runChecker(buildRoot);
    assert.equal(result.ok, false);
    assert.match(result.output, /pagefind\.js/);
  } finally {
    rmSync(buildRoot, {recursive: true, force: true});
  }
});

test('search artifact checker rejects a missing WebAssembly runtime', () => {
  const buildRoot = buildFixture((directory) =>
    writeGeneratedIndex(directory, {wasm: false}),
  );
  try {
    const result = runChecker(buildRoot);
    assert.equal(result.ok, false);
    assert.match(result.output, /WASM/);
  } finally {
    rmSync(buildRoot, {recursive: true, force: true});
  }
});

test('search artifact checker rejects empty Pagefind index fragments', () => {
  const buildRoot = buildFixture((directory) =>
    writeGeneratedIndex(directory, {fragment: ''}),
  );
  try {
    const result = runChecker(buildRoot);
    assert.equal(result.ok, false);
    assert.match(result.output, /fragment/i);
  } finally {
    rmSync(buildRoot, {recursive: true, force: true});
  }
});

test('search artifact checker accepts a generated documentation index', () => {
  const buildRoot = buildFixture((directory) => writeGeneratedIndex(directory));
  try {
    const result = runChecker(buildRoot);
    assert.equal(result.ok, true, result.output);
  } finally {
    rmSync(buildRoot, {recursive: true, force: true});
  }
});
