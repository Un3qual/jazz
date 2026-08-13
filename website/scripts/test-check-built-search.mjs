import assert from 'node:assert/strict';
import {execFileSync} from 'node:child_process';
import {copyFileSync, mkdtempSync, mkdirSync, readFileSync, rmSync, statSync, writeFileSync} from 'node:fs';
import {createServer} from 'node:http';
import {tmpdir} from 'node:os';
import path from 'node:path';
import {pathToFileURL} from 'node:url';
import test from 'node:test';

const checker = path.join(import.meta.dirname, 'check-built-search.mjs');
const websiteRoot = path.resolve(import.meta.dirname, '..');

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

function serveBuild(buildRoot) {
  const server = createServer((request, response) => {
    const pathname = new URL(request.url, 'http://127.0.0.1').pathname;
    const file = path.resolve(buildRoot, `.${pathname}`);
    if (!file.startsWith(`${buildRoot}${path.sep}`) || !statSync(file).isFile()) {
      response.writeHead(404).end();
      return;
    }
    response.writeHead(200).end(readFileSync(file));
  });

  return new Promise((resolve) => {
    server.listen(0, '127.0.0.1', () => {
      const {port} = server.address();
      resolve({server, origin: `http://127.0.0.1:${port}`});
    });
  });
}

function closeServer(server) {
  return new Promise((resolve, reject) => server.close((error) => error ? reject(error) : resolve()));
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

test('production Pagefind index includes document content and excludes Docusaurus shell text', async () => {
  // Mutation caught: removing data-pagefind-body from the DocItem layout causes
  // Pagefind to index shell text such as the skip-navigation link.
  execFileSync('pnpm', ['run', 'build'], {
    cwd: websiteRoot,
    stdio: 'pipe',
  });

  const fixture = mkdtempSync(path.join(tmpdir(), 'jazz-pagefind-runtime-'));
  const buildRoot = path.join(websiteRoot, 'build');
  const modulePath = path.join(fixture, 'pagefind.mjs');
  copyFileSync(path.join(buildRoot, 'pagefind', 'pagefind.js'), modulePath);
  const {server, origin} = await serveBuild(buildRoot);
  try {
    const pagefind = await import(`${pathToFileURL(modulePath).href}?scope-test`);
    const index = pagefind.createInstance({basePath: `${origin}/pagefind/`});
    const documentResults = await index.search('maybeMap');
    const shellResults = await index.search('Skip to main content');

    assert.ok(documentResults.results.length > 0, 'document content was not indexed');
    assert.equal(shellResults.results.length, 0, 'shell text was indexed');
    await index.destroy();
  } finally {
    await closeServer(server);
    rmSync(fixture, {recursive: true, force: true});
  }
});
