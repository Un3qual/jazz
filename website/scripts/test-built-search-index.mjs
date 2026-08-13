import assert from 'node:assert/strict';
import {copyFileSync, mkdtempSync, readFileSync, rmSync, statSync} from 'node:fs';
import {createServer} from 'node:http';
import {tmpdir} from 'node:os';
import path from 'node:path';
import {pathToFileURL} from 'node:url';
import test from 'node:test';

const websiteRoot = path.resolve(import.meta.dirname, '..');

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
  return new Promise((resolve, reject) =>
    server.close((error) => error ? reject(error) : resolve()),
  );
}

test('production Pagefind index includes document content and excludes Docusaurus shell text', async () => {
  // Mutation caught: removing data-pagefind-body from the DocItem layout causes
  // Pagefind to index shell text such as the skip-navigation link.
  const fixture = mkdtempSync(path.join(tmpdir(), 'jazz-pagefind-runtime-'));
  const buildRoot = path.join(websiteRoot, 'build');
  const modulePath = path.join(fixture, 'pagefind.mjs');
  copyFileSync(path.join(buildRoot, 'pagefind', 'pagefind.js'), modulePath);
  const {server, origin} = await serveBuild(buildRoot);
  let index;
  try {
    const pagefind = await import(`${pathToFileURL(modulePath).href}?scope-test`);
    index = pagefind.createInstance({basePath: `${origin}/pagefind/`});
    const documentResults = await index.search('maybeMap');
    const shellResults = await index.search('Skip to main content');

    assert.ok(documentResults.results.length > 0, 'document content was not indexed');
    assert.equal(shellResults.results.length, 0, 'shell text was indexed');
  } finally {
    try {
      await index?.destroy();
    } finally {
      await closeServer(server);
      rmSync(fixture, {recursive: true, force: true});
    }
  }
});
