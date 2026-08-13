import {existsSync, readdirSync, readFileSync, statSync} from 'node:fs';
import path from 'node:path';

const buildRoot = path.resolve(
  process.argv[2] ?? path.join(import.meta.dirname, '..', 'build'),
);
const pagefindRoot = path.join(buildRoot, 'pagefind');

function filesIn(directory) {
  if (!existsSync(directory)) {
    return [];
  }

  return readdirSync(directory, {withFileTypes: true}).flatMap((entry) => {
    const entryPath = path.join(directory, entry.name);
    return entry.isDirectory() ? filesIn(entryPath) : [entryPath];
  });
}

function requireNonEmptyFile(file, description) {
  if (!existsSync(file) || !statSync(file).isFile() || statSync(file).size === 0) {
    throw new Error(`production Pagefind index is missing ${description}: ${file}`);
  }
}

function requireNonEmptyFiles(directory, description) {
  const files = filesIn(directory).filter((file) => statSync(file).size > 0);
  if (files.length === 0) {
    throw new Error(`production Pagefind index has no ${description}`);
  }
  return files;
}

requireNonEmptyFile(path.join(pagefindRoot, 'pagefind.js'), 'browser runtime');

const wasmFiles = filesIn(pagefindRoot).filter(
  (file) =>
    path.basename(file).startsWith('wasm.') &&
    file.endsWith('.pagefind') &&
    statSync(file).size > 0,
);
if (wasmFiles.length === 0) {
  throw new Error('production Pagefind index is missing a non-empty WASM runtime');
}

const metadata = filesIn(pagefindRoot).filter(
  (file) => file.endsWith('.pf_meta') && statSync(file).size > 0,
);
if (metadata.length === 0) {
  throw new Error('production Pagefind index has no non-empty metadata files');
}
const fragments = requireNonEmptyFiles(
  path.join(pagefindRoot, 'fragment'),
  'non-empty index fragments',
);

const entry = path.join(pagefindRoot, 'pagefind-entry.json');
requireNonEmptyFile(entry, 'page metadata');
const pageMetadata = JSON.parse(readFileSync(entry, 'utf8'));
const indexedPages = Object.values(pageMetadata.languages ?? {}).reduce(
  (total, language) => total + (language.page_count ?? 0),
  0,
);
if (indexedPages === 0) {
  throw new Error('production Pagefind index reports no indexed documentation pages');
}

console.log(
  `Pagefind search check passed (${indexedPages} pages, ${fragments.length} fragments).`,
);
