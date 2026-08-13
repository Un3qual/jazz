import {existsSync, readdirSync, readFileSync} from 'node:fs';
import path from 'node:path';

import {JAZZ_TYPE_DESTINATIONS} from './jazz-type-links.mjs';

const buildRoot = path.resolve(
  process.argv[2] ?? path.join(import.meta.dirname, '..', 'build'),
);
const baseUrl = '/jazz/';

function htmlFiles(directory) {
  if (!existsSync(directory)) {
    return [];
  }

  return readdirSync(directory, {withFileTypes: true}).flatMap((entry) => {
    const entryPath = path.join(directory, entry.name);
    if (entry.isDirectory()) {
      return htmlFiles(entryPath);
    }
    return entry.isFile() && entry.name.endsWith('.html') ? [entryPath] : [];
  });
}

function attribute(tag, name) {
  const match = tag.match(new RegExp(`\\b${name}="([^"]*)"`));
  return match?.[1];
}

const HTML_CHARACTER_REFERENCES = Object.freeze({
  lt: '<',
  gt: '>',
  amp: '&',
  quot: '"',
  '#x27': "'",
});

function textContent(html) {
  let text = '';
  let index = 0;

  while (index < html.length) {
    if (html[index] === '<') {
      const tagEnd = html.indexOf('>', index + 1);
      if (tagEnd === -1) {
        break;
      }
      index = tagEnd + 1;
      continue;
    }

    if (html[index] === '&') {
      const referenceEnd = html.indexOf(';', index + 1);
      const reference = referenceEnd === -1
        ? undefined
        : html.slice(index + 1, referenceEnd);
      const decoded = reference === undefined
        ? undefined
        : HTML_CHARACTER_REFERENCES[reference];
      if (decoded !== undefined) {
        text += decoded;
        index = referenceEnd + 1;
        continue;
      }
    }

    text += html[index];
    index += 1;
  }

  return text;
}

function signatureBlocks(html) {
  return [
    ...html.matchAll(
      /<pre\b(?=[^>]*\bdata-jazz-highlighter="textmate")(?=[^>]*\bdata-jazz-signature(?:="[^"]*")?)[^>]*>[\s\S]*?<\/pre>/g,
    ),
  ].map(([block]) => block);
}

function ordinaryJazzBlocks(html) {
  return [
    ...html.matchAll(
      /<pre\b(?=[^>]*\bdata-jazz-highlighter="textmate")[^>]*>[\s\S]*?<\/pre>/g,
    ),
  ]
    .map(([block]) => block)
    .filter((block) => !/\bdata-jazz-signature(?:=|\s|>)/.test(block));
}

function typeLinks(html) {
  return [
    ...html.matchAll(
      /(?<tag><a\b(?=[^>]*\bdata-jazz-type-link(?:="[^"]*")?)[^>]*>)(?<contents>[\s\S]*?)<\/a>/g,
    ),
  ].map(({groups}) => ({
    href: attribute(groups.tag, 'href'),
    text: textContent(groups.contents),
  }));
}

function requireRepresentative(links, description, predicate) {
  if (!links.some(predicate)) {
    throw new Error(`production signatures are missing ${description}`);
  }
}

function targetFileFor(href) {
  const url = new URL(href, 'https://jazz.invalid');
  if (!url.pathname.startsWith(baseUrl)) {
    throw new Error(`Jazz type link does not use the ${baseUrl} base URL: ${href}`);
  }

  const route = decodeURIComponent(url.pathname.slice(baseUrl.length))
    .replace(/^\/+|\/+$/g, '');
  const flatTarget = route
    ? path.join(buildRoot, `${route}.html`)
    : path.join(buildRoot, 'index.html');
  const directoryTarget = route
    ? path.join(buildRoot, route, 'index.html')
    : flatTarget;
  const target = existsSync(flatTarget) ? flatTarget : directoryTarget;
  return {target, fragment: decodeURIComponent(url.hash.slice(1))};
}

function requireTarget(href) {
  const {target, fragment} = targetFileFor(href);
  if (!existsSync(target)) {
    throw new Error(`Jazz type link target does not exist: ${href} -> ${target}`);
  }

  if (fragment) {
    const targetHtml = readFileSync(target, 'utf8');
    const hasFragment = [...targetHtml.matchAll(/\bid="([^"]+)"/g)].some(
      (match) => match[1] === fragment,
    );
    if (!hasFragment) {
      throw new Error(`Jazz type link fragment does not exist: ${href}`);
    }
  }
}

const allFiles = htmlFiles(buildRoot);
const standardLibraryFiles = htmlFiles(
  path.join(buildRoot, 'docs', 'standard-library'),
);
if (standardLibraryFiles.length === 0) {
  throw new Error('production build contains no standard-library HTML');
}

const standardLibraryHtml = standardLibraryFiles.map((file) =>
  readFileSync(file, 'utf8'),
);
const signatureHtml = standardLibraryHtml.flatMap(signatureBlocks);
if (signatureHtml.length === 0) {
  throw new Error('production standard library contains no Jazz signatures');
}

const links = signatureHtml.flatMap(typeLinks);
if (links.length === 0) {
  throw new Error('production Jazz signatures contain no mapped type links');
}

const allTypeLinkCount = allFiles
  .map((file) => readFileSync(file, 'utf8'))
  .flatMap(typeLinks).length;
if (allTypeLinkCount !== links.length) {
  throw new Error('a Jazz type link appears outside a standard-library signature block');
}

for (const {href, text} of links) {
  if (!href) {
    throw new Error(`Jazz type link has no href: ${text}`);
  }
  requireTarget(href);
}

for (const destination of new Set(Object.values(JAZZ_TYPE_DESTINATIONS))) {
  requireTarget(`${baseUrl}${destination.replace(/^\/+/, '')}`);
}

const moduleTypes = new Set([
  'Maybe',
  'Result',
  'NonEmpty',
  'Dictionary',
  'Queue',
  'Map',
  'Set',
  'Char',
  'Text',
  'List',
  'IOError',
]);
const builtins = new Set([
  'Bool',
  'Int',
  'Int8',
  'Int16',
  'Int32',
  'Int64',
  'UInt8',
  'UInt16',
  'UInt32',
  'UInt64',
  'Float',
  'Float16',
  'Float32',
  'Float64',
]);
const capabilities = new Set([
  'Eq',
  'Ord',
  'Num',
  'Integral',
  'Fractional',
  'Showable',
  'Default',
]);

requireRepresentative(
  links,
  'a linked module type',
  ({text}) => moduleTypes.has(text),
);
requireRepresentative(
  links,
  'a linked built-in type',
  ({text}) => builtins.has(text),
);
requireRepresentative(
  links,
  'a linked capability',
  ({text}) => capabilities.has(text),
);
requireRepresentative(links, 'linked list syntax', ({text}) => text === '[');
requireRepresentative(links, 'linked list syntax', ({text}) => text === ']');
requireRepresentative(links, 'linked tuple syntax', ({text}) => text === '(');
requireRepresentative(links, 'linked tuple syntax', ({text}) => text === ')');
requireRepresentative(links, 'linked unit syntax', ({text}) => text === '()');

const hasNestedSignature = signatureHtml.some((block) => {
  const texts = new Set(typeLinks(block).map(({text}) => text));
  return texts.has('Maybe') && texts.has('[') && texts.has(']');
});
if (!hasNestedSignature) {
  throw new Error('production signatures are missing nested linked type syntax');
}

const ordinaryBlocks = allFiles
  .map((file) => readFileSync(file, 'utf8'))
  .flatMap(ordinaryJazzBlocks);
if (ordinaryBlocks.length === 0) {
  throw new Error('production build contains no ordinary TextMate Jazz example');
}
if (ordinaryBlocks.some((block) => typeLinks(block).length > 0)) {
  throw new Error('an ordinary Jazz example contains a type link');
}

console.log(
  `Jazz type-link check passed (${signatureHtml.length} signatures, ${links.length} links, ${ordinaryBlocks.length} ordinary examples).`,
);
