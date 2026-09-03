import {existsSync, readdirSync, readFileSync} from 'node:fs';
import path from 'node:path';

const SITE_ROOT = 'https://un3qual.github.io/jazz/';
const SITE_ROOT_PATH = new URL(SITE_ROOT).pathname;
const buildRoot = path.resolve(
  process.argv[2] ?? path.join(import.meta.dirname, '..', 'build'),
);

function filesIn(directory) {
  if (!existsSync(directory)) {
    return [];
  }
  return readdirSync(directory, {withFileTypes: true}).flatMap((entry) => {
    const entryPath = path.join(directory, entry.name);
    return entry.isDirectory() ? filesIn(entryPath) : [entryPath];
  });
}

function decodeHtmlReferences(value) {
  const namedReferences = {
    amp: '&',
    apos: "'",
    gt: '>',
    lt: '<',
    quot: '"',
  };
  return value.replace(
    /&(?:#(\d+)|#x([\da-f]+)|(amp|apos|gt|lt|quot));/gi,
    (reference, decimal, hexadecimal, name) => {
      if (name) {
        return namedReferences[name.toLowerCase()];
      }
      const codePoint = Number.parseInt(decimal ?? hexadecimal, decimal ? 10 : 16);
      return codePoint > 0
        && codePoint <= 0x10ffff
        && !(codePoint >= 0xd800 && codePoint <= 0xdfff)
        ? String.fromCodePoint(codePoint)
        : '\ufffd';
    },
  );
}

function attributes(tag) {
  return Object.fromEntries(
    [...tag.matchAll(/([:\w-]+)\s*=\s*(?:"([^"]*)"|'([^']*)')/g)].map(
      ([, name, doubleQuoted, singleQuoted]) => [
        name.toLowerCase(),
        decodeHtmlReferences(doubleQuoted ?? singleQuoted),
      ],
    ),
  );
}

function metadataValue(source, key) {
  for (const match of source.matchAll(/<meta\b[^>]*>/gi)) {
    const attrs = attributes(match[0]);
    if (attrs.name === key || attrs.property === key) {
      return attrs.content;
    }
  }
  return undefined;
}

function canonicalUrl(source) {
  for (const match of source.matchAll(/<link\b[^>]*>/gi)) {
    const attrs = attributes(match[0]);
    if (attrs.rel === 'canonical') {
      return attrs.href;
    }
  }
  return undefined;
}

function isNoIndex(source) {
  return /(?:^|[,\s])noindex(?:$|[,\s])/i.test(metadataValue(source, 'robots') ?? '');
}

function title(source) {
  const value = source.match(/<title\b[^>]*>([^<]*)<\/title>/i)?.[1];
  return value ? decodeHtmlReferences(value).trim() : undefined;
}

function structuredData(source, violations, label) {
  const values = [];
  const pattern = /<script\b([^>]*)>([\s\S]*?)<\/script\b[^>]*>/gi;
  for (const match of source.matchAll(pattern)) {
    if (attributes(match[1]).type !== 'application/ld+json') {
      continue;
    }
    try {
      values.push(JSON.parse(match[2]));
    } catch {
      violations.push(`${label}: contains malformed JSON-LD`);
    }
  }
  return values.flatMap((value) => (Array.isArray(value) ? value : [value]));
}

function isDateOnly(value) {
  if (!/^\d{4}-\d{2}-\d{2}$/.test(value ?? '')) {
    return false;
  }
  const parsed = new Date(`${value}T00:00:00Z`);
  return !Number.isNaN(parsed.valueOf()) && parsed.toISOString().slice(0, 10) === value;
}

function sitemapItems(source, violations) {
  const items = [];
  for (const match of source.matchAll(/<url>([\s\S]*?)<\/url>/gi)) {
    const block = match[1];
    const loc = block.match(/<loc>([^<]+)<\/loc>/i)?.[1];
    const lastmod = block.match(/<lastmod>([^<]+)<\/lastmod>/i)?.[1];
    if (!loc) {
      violations.push('sitemap.xml: contains a URL without loc');
      continue;
    }
    if (!isDateOnly(lastmod)) {
      violations.push(`${loc}: sitemap entry is missing a date-only lastmod`);
    }
    items.push({loc, lastmod});
  }
  if (items.length === 0) {
    violations.push('sitemap.xml: contains no indexable URLs');
  }
  return items;
}

function expectedHtmlFile(url, violations) {
  if (!url.startsWith(SITE_ROOT)) {
    violations.push(`sitemap.xml: URL is outside the canonical site: ${url}`);
    return undefined;
  }
  let route;
  try {
    route = decodeURIComponent(new URL(url).pathname.slice(SITE_ROOT_PATH.length));
  } catch {
    violations.push(`sitemap.xml: URL contains a malformed percent escape: ${url}`);
    return undefined;
  }
  route = route.replace(/\/$/, '');
  return path.join(buildRoot, route ? `${route}.html` : 'index.html');
}

function checkPage(file, expectedUrl, violations) {
  const relative = path.relative(buildRoot, file).split(path.sep).join('/');
  if (!existsSync(file)) {
    violations.push(`${expectedUrl}: sitemap target is missing generated HTML`);
    return;
  }
  const source = readFileSync(file, 'utf8');
  if (isNoIndex(source)) {
    violations.push(`${relative}: noindex page must not appear in sitemap.xml`);
  }
  const pageTitle = title(source);
  const description = metadataValue(source, 'description');
  const canonical = canonicalUrl(source);
  const requiredMetadata = [
    'og:title',
    'og:description',
    'og:url',
    'og:image',
    'og:type',
    'og:site_name',
    'twitter:card',
    'twitter:image',
  ];

  if (!pageTitle || pageTitle.length > 70) {
    violations.push(`${relative}: title must contain 1-70 characters`);
  }
  if (!description || description.length > 160) {
    violations.push(`${relative}: description must contain 1-160 characters`);
  }
  if (canonical !== expectedUrl) {
    violations.push(`${relative}: canonical URL does not match ${expectedUrl}`);
  }
  for (const key of requiredMetadata) {
    if (!metadataValue(source, key)) {
      violations.push(`${relative}: missing ${key} metadata`);
    }
  }
  if (metadataValue(source, 'og:url') !== expectedUrl) {
    violations.push(`${relative}: og:url does not match the canonical URL`);
  }
  const requiredType = expectedUrl === SITE_ROOT ? 'WebSite' : 'TechArticle';
  const expectedOpenGraphType = requiredType === 'WebSite' ? 'website' : 'article';
  if (metadataValue(source, 'og:type') !== expectedOpenGraphType) {
    violations.push(`${relative}: og:type must be ${expectedOpenGraphType}`);
  }
  if ((source.match(/<h1\b/gi) ?? []).length !== 1) {
    violations.push(`${relative}: indexable pages must contain exactly one h1`);
  }

  const schema = structuredData(source, violations, relative).find(
    (value) => value?.['@type'] === requiredType,
  );
  if (!schema) {
    violations.push(`${relative}: missing ${requiredType} structured data`);
    return;
  }
  if (schema.url !== expectedUrl) {
    violations.push(`${relative}: ${requiredType} URL does not match the canonical URL`);
  }
  if (schema.description !== description) {
    violations.push(`${relative}: ${requiredType} description does not match page metadata`);
  }
  if (schema.inLanguage !== 'en') {
    violations.push(`${relative}: ${requiredType} language must be en`);
  }
  if (requiredType === 'TechArticle' && !schema.headline) {
    violations.push(`${relative}: TechArticle structured data is missing a headline`);
  }
}

const violations = [];
if (!existsSync(buildRoot)) {
  violations.push(`${buildRoot}: build directory is missing`);
}

const sitemapFile = path.join(buildRoot, 'sitemap.xml');
const items = existsSync(sitemapFile)
  ? sitemapItems(readFileSync(sitemapFile, 'utf8'), violations)
  : [];
if (!existsSync(sitemapFile)) {
  violations.push('sitemap.xml: file is missing');
}

const sitemapUrls = new Set(items.map(({loc}) => loc));
for (const {loc} of items) {
  const file = expectedHtmlFile(loc, violations);
  if (file) {
    checkPage(file, loc, violations);
  }
}

for (const file of filesIn(buildRoot).filter((entry) => entry.endsWith('.html'))) {
  const relative = path.relative(buildRoot, file).split(path.sep).join('/');
  const source = readFileSync(file, 'utf8');
  if (
    relative === '404.html'
    || /<meta\b[^>]*http-equiv=["']refresh["']/i.test(source)
    || isNoIndex(source)
  ) {
    continue;
  }
  const canonical = canonicalUrl(source);
  if (!canonical) {
    violations.push(`${relative}: indexable page is missing a canonical URL`);
  } else if (!sitemapUrls.has(canonical)) {
    violations.push(`${relative}: canonical URL is missing from sitemap.xml`);
  }
}

if (violations.length > 0) {
  console.error('SEO checks failed:');
  for (const violation of [...new Set(violations)].sort()) {
    console.error(`- ${violation}`);
  }
  process.exitCode = 1;
} else {
  console.log(`SEO checks passed (${items.length} indexable pages).`);
}
