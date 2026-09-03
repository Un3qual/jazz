import assert from 'node:assert/strict';
import {execFileSync} from 'node:child_process';
import {mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync} from 'node:fs';
import {tmpdir} from 'node:os';
import path from 'node:path';
import test from 'node:test';

const checker = path.join(import.meta.dirname, 'check-built-seo.mjs');
const siteRoot = 'https://un3qual.github.io/jazz/';

function write(buildRoot, relativePath, contents) {
  const file = path.join(buildRoot, relativePath);
  mkdirSync(path.dirname(file), {recursive: true});
  writeFileSync(file, contents);
}

function structuredData(type, url, title, description) {
  const data = type === 'WebSite'
    ? {
        '@context': 'https://schema.org',
        '@type': 'WebSite',
        name: 'Jazz programming language',
        url,
        description,
        inLanguage: 'en',
      }
    : {
        '@context': 'https://schema.org',
        '@type': 'TechArticle',
        headline: title,
        url,
        description,
        inLanguage: 'en',
      };
  return `<script type="application/ld+json">${JSON.stringify(data)}</script>`;
}

function pageHtml({url, title, description, schemaType}) {
  return `<!doctype html><html lang="en"><head>
    <title>${title}</title>
    <meta name="description" content="${description}">
    <meta property="og:title" content="${title}">
    <meta property="og:description" content="${description}">
    <meta property="og:url" content="${url}">
    <meta property="og:image" content="${siteRoot}img/social-card.png">
    <meta property="og:type" content="${schemaType === 'WebSite' ? 'website' : 'article'}">
    <meta property="og:site_name" content="Jazz programming language">
    <meta name="twitter:card" content="summary_large_image">
    <meta name="twitter:image" content="${siteRoot}img/social-card.png">
    <link rel="canonical" href="${url}">
    ${structuredData(schemaType, url, title, description)}
  </head><body><main><h1>${title}</h1></main></body></html>`;
}

function completeBuildFixture() {
  const buildRoot = mkdtempSync(path.join(tmpdir(), 'jazz-seo-build-'));
  const homepageDescription =
    'Jazz is an experimental, statically typed functional programming language.';
  const guideDescription =
    'Learn Jazz functions, immutable values, algebraic data types, and pattern matching.';
  write(
    buildRoot,
    'sitemap.xml',
    `<?xml version="1.0" encoding="UTF-8"?><urlset>
      <url><loc>${siteRoot}</loc><lastmod>2026-09-01</lastmod></url>
      <url><loc>${siteRoot}docs/language/overview</loc><lastmod>2026-09-02</lastmod></url>
    </urlset>`,
  );
  write(
    buildRoot,
    'index.html',
    pageHtml({
      url: siteRoot,
      title: 'Statically typed functional programming language · Jazz',
      description: homepageDescription,
      schemaType: 'WebSite',
    }),
  );
  write(
    buildRoot,
    'docs/language/overview.html',
    pageHtml({
      url: `${siteRoot}docs/language/overview`,
      title: 'Jazz programming language overview · Jazz',
      description: guideDescription,
      schemaType: 'TechArticle',
    }),
  );
  write(
    buildRoot,
    '404.html',
    '<!doctype html><html><head><title>Page Not Found</title></head></html>',
  );
  write(
    buildRoot,
    'old/index.html',
    '<!doctype html><meta http-equiv="refresh" content="0; url=/jazz/">',
  );
  return buildRoot;
}

function runChecker(buildRoot) {
  try {
    const output = execFileSync(process.execPath, [checker, buildRoot], {
      encoding: 'utf8',
      stdio: 'pipe',
    });
    return {ok: true, output};
  } catch (error) {
    return {
      ok: false,
      output: `${error.stdout ?? ''}${error.stderr ?? ''}`,
    };
  }
}

function withFixture(run) {
  const buildRoot = completeBuildFixture();
  try {
    run(buildRoot);
  } finally {
    rmSync(buildRoot, {recursive: true, force: true});
  }
}

test('SEO artifact checker accepts complete indexable pages and ignores utility pages', () => {
  withFixture((buildRoot) => {
    const result = runChecker(buildRoot);
    assert.equal(result.ok, true, result.output);
    assert.match(result.output, /2 indexable pages/);
  });
});

test('SEO artifact checker rejects missing, malformed, and impossible dates', () => {
  for (const replacement of [
    '',
    '<lastmod>not-a-date</lastmod>',
    '<lastmod>2026-02-31</lastmod>',
  ]) {
    withFixture((buildRoot) => {
      const sitemap = readFileSync(path.join(buildRoot, 'sitemap.xml'), 'utf8');
      write(
        buildRoot,
        'sitemap.xml',
        sitemap.replace(/<lastmod>[^<]+<\/lastmod>/, replacement),
      );
      const result = runChecker(buildRoot);
      assert.equal(result.ok, false, `${replacement}: ${result.output}`);
      assert.match(result.output, /lastmod/i);
    });
  }
});

test('SEO artifact checker rejects an indexable page missing from the sitemap', () => {
  withFixture((buildRoot) => {
    write(
      buildRoot,
      'extra.html',
      pageHtml({
        url: `${siteRoot}extra`,
        title: 'Extra Jazz guide · Jazz',
        description: 'Learn another documented part of the Jazz programming language.',
        schemaType: 'TechArticle',
      }),
    );
    const result = runChecker(buildRoot);
    assert.equal(result.ok, false);
    assert.match(result.output, /extra.+sitemap/i);
  });
});

test('SEO artifact checker excludes noindex pages from sitemap coverage', () => {
  withFixture((buildRoot) => {
    write(
      buildRoot,
      'private.html',
      pageHtml({
        url: `${siteRoot}private`,
        title: 'Private Jazz guide · Jazz',
        description: 'A deliberately non-indexable Jazz documentation page.',
        schemaType: 'TechArticle',
      }).replace('<head>', '<head><meta name="robots" content="noindex, nofollow">'),
    );

    const result = runChecker(buildRoot);
    assert.equal(result.ok, true, result.output);
  });
});

test('SEO artifact checker rejects noindex pages listed in the sitemap', () => {
  withFixture((buildRoot) => {
    const sitemap = readFileSync(path.join(buildRoot, 'sitemap.xml'), 'utf8');
    write(
      buildRoot,
      'sitemap.xml',
      sitemap.replace(
        '</urlset>',
        `<url><loc>${siteRoot}private</loc><lastmod>2026-09-02</lastmod></url></urlset>`,
      ),
    );
    write(
      buildRoot,
      'private.html',
      pageHtml({
        url: `${siteRoot}private`,
        title: 'Private Jazz guide · Jazz',
        description: 'A deliberately non-indexable Jazz documentation page.',
        schemaType: 'TechArticle',
      }).replace('<head>', '<head><meta name="robots" content="noindex, nofollow">'),
    );

    const result = runChecker(buildRoot);
    assert.equal(result.ok, false);
    assert.match(result.output, /private\.html.+noindex.+sitemap/i);
  });
});

test('SEO artifact checker rejects incomplete canonical and social metadata', () => {
  withFixture((buildRoot) => {
    const homepage = readFileSync(path.join(buildRoot, 'index.html'), 'utf8');
    write(
      buildRoot,
      'index.html',
      homepage.replace(/<meta property="og:description"[^>]+>/, ''),
    );
    const result = runChecker(buildRoot);
    assert.equal(result.ok, false);
    assert.match(result.output, /og:description/);
  });
});

test('SEO artifact checker rejects social metadata without a site identity', () => {
  withFixture((buildRoot) => {
    const homepage = readFileSync(path.join(buildRoot, 'index.html'), 'utf8');
    write(
      buildRoot,
      'index.html',
      homepage.replace(/<meta property="og:site_name"[^>]+>/, ''),
    );
    const result = runChecker(buildRoot);
    assert.equal(result.ok, false);
    assert.match(result.output, /og:site_name/);
  });
});

test('SEO artifact checker rejects website Open Graph type on documentation', () => {
  withFixture((buildRoot) => {
    const guidePath = path.join(buildRoot, 'docs/language/overview.html');
    const guide = readFileSync(guidePath, 'utf8');
    write(
      buildRoot,
      'docs/language/overview.html',
      guide.replace('property="og:type" content="article"', 'property="og:type" content="website"'),
    );

    const result = runChecker(buildRoot);
    assert.equal(result.ok, false);
    assert.match(result.output, /og:type.+article/i);
  });
});

test('SEO artifact checker rejects missing page-specific structured data', () => {
  for (const [relativePath, schemaType] of [
    ['index.html', 'WebSite'],
    ['docs/language/overview.html', 'TechArticle'],
  ]) {
    withFixture((buildRoot) => {
      const page = readFileSync(path.join(buildRoot, relativePath), 'utf8');
      write(
        buildRoot,
        relativePath,
        page.replace(/<script type="application\/ld\+json">.*?<\/script>/s, ''),
      );
      const result = runChecker(buildRoot);
      assert.equal(result.ok, false);
      assert.match(result.output, new RegExp(schemaType));
    });
  }
});

test('SEO artifact checker accepts browser-recovered JSON-LD closing script tags', () => {
  withFixture((buildRoot) => {
    const guidePath = path.join(buildRoot, 'docs/language/overview.html');
    const guide = readFileSync(guidePath, 'utf8');
    write(
      buildRoot,
      'docs/language/overview.html',
      guide.replace('</script>', '</script\t\n data-recovered>'),
    );

    const result = runChecker(buildRoot);
    assert.equal(result.ok, true, result.output);
  });
});
