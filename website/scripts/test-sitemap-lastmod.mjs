import assert from 'node:assert/strict';
import {execFileSync} from 'node:child_process';
import {mkdirSync, mkdtempSync, rmSync, writeFileSync} from 'node:fs';
import {tmpdir} from 'node:os';
import path from 'node:path';
import test from 'node:test';

import {latestGitDate, withSitemapLastmods} from './sitemap-lastmod.mjs';
import * as sitemapSourcePaths from './sitemap-source-paths.mjs';

const {
  documentationSharedSources,
  homepageSources,
} = sitemapSourcePaths;

function git(cwd, args, date) {
  execFileSync('git', args, {
    cwd,
    env: date
      ? {...process.env, GIT_AUTHOR_DATE: `${date}T12:00:00Z`, GIT_COMMITTER_DATE: `${date}T12:00:00Z`}
      : process.env,
  });
}

function write(repositoryRoot, relativePath, contents) {
  const file = path.join(repositoryRoot, relativePath);
  mkdirSync(path.dirname(file), {recursive: true});
  writeFileSync(file, contents);
}

function assertTrackedSources(sources, changes, fixtureName) {
  const repositoryRoot = mkdtempSync(path.join(tmpdir(), fixtureName));
  const websiteRoot = path.join(repositoryRoot, 'website');
  try {
    git(repositoryRoot, ['init', '--quiet']);
    git(repositoryRoot, ['config', 'user.email', 'seo-test@example.com']);
    git(repositoryRoot, ['config', 'user.name', 'SEO test']);
    for (const [relativePath, date] of changes) {
      write(repositoryRoot, relativePath, date);
      git(repositoryRoot, ['add', relativePath]);
      git(repositoryRoot, ['commit', '--quiet', '-m', relativePath], date);
      assert.equal(latestGitDate(websiteRoot, sources), date, relativePath);
    }
  } finally {
    rmSync(repositoryRoot, {recursive: true, force: true});
  }
}

test('homepage modification date tracks every rendered source group', () => {
  assertTrackedSources(
    homepageSources,
    [
      ['website/docusaurus.config.ts', '2026-08-01'],
      ['website/src/pages/index.tsx', '2026-08-02'],
      ['website/src/pages/index.module.css', '2026-08-03'],
      ['website/src/components/HomepageHeader.tsx', '2026-08-04'],
      ['website/src/seo/jsonLd.mjs', '2026-08-05'],
      ['website/src/generated/factorial.ts', '2026-08-06'],
      ['website/scripts/sync-factorial.mjs', '2026-08-07'],
      ['examples/functions/factorial.jz', '2026-08-08'],
      ['scripts/example-cases.tsv', '2026-08-09'],
      ['website/sidebars.ts', '2026-08-10'],
      ['website/src/theme/Navbar/Logo/index.tsx', '2026-08-11'],
      ['website/src/theme/SearchBar/index.tsx', '2026-08-12'],
      ['website/scripts/pagefind-search-model.mjs', '2026-08-13'],
      ['website/src/theme/CodeBlock/Content/index.tsx', '2026-08-14'],
      ['website/scripts/jazz-highlighter.mjs', '2026-08-15'],
      ['website/scripts/jazz-type-links.mjs', '2026-08-16'],
      ['editors/vscode-jazz/syntaxes/jazz.tmLanguage.json', '2026-08-17'],
      ['website/scripts/jazz-signature-metadata.mjs', '2026-08-18'],
      ['website/src/css/custom.css', '2026-08-19'],
    ],
    'jazz-sitemap-homepage-git-',
  );
});

test('documentation modification date tracks every shared rendered source group', () => {
  assertTrackedSources(
    documentationSharedSources,
    [
      ['website/docusaurus.config.ts', '2026-08-01'],
      ['website/src/seo/jsonLd.mjs', '2026-08-02'],
      ['website/sidebars.ts', '2026-08-03'],
      ['website/src/theme/Navbar/Logo/index.tsx', '2026-08-04'],
      ['website/src/theme/SearchBar/index.tsx', '2026-08-05'],
      ['website/scripts/pagefind-search-model.mjs', '2026-08-06'],
      ['website/src/theme/DocItem/Layout/index.tsx', '2026-08-07'],
      ['website/src/theme/CodeBlock/Content/index.tsx', '2026-08-08'],
      ['website/scripts/jazz-highlighter.mjs', '2026-08-09'],
      ['website/scripts/jazz-type-links.mjs', '2026-08-10'],
      ['editors/vscode-jazz/syntaxes/jazz.tmLanguage.json', '2026-08-11'],
      ['website/scripts/jazz-signature-metadata.mjs', '2026-08-12'],
      ['website/src/css/custom.css', '2026-08-13'],
    ],
    'jazz-sitemap-docs-git-',
  );
});

test('documentation navigation groups track every sidebar source', () => {
  const groups = sitemapSourcePaths.documentationNavigationGroups;
  assert.ok(Array.isArray(groups));
  const expectations = [
    {
      anchor: '',
      routes: ['', 'getting-started', 'language', 'compiler', 'project'],
      changes: [
        ['docs/index.md', '2026-08-01'],
        ['docs/getting-started/overview.md', '2026-08-02'],
        ['docs/language/overview.md', '2026-08-03'],
        ['docs/compiler/architecture.md', '2026-08-04'],
        ['docs/project/status.md', '2026-08-05'],
      ],
    },
    {
      anchor: 'standard-library',
      routes: ['standard-library'],
      changes: [['docs/standard-library/overview.md', '2026-08-01']],
    },
    {
      anchor: 'reference',
      routes: ['reference'],
      changes: [['docs/reference/lexical-grammar.md', '2026-08-01']],
    },
  ];

  for (const {anchor, routes, changes} of expectations) {
    const group = groups.find(({routes: candidateRoutes}) =>
      candidateRoutes.includes(anchor),
    );
    assert.deepEqual(group?.routes, routes, anchor || 'documentation root');
    assertTrackedSources(
      group.sources,
      changes,
      `jazz-sitemap-${anchor || 'learn'}-navigation-git-`,
    );
  }
});

test('shared modification dates update affected pages without replacing newer page dates', () => {
  const items = [
    {url: 'https://un3qual.github.io/jazz/', lastmod: '2026-08-01'},
    {url: 'https://un3qual.github.io/jazz/docs', lastmod: '2026-08-10'},
    {url: 'https://un3qual.github.io/jazz/docs/language/overview', lastmod: '2026-08-15'},
    {url: 'https://un3qual.github.io/jazz/docs/reference/grammar', lastmod: '2026-09-03'},
    {url: 'https://un3qual.github.io/jazz/docs-preview', lastmod: '2026-08-18'},
    {url: 'https://un3qual.github.io/jazz/playground', lastmod: '2026-08-20'},
  ];

  assert.deepEqual(
    withSitemapLastmods(
      items,
      'https://un3qual.github.io/jazz/',
      '2026-09-02',
      'https://un3qual.github.io/jazz/docs',
      '2026-09-01',
    ),
    [
      {url: 'https://un3qual.github.io/jazz/', lastmod: '2026-09-02'},
      {url: 'https://un3qual.github.io/jazz/docs', lastmod: '2026-09-01'},
      {url: 'https://un3qual.github.io/jazz/docs/language/overview', lastmod: '2026-09-01'},
      items[3],
      items[4],
      items[5],
    ],
  );
});

test('navigation group dates update only documentation rendered with changed siblings', () => {
  const documentationRoot = 'https://un3qual.github.io/jazz/docs';
  const items = [
    {url: documentationRoot, lastmod: '2026-08-01'},
    {url: `${documentationRoot}/language/overview`, lastmod: '2026-08-15'},
    {url: `${documentationRoot}/standard-library/list`, lastmod: '2026-08-20'},
    {url: `${documentationRoot}/reference/grammar`, lastmod: '2026-09-04'},
    {url: 'https://un3qual.github.io/jazz/playground', lastmod: '2026-08-25'},
  ];

  assert.deepEqual(
    withSitemapLastmods(
      items,
      'https://un3qual.github.io/jazz/',
      '2026-08-30',
      documentationRoot,
      '2026-09-01',
      [
        {
          urls: [documentationRoot],
          urlPrefixes: [
            `${documentationRoot}/getting-started`,
            `${documentationRoot}/language`,
            `${documentationRoot}/compiler`,
            `${documentationRoot}/project`,
          ],
          lastmod: '2026-09-02',
        },
        {
          urls: [],
          urlPrefixes: [`${documentationRoot}/standard-library`],
          lastmod: '2026-09-03',
        },
        {
          urls: [],
          urlPrefixes: [`${documentationRoot}/reference`],
          lastmod: '2026-09-03',
        },
      ],
    ),
    [
      {url: documentationRoot, lastmod: '2026-09-02'},
      {url: `${documentationRoot}/language/overview`, lastmod: '2026-09-02'},
      {url: `${documentationRoot}/standard-library/list`, lastmod: '2026-09-03'},
      items[3],
      items[4],
    ],
  );
});
