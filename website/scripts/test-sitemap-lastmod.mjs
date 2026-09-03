import assert from 'node:assert/strict';
import {execFileSync} from 'node:child_process';
import {mkdirSync, mkdtempSync, rmSync, writeFileSync} from 'node:fs';
import {tmpdir} from 'node:os';
import path from 'node:path';
import test from 'node:test';

import {latestGitDate, withSitemapLastmods} from './sitemap-lastmod.mjs';

function git(cwd, args, date) {
  execFileSync('git', args, {
    cwd,
    env: date
      ? {...process.env, GIT_AUTHOR_DATE: `${date}T12:00:00Z`, GIT_COMMITTER_DATE: `${date}T12:00:00Z`}
      : process.env,
  });
}

test('homepage modification date includes generated-content dependencies', () => {
  const repositoryRoot = mkdtempSync(path.join(tmpdir(), 'jazz-sitemap-git-'));
  const websiteRoot = path.join(repositoryRoot, 'website');
  try {
    mkdirSync(websiteRoot);
    git(repositoryRoot, ['init', '--quiet']);
    git(repositoryRoot, ['config', 'user.email', 'seo-test@example.com']);
    git(repositoryRoot, ['config', 'user.name', 'SEO test']);
    writeFileSync(path.join(websiteRoot, 'index.tsx'), 'first');
    git(repositoryRoot, ['add', 'website/index.tsx']);
    git(repositoryRoot, ['commit', '--quiet', '-m', 'homepage'], '2026-08-01');
    writeFileSync(path.join(repositoryRoot, 'factorial.jz'), 'new dependency');
    git(repositoryRoot, ['add', 'factorial.jz']);
    git(repositoryRoot, ['commit', '--quiet', '-m', 'example'], '2026-09-02');

    assert.equal(
      latestGitDate(websiteRoot, ['website/index.tsx', 'factorial.jz']),
      '2026-09-02',
    );
  } finally {
    rmSync(repositoryRoot, {recursive: true, force: true});
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
