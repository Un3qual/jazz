import assert from 'node:assert/strict';
import {execFileSync} from 'node:child_process';
import {mkdirSync, mkdtempSync, rmSync, writeFileSync} from 'node:fs';
import {tmpdir} from 'node:os';
import path from 'node:path';
import test from 'node:test';

import {latestGitDate, withHomepageLastmod} from './sitemap-lastmod.mjs';

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

test('homepage modification date override leaves documentation entries unchanged', () => {
  const items = [
    {url: 'https://un3qual.github.io/jazz/', lastmod: '2026-08-01'},
    {url: 'https://un3qual.github.io/jazz/docs/language/overview', lastmod: '2026-08-15'},
  ];

  assert.deepEqual(
    withHomepageLastmod(items, 'https://un3qual.github.io/jazz/', '2026-09-02'),
    [
      {url: 'https://un3qual.github.io/jazz/', lastmod: '2026-09-02'},
      items[1],
    ],
  );
});
