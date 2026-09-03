import {execFileSync} from 'node:child_process';
import path from 'node:path';

export function latestGitDate(websiteRoot, repositoryPaths) {
  const repositoryRoot = path.resolve(websiteRoot, '..');
  const date = execFileSync(
    'git',
    ['log', '-1', '--format=%cs', '--', ...repositoryPaths],
    {cwd: repositoryRoot, encoding: 'utf8'},
  ).trim();

  if (!/^\d{4}-\d{2}-\d{2}$/.test(date)) {
    throw new Error('Unable to determine the homepage modification date from Git history');
  }
  return date;
}

export function withHomepageLastmod(items, homepageUrl, lastmod) {
  return items.map((item) => item.url === homepageUrl ? {...item, lastmod} : item);
}
