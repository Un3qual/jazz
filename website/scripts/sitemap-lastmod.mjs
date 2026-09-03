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
    throw new Error('Unable to determine the page modification date from Git history');
  }
  return date;
}

export function withSitemapLastmods(
  items,
  homepageUrl,
  homepageLastmod,
  documentationRootUrl,
  documentationLastmod,
  documentationGroupLastmods = [],
) {
  return items.map((item) => {
    const sharedLastmodFloor = item.url === homepageUrl
      ? homepageLastmod
      : item.url === documentationRootUrl || item.url.startsWith(`${documentationRootUrl}/`)
        ? documentationLastmod
        : undefined;
    const groupLastmodFloors = documentationGroupLastmods
      .filter(({urls, urlPrefixes}) =>
        urls.includes(item.url)
        || urlPrefixes.some((urlPrefix) =>
          item.url === urlPrefix || item.url.startsWith(`${urlPrefix}/`),
        ),
      )
      .map(({lastmod}) => lastmod);
    const lastmodFloor = [sharedLastmodFloor, ...groupLastmodFloors]
      .filter(Boolean)
      .reduce(
        (latest, candidate) => candidate > latest ? candidate : latest,
        '',
      ) || undefined;
    return lastmodFloor && (!item.lastmod || item.lastmod < lastmodFloor)
      ? {...item, lastmod: lastmodFloor}
      : item;
  });
}
