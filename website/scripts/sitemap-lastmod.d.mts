type SitemapItem = {
  url: string;
  lastmod?: string | null;
  changefreq?: 'always' | 'daily' | 'hourly' | 'monthly' | 'never' | 'weekly' | 'yearly' | null;
  priority?: number | null;
};

type SitemapLastmodGroup = {
  urls: string[];
  urlPrefixes: string[];
  lastmod: string;
};

export function latestGitDate(websiteRoot: string, repositoryPaths: string[]): string;
export function withSitemapLastmods(
  items: SitemapItem[],
  homepageUrl: string,
  homepageLastmod: string,
  documentationRootUrl: string,
  documentationLastmod: string,
  documentationGroupLastmods?: SitemapLastmodGroup[],
): SitemapItem[];
