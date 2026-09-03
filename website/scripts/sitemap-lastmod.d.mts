type SitemapItem = {
  url: string;
  lastmod?: string | null;
  changefreq?: 'always' | 'daily' | 'hourly' | 'monthly' | 'never' | 'weekly' | 'yearly' | null;
  priority?: number | null;
};

export function latestGitDate(websiteRoot: string, repositoryPaths: string[]): string;
export function withHomepageLastmod(
  items: SitemapItem[],
  homepageUrl: string,
  lastmod: string,
): SitemapItem[];
