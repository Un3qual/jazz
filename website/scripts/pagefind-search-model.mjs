const routeCategories = [
  ['getting-started', 'Getting started'],
  ['language', 'Language'],
  ['standard-library', 'Standard library'],
  ['compiler', 'Compiler'],
  ['project', 'Project'],
  ['reference', 'Reference'],
];

export function createSearchRequestTracker() {
  let currentRequest = 0;
  return {
    begin: () => ++currentRequest,
    invalidate: () => ++currentRequest,
    isCurrent: (request) => request === currentRequest,
  };
}

export function replaceSearchResults(state, rows) {
  return {
    rows,
    activeIndex: 0,
    revision: state.revision + 1,
  };
}

export function isEditableTarget(target) {
  if (!target) {
    return false;
  }

  if (target.isContentEditable) {
    return true;
  }

  const tagName = target.tagName?.toLowerCase();
  if (['input', 'textarea', 'select'].includes(tagName)) {
    return true;
  }

  return Boolean(target.closest?.('[contenteditable="true"], input, textarea, select'));
}

export function shouldOpenSearch(event) {
  if (isEditableTarget(event.target) || event.altKey) {
    return false;
  }

  const key = event.key?.toLowerCase();
  return (
    (key === '/' && !event.ctrlKey && !event.metaKey) ||
    (key === 'k' && Boolean(event.ctrlKey || event.metaKey))
  );
}

export function withBaseUrl(url, baseUrl) {
  if (/^https?:\/\//i.test(url)) {
    return url;
  }

  const base = `/${baseUrl ?? '/'}`.replace(/\/+/g, '/').replace(/\/?$/, '/');
  const result = `/${url ?? ''}`.replace(/\/+/g, '/');
  return result.startsWith(base) ? result : `${base}${result.slice(1)}`;
}

export function categoryForUrl(url) {
  const pathname = new URL(url, 'https://jazz.invalid').pathname;
  const route = pathname.match(/\/docs\/([^/]+)/)?.[1];
  return routeCategories.find(([prefix]) => prefix === route)?.[1] ?? 'Reference';
}

function plainExcerpt(excerpt) {
  return String(excerpt ?? '').replace(/<\/?mark>/g, '');
}

export async function normalizePagefindResults(response, baseUrl) {
  const results = response?.results ?? [];
  const rows = await Promise.all(results.map((result) => result.data()));

  return rows.flatMap((page) => {
    const pageTitle = page.meta?.title ?? page.title ?? '';
    const sections = page.sub_results?.length > 0
      ? page.sub_results
      : [{url: page.url, title: '', excerpt: page.excerpt}];

    return sections.map((section) => {
      const url = withBaseUrl(section.url ?? page.url, baseUrl);
      return {
        url,
        pageTitle,
        sectionTitle: section.title ?? '',
        category: categoryForUrl(url),
        excerpt: plainExcerpt(section.excerpt ?? page.excerpt),
      };
    });
  });
}
