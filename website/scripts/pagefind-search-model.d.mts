export interface SearchShortcutEvent {
  altKey?: boolean;
  ctrlKey?: boolean;
  key?: string;
  metaKey?: boolean;
  target?: unknown;
}

export interface SearchResultRow {
  url: string;
  pageTitle: string;
  sectionTitle: string;
  category: string;
  excerpt: string;
}

export interface PagefindResultData {
  url?: string;
  excerpt?: string;
  title?: string;
  meta?: {title?: string};
  sub_results?: Array<{url?: string; title?: string; excerpt?: string}>;
}

export interface PagefindSearchResponse {
  results?: Array<{data(): Promise<PagefindResultData>}>;
}

export function isEditableTarget(target: unknown): boolean;
export function shouldOpenSearch(event: SearchShortcutEvent): boolean;
export function withBaseUrl(url: string, baseUrl?: string): string;
export function categoryForUrl(url: string): string;
export function normalizePagefindResults(
  response: PagefindSearchResponse | undefined,
  baseUrl?: string,
): Promise<SearchResultRow[]>;
