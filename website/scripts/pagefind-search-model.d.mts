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

export interface SearchRequestTracker {
  begin(): number;
  invalidate(): number;
  isCurrent(request: number): boolean;
}

export interface SearchResultState<T> {
  rows: T[];
  activeIndex: number;
  revision: number;
}

export interface EmptySearchState {
  status: 'idle' | 'loading' | 'ready' | 'unavailable';
  query: string;
  resultCount: number;
  pending: boolean;
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
export function createSearchRequestTracker(): SearchRequestTracker;
export function replaceSearchResults<T>(
  state: SearchResultState<T>,
  rows: T[],
): SearchResultState<T>;
export function shouldShowNoMatches(state: EmptySearchState): boolean;
export function shouldOpenSearch(event: SearchShortcutEvent): boolean;
export function withBaseUrl(url: string, baseUrl?: string): string;
export function categoryForUrl(url: string): string;
export function normalizePagefindResults(
  response: PagefindSearchResponse | undefined,
  baseUrl?: string,
): Promise<SearchResultRow[]>;
