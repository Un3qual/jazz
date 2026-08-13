export interface JazzTypeLinkSpan {
  readonly start: number;
  readonly end: number;
  readonly destination: string;
}

export const JAZZ_TYPE_DESTINATIONS: Readonly<Record<string, string>>;

export function getJazzTypeLinkSpans(source: string): JazzTypeLinkSpan[];
