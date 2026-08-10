import type {TokenizeWithThemeOptions, TokensResult} from 'shiki/types';

export type JazzColorMode = 'light' | 'dark';

export interface JazzTokenizeOptions {
  includeExplanation?: TokenizeWithThemeOptions['includeExplanation'];
}

export function tokenizeJazz(
  code: string,
  colorMode: JazzColorMode,
  options?: JazzTokenizeOptions,
): TokensResult;
