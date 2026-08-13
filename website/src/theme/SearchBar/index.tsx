import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import {useHistory} from '@docusaurus/router';
import {useCallback, useEffect, useRef, useState} from 'react';

import {
  normalizePagefindResults,
  shouldOpenSearch,
  type PagefindSearchResponse,
  type SearchResultRow,
} from '../../../scripts/pagefind-search-model.mjs';
import styles from './styles.module.css';

type Pagefind = {
  search(query: string): Promise<PagefindSearchResponse>;
};

type SearchStatus = 'idle' | 'loading' | 'ready' | 'unavailable';

function shortcutHint() {
  return typeof navigator !== 'undefined' && /Mac|iPhone|iPad/.test(navigator.platform)
    ? '⌘K'
    : 'Ctrl K';
}

export default function SearchBar() {
  const {siteConfig} = useDocusaurusContext();
  const history = useHistory();
  const baseUrl = siteConfig.baseUrl;
  const dialogRef = useRef<HTMLDialogElement>(null);
  const inputRef = useRef<HTMLInputElement>(null);
  const openerRef = useRef<HTMLButtonElement>(null);
  const activeResultRef = useRef<HTMLButtonElement>(null);
  const pagefindRef = useRef<Pagefind | undefined>(undefined);
  const requestId = useRef(0);
  const [open, setOpen] = useState(false);
  const [status, setStatus] = useState<SearchStatus>('idle');
  const [query, setQuery] = useState('');
  const [results, setResults] = useState<SearchResultRow[]>([]);
  const [activeIndex, setActiveIndex] = useState(0);

  const closeSearch = useCallback(() => {
    requestId.current += 1;
    dialogRef.current?.close();
    setOpen(false);
    setQuery('');
    setResults([]);
    setActiveIndex(0);
    openerRef.current?.focus();
  }, []);

  const openSearch = useCallback(() => {
    setOpen(true);
  }, []);

  useEffect(() => {
    const onKeyDown = (event: KeyboardEvent) => {
      if (shouldOpenSearch(event)) {
        event.preventDefault();
        openSearch();
      }
    };

    window.addEventListener('keydown', onKeyDown);
    return () => window.removeEventListener('keydown', onKeyDown);
  }, [openSearch]);

  useEffect(() => {
    if (!open) {
      dialogRef.current?.close();
      return;
    }

    const dialog = dialogRef.current;
    if (dialog && !dialog.open) {
      dialog.showModal();
    }
    inputRef.current?.focus();

    if (pagefindRef.current) {
      setStatus('ready');
      return;
    }

    let cancelled = false;
    setStatus('loading');
    import(/* webpackIgnore: true */ `${baseUrl}pagefind/pagefind.js`)
      .then((pagefind: Pagefind) => {
        if (!cancelled) {
          pagefindRef.current = pagefind;
          setStatus('ready');
        }
      })
      .catch(() => {
        if (!cancelled) {
          setStatus('unavailable');
        }
      });

    return () => {
      cancelled = true;
    };
  }, [baseUrl, open]);

  useEffect(() => {
    if (!open || status !== 'ready' || !pagefindRef.current) {
      return;
    }

    const trimmedQuery = query.trim();
    if (!trimmedQuery) {
      setResults([]);
      setActiveIndex(0);
      return;
    }

    const currentRequest = ++requestId.current;
    pagefindRef.current.search(trimmedQuery)
      .then((response) => normalizePagefindResults(response, baseUrl))
      .then((rows) => {
        if (requestId.current === currentRequest) {
          setResults(rows);
          setActiveIndex(0);
        }
      })
      .catch(() => {
        if (requestId.current === currentRequest) {
          setStatus('unavailable');
          setResults([]);
        }
      });
  }, [baseUrl, open, query, status]);

  useEffect(() => {
    activeResultRef.current?.scrollIntoView({block: 'nearest'});
  }, [activeIndex]);

  const goToResult = useCallback((result: SearchResultRow) => {
    closeSearch();
    history.push(result.url);
  }, [closeSearch, history]);

  const onInputKeyDown = (event: React.KeyboardEvent<HTMLInputElement>) => {
    if (event.key === 'ArrowDown' && results.length > 0) {
      event.preventDefault();
      setActiveIndex((index) => Math.min(index + 1, results.length - 1));
    }
    if (event.key === 'ArrowUp' && results.length > 0) {
      event.preventDefault();
      setActiveIndex((index) => Math.max(index - 1, 0));
    }
    if (event.key === 'Enter' && results[activeIndex]) {
      event.preventDefault();
      goToResult(results[activeIndex]);
    }
  };

  return (
    <>
      <button
        ref={openerRef}
        className={styles.searchControl}
        type="button"
        aria-label="Search documentation"
        onClick={openSearch}>
        <span className={styles.searchIcon} aria-hidden="true">⌕</span>
        <span className={styles.controlLabel}>Search</span>
        <kbd className={styles.shortcut}>{shortcutHint()}</kbd>
      </button>
      <dialog
        ref={dialogRef}
        className={styles.searchDialog}
        aria-label="Search documentation"
        onCancel={(event) => {
          event.preventDefault();
          closeSearch();
        }}
        onClose={() => setOpen(false)}>
        <div className={styles.dialogHeader}>
          <label className={styles.searchLabel} htmlFor="documentation-search">Search documentation</label>
          <button className={styles.closeControl} type="button" onClick={closeSearch}>Close</button>
        </div>
        <div className={styles.inputRow}>
          <span className={styles.searchIcon} aria-hidden="true">⌕</span>
          <input
            ref={inputRef}
            id="documentation-search"
            type="search"
            value={query}
            placeholder="Search the reference"
            autoComplete="off"
            onChange={(event) => setQuery(event.target.value)}
            onKeyDown={onInputKeyDown}
          />
          <kbd className={styles.escapeHint}>Esc</kbd>
        </div>
        <p className={styles.keyboardHint}>↑↓ to select · Enter to open · Esc to close</p>
        {status === 'loading' && <p className={styles.state}>Loading search index…</p>}
        {status === 'unavailable' && <p className={styles.state}>Search is unavailable in this preview.</p>}
        {status === 'ready' && query.trim() && results.length === 0 && <p className={styles.state}>No documentation matches.</p>}
        {status === 'ready' && results.length > 0 && (
          <ol className={styles.results} aria-label="Search results">
            {results.map((result, index) => (
              <li key={result.url}>
                <button
                  ref={index === activeIndex ? activeResultRef : undefined}
                  className={index === activeIndex ? styles.activeResult : styles.result}
                  type="button"
                  onMouseMove={() => setActiveIndex(index)}
                  onClick={() => goToResult(result)}>
                  <span className={styles.resultContext}>{result.category} · {result.pageTitle}</span>
                  {result.sectionTitle && <strong>{result.sectionTitle}</strong>}
                  <span>{result.excerpt}</span>
                </button>
              </li>
            ))}
          </ol>
        )}
      </dialog>
    </>
  );
}
