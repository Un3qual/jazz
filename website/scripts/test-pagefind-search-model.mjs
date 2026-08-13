import assert from 'node:assert/strict';
import test from 'node:test';

import {
  categoryForUrl,
  createSearchRequestTracker,
  normalizePagefindResults,
  replaceSearchResults,
  shouldOpenSearch,
  withBaseUrl,
} from './pagefind-search-model.mjs';

test('search shortcuts open only outside editable targets', () => {
  // Mutation caught: accepting ordinary keys, or ignoring an input target,
  // steals text entry instead of opening the documentation search dialog.
  assert.equal(shouldOpenSearch({key: '/', target: {tagName: 'DIV'}}), true);
  assert.equal(shouldOpenSearch({key: 'k', ctrlKey: true, target: {tagName: 'DIV'}}), true);
  assert.equal(shouldOpenSearch({key: 'k', metaKey: true, target: {tagName: 'DIV'}}), true);
  assert.equal(shouldOpenSearch({key: 'k', target: {tagName: 'DIV'}}), false);
  assert.equal(shouldOpenSearch({key: '/', target: {tagName: 'INPUT'}}), false);
  assert.equal(shouldOpenSearch({key: 'k', ctrlKey: true, target: {isContentEditable: true}}), false);
  assert.equal(shouldOpenSearch({key: '/', target: {tagName: 'BUTTON'}}), true);
});

test('clearing a search invalidates its outstanding Pagefind response', () => {
  // Mutation caught: a completed request repopulates results after its query
  // has been cleared because the empty-query branch kept its request current.
  const requests = createSearchRequestTracker();
  const request = requests.begin();

  requests.invalidate();

  assert.equal(requests.isCurrent(request), false);
});

test('replacing results advances the visible result-set revision', () => {
  // Mutation caught: replacing a scrolled result list while the active row is
  // already zero skips its visibility effect and leaves the active row hidden.
  const replacement = replaceSearchResults(
    {rows: [{url: '/jazz/docs/old'}], activeIndex: 0, revision: 4},
    [{url: '/jazz/docs/new'}],
  );

  assert.deepEqual(replacement, {
    rows: [{url: '/jazz/docs/new'}],
    activeIndex: 0,
    revision: 5,
  });
});

test('empty-result feedback waits for the current Pagefind request', async () => {
  // Mutation caught: treating an in-flight empty row set as a completed empty
  // search flashes "No documentation matches" before Pagefind responds.
  const searchModel = await import('./pagefind-search-model.mjs');

  assert.equal(typeof searchModel.shouldShowNoMatches, 'function');
  assert.equal(searchModel.shouldShowNoMatches({
    status: 'ready',
    query: 'maybe',
    resultCount: 0,
    pending: true,
  }), false);
  assert.equal(searchModel.shouldShowNoMatches({
    status: 'ready',
    query: 'maybe',
    resultCount: 0,
    pending: false,
  }), true);
  assert.equal(searchModel.shouldShowNoMatches({
    status: 'ready',
    query: 'maybe',
    resultCount: 1,
    pending: false,
  }), false);
});

test('search categories follow public documentation routes', () => {
  // Mutation caught: changing the route prefix sends a result to the wrong
  // reference context, making a dense result list harder to scan.
  assert.equal(categoryForUrl('/jazz/docs/getting-started/overview'), 'Getting started');
  assert.equal(categoryForUrl('/jazz/docs/language/overview'), 'Language');
  assert.equal(categoryForUrl('/jazz/docs/standard-library/maybe'), 'Standard library');
  assert.equal(categoryForUrl('/jazz/docs/compiler/architecture'), 'Compiler');
  assert.equal(categoryForUrl('/jazz/docs/project/status'), 'Project');
  assert.equal(categoryForUrl('/jazz/docs/reference/expression-grammar'), 'Reference');
});

test('search result URLs preserve a configured base URL exactly once', () => {
  // Mutation caught: dropping /jazz/ breaks production navigation, while
  // applying it twice produces a non-existent path.
  assert.equal(withBaseUrl('/docs/standard-library/maybe#maybemap', '/jazz/'), '/jazz/docs/standard-library/maybe#maybemap');
  assert.equal(withBaseUrl('/jazz/docs/standard-library/maybe#maybemap', '/jazz/'), '/jazz/docs/standard-library/maybe#maybemap');
  assert.equal(withBaseUrl('docs/reference/types', '/jazz/'), '/jazz/docs/reference/types');
});

test('search model flattens Pagefind section results with local excerpts', async () => {
  // Mutation caught: using the wrapper payload or a page-only row discards
  // Pagefind's ranked section URL and excerpt.
  const rows = await normalizePagefindResults({
    results: [
      {
        excerpt: 'non-local wrapper excerpt',
        data: async () => ({
          url: '/docs/standard-library/maybe',
          excerpt: 'page excerpt',
          meta: {title: 'Maybe'},
          sub_results: [
            {
              url: '/docs/standard-library/maybe#maybemap',
              title: 'maybeMap',
              excerpt: 'Maps a function over a Maybe value.',
            },
          ],
        }),
      },
    ],
  }, '/jazz/');

  assert.deepEqual(rows, [{
    url: '/jazz/docs/standard-library/maybe#maybemap',
    pageTitle: 'Maybe',
    sectionTitle: 'maybeMap',
    category: 'Standard library',
    excerpt: 'Maps a function over a Maybe value.',
  }]);
});

test('search model keeps a page result when Pagefind has no sections', async () => {
  const rows = await normalizePagefindResults({
    results: [
      {
        data: async () => ({
          url: '/docs/compiler/architecture',
          excerpt: 'The compiler moves through explicit stages.',
          meta: {title: 'Compiler architecture'},
        }),
      },
    ],
  }, '/jazz/');

  assert.deepEqual(rows, [{
    url: '/jazz/docs/compiler/architecture',
    pageTitle: 'Compiler architecture',
    sectionTitle: '',
    category: 'Compiler',
    excerpt: 'The compiler moves through explicit stages.',
  }]);
});

test('search model normalizes an empty Pagefind response', async () => {
  assert.deepEqual(await normalizePagefindResults({results: []}, '/jazz/'), []);
  assert.deepEqual(await normalizePagefindResults(undefined, '/jazz/'), []);
});
