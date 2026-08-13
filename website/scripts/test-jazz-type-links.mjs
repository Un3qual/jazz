import assert from 'node:assert/strict';
import test from 'node:test';

import {getJazzTypeLinkSpans} from './jazz-type-links.mjs';

test('maps every named module type and capability to its canonical route', () => {
  const cases = [
    ['Maybe', 5, '/docs/standard-library/maybe'],
    ['Result', 6, '/docs/standard-library/result'],
    ['NonEmpty', 8, '/docs/standard-library/nonempty'],
    ['Dictionary', 10, '/docs/standard-library/dictionary'],
    ['Queue', 5, '/docs/standard-library/queue'],
    ['Map', 3, '/docs/standard-library/map'],
    ['Set', 3, '/docs/standard-library/set'],
    ['Char', 4, '/docs/standard-library/char'],
    ['Text', 4, '/docs/standard-library/text'],
    ['List', 4, '/docs/standard-library/list'],
    ['IOError', 7, '/docs/standard-library/io-error#ioerror'],
    ['IOErrorCategory', 15, '/docs/standard-library/io-error#ioerrorcategory'],
    ['Ordering', 8, '/docs/standard-library/prelude#ordering'],
    ['Eq', 2, '/docs/standard-library/prelude#eq'],
    ['Ord', 3, '/docs/standard-library/prelude#ord'],
    ['Num', 3, '/docs/standard-library/prelude#num'],
    ['Integral', 8, '/docs/standard-library/prelude#integral'],
    ['Fractional', 10, '/docs/standard-library/prelude#fractional'],
    ['Showable', 8, '/docs/standard-library/prelude#showable'],
    ['Default', 7, '/docs/standard-library/prelude#default'],
  ];

  for (const [source, end, destination] of cases) {
    assert.deepEqual(getJazzTypeLinkSpans(source), [
      {start: 0, end, destination},
    ]);
  }
});

test('maps every runtime type to its exact Runtime values anchor', () => {
  const cases = [
    ['Bool', 4, '/docs/reference/runtime-values#bool'],
    ['Int', 3, '/docs/reference/runtime-values#int'],
    ['Float', 5, '/docs/reference/runtime-values#float'],
    ['Int8', 4, '/docs/reference/runtime-values#int8'],
    ['Int16', 5, '/docs/reference/runtime-values#int16'],
    ['Int32', 5, '/docs/reference/runtime-values#int32'],
    ['Int64', 5, '/docs/reference/runtime-values#int64'],
    ['UInt8', 5, '/docs/reference/runtime-values#uint8'],
    ['UInt16', 6, '/docs/reference/runtime-values#uint16'],
    ['UInt32', 6, '/docs/reference/runtime-values#uint32'],
    ['UInt64', 6, '/docs/reference/runtime-values#uint64'],
    ['Float16', 7, '/docs/reference/runtime-values#float16'],
    ['Float32', 7, '/docs/reference/runtime-values#float32'],
    ['Float64', 7, '/docs/reference/runtime-values#float64'],
  ];

  for (const [source, end, destination] of cases) {
    assert.deepEqual(getJazzTypeLinkSpans(source), [
      {start: 0, end, destination},
    ]);
  }
});

test('maps repeated and nested concrete types without guessing identifiers', () => {
  assert.deepEqual(getJazzTypeLinkSpans('Maybe -> Maybe'), [
    {start: 0, end: 5, destination: '/docs/standard-library/maybe'},
    {start: 9, end: 14, destination: '/docs/standard-library/maybe'},
  ]);
  assert.deepEqual(getJazzTypeLinkSpans('Result(IOError, Maybe(Text))'), [
    {start: 0, end: 6, destination: '/docs/standard-library/result'},
    {start: 7, end: 14, destination: '/docs/standard-library/io-error#ioerror'},
    {start: 16, end: 21, destination: '/docs/standard-library/maybe'},
    {start: 22, end: 26, destination: '/docs/standard-library/text'},
  ]);
  assert.deepEqual(getJazzTypeLinkSpans('a -> value -> Future'), []);
});

test('does not link mapped names inside valid unknown Jazz identifiers', () => {
  const cases = ['_Maybe', 'Maybe_', "Maybe'", 'Maybe!', 'ΩMaybe'];

  for (const source of cases) {
    assert.deepEqual(getJazzTypeLinkSpans(source), []);
  }
});

test('maps only balanced list delimiters and their nested concrete types', () => {
  assert.deepEqual(getJazzTypeLinkSpans('[[Maybe(Int)]]'), [
    {start: 0, end: 1, destination: '/docs/standard-library/list'},
    {start: 1, end: 2, destination: '/docs/standard-library/list'},
    {start: 2, end: 7, destination: '/docs/standard-library/maybe'},
    {start: 8, end: 11, destination: '/docs/reference/runtime-values#int'},
    {start: 12, end: 13, destination: '/docs/standard-library/list'},
    {start: 13, end: 14, destination: '/docs/standard-library/list'},
  ]);
  assert.deepEqual(getJazzTypeLinkSpans('[Maybe'), [
    {start: 1, end: 6, destination: '/docs/standard-library/maybe'},
  ]);
});

test('maps tuple and unit syntax but not function-argument parentheses', () => {
  assert.deepEqual(getJazzTypeLinkSpans('(Int, Float)'), [
    {start: 0, end: 1, destination: '/docs/reference/runtime-values#tuples'},
    {start: 1, end: 4, destination: '/docs/reference/runtime-values#int'},
    {start: 6, end: 11, destination: '/docs/reference/runtime-values#float'},
    {start: 11, end: 12, destination: '/docs/reference/runtime-values#tuples'},
  ]);
  assert.deepEqual(getJazzTypeLinkSpans('()'), [
    {start: 0, end: 2, destination: '/docs/reference/runtime-values#unit'},
  ]);
  assert.deepEqual(getJazzTypeLinkSpans('(a -> Bool)'), [
    {start: 6, end: 10, destination: '/docs/reference/runtime-values#bool'},
  ]);
  assert.deepEqual(getJazzTypeLinkSpans('Ω(a,b)'), []);
});
