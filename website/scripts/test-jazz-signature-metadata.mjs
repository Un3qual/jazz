import assert from 'node:assert/strict';
import test from 'node:test';
import {unified} from 'unified';
import remarkParse from 'remark-parse';

import {withJazzSignatureMetadata} from './jazz-signature-metadata.mjs';

function codeFences(markdown) {
  const tree = unified().use(remarkParse).parse(markdown);
  return tree.children.filter((node) => node.type === 'code');
}

test('native Jazz fence metadata reaches code-block metadata', () => {
  const [signature, ordinary] = codeFences(
    '```jazz jazz-signature title="API"\nvalue :: Maybe(a).\n```\n\n' +
      '```jazz title="Ordinary"\nvalue :: Maybe(a).\n```',
  );

  assert.equal(signature.meta, 'jazz-signature title="API"');
  assert.equal(ordinary.meta, 'title="Ordinary"');
  assert.deepEqual(
    withJazzSignatureMetadata({code: signature.value}, signature.meta),
    {code: 'value :: Maybe(a).', jazzSignature: true},
  );
  assert.deepEqual(
    withJazzSignatureMetadata({code: ordinary.value}, ordinary.meta),
    {code: 'value :: Maybe(a).', jazzSignature: false},
  );
});

test('signature metadata is an exact standalone fence token', () => {
  assert.equal(
    withJazzSignatureMetadata({}, 'not-jazz-signature').jazzSignature,
    false,
  );
  assert.equal(
    withJazzSignatureMetadata({}, 'title="API" jazz-signature {1}')
      .jazzSignature,
    true,
  );
});

test('signature metadata ignores marker text inside quoted values', () => {
  for (const metastring of [
    'title="ordinary jazz-signature sample"',
    "title='ordinary jazz-signature sample'",
    'title="ordinary \\"quoted jazz-signature\\" sample"',
  ]) {
    assert.equal(
      withJazzSignatureMetadata({}, metastring).jazzSignature,
      false,
      metastring,
    );
  }

  assert.equal(
    withJazzSignatureMetadata(
      {},
      'title="ordinary \\"quoted sample\\"" jazz-signature',
    ).jazzSignature,
    true,
  );
});

test('signature metadata requires the lowercase marker spelling', () => {
  assert.equal(
    withJazzSignatureMetadata({}, 'JAZZ-SIGNATURE').jazzSignature,
    false,
  );
});
