import assert from 'node:assert/strict';
import test from 'node:test';

import {addJazzSignatureMetadata} from './remark-jazz-signatures.mjs';

function code(lang, meta = undefined) {
  return {type: 'code', lang, meta, value: 'value :: Maybe(a).'};
}

test('marks only a Jazz fence immediately following a signature marker', () => {
  const tree = {
    type: 'root',
    children: [
      {type: 'html', value: '<!-- jazz-signature -->'},
      code('jazz', '{1} title="Example"'),
    ],
  };

  addJazzSignatureMetadata(tree);

  assert.equal(tree.children[0].value, '<!-- jazz-signature -->');
  assert.equal(tree.children[1].meta, '{1} title="Example" jazz-signature');
});

test('marks a Jazz fence separated by whitespace-only AST text', () => {
  const tree = {
    type: 'root',
    children: [
      {type: 'html', value: '<!-- jazz-signature -->'},
      {type: 'text', value: '\n\n'},
      code('jazz'),
    ],
  };

  addJazzSignatureMetadata(tree);

  assert.equal(tree.children[2].meta, 'jazz-signature');
});

test('leaves non-Jazz fences and ordinary Jazz examples unchanged', () => {
  const tree = {
    type: 'root',
    children: [
      {type: 'html', value: '<!-- jazz-signature -->'},
      code('typescript', 'showLineNumbers'),
      code('jazz', 'title="Ordinary example"'),
    ],
  };

  addJazzSignatureMetadata(tree);

  assert.equal(tree.children[1].meta, 'showLineNumbers');
  assert.equal(tree.children[2].meta, 'title="Ordinary example"');
});

test('does not cross non-whitespace nodes to find a Jazz fence', () => {
  const tree = {
    type: 'root',
    children: [
      {type: 'html', value: '<!-- jazz-signature -->'},
      {type: 'text', value: 'An explanation separates this fence.'},
      code('jazz'),
    ],
  };

  addJazzSignatureMetadata(tree);

  assert.equal(tree.children[2].meta, undefined);
});
