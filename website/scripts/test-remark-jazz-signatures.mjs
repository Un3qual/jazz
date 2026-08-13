import assert from 'node:assert/strict';
import test from 'node:test';
import {unified} from 'unified';
import remarkParse from 'remark-parse';

import remarkJazzSignatures, {
  addJazzSignatureMetadata,
} from './remark-jazz-signatures.mjs';

function code(lang, meta = undefined) {
  return {type: 'code', lang, meta, value: 'value :: Maybe(a).'};
}

function parseWithSignaturePlugin(markdown) {
  const processor = unified().use(remarkParse).use(remarkJazzSignatures);
  const tree = processor.parse(markdown);
  return processor.runSync(tree);
}

test('parses Markdown fixtures through the exported signature plugin', () => {
  const cases = [
    {
      name: 'an adjacent Jazz fence',
      markdown:
        '<!-- jazz-signature -->\n\n```jazz {1} title="Example"\nvalue :: Maybe(a).\n```',
      expectedMeta: '{1} title="Example" jazz-signature',
      expectedMarker: '<!-- jazz-signature -->',
    },
    {
      name: 'a blank line before a Jazz fence',
      markdown: '<!-- jazz-signature -->\n\n\n```jazz\nvalue :: Maybe(a).\n```',
      expectedMeta: 'jazz-signature',
      expectedMarker: '<!-- jazz-signature -->',
    },
    {
      name: 'a non-Jazz fence',
      markdown: '<!-- jazz-signature -->\n\n```typescript showLineNumbers\nconst value = 1;\n```',
      expectedMeta: 'showLineNumbers',
      expectedMarker: '<!-- jazz-signature -->',
    },
    {
      name: 'an ordinary Jazz example',
      markdown: '```jazz title="Ordinary example"\nvalue :: Maybe(a).\n```',
      expectedMeta: 'title="Ordinary example"',
      expectedMarker: undefined,
    },
    {
      name: 'a non-adjacent marker',
      markdown:
        '<!-- jazz-signature -->\n\nAn explanation separates this fence.\n\n```jazz\nvalue :: Maybe(a).\n```',
      expectedMeta: null,
      expectedMarker: '<!-- jazz-signature -->',
    },
  ];

  for (const fixture of cases) {
    const tree = parseWithSignaturePlugin(fixture.markdown);
    const marker = tree.children.find((node) => node.type === 'html');
    const fence = tree.children.find((node) => node.type === 'code');

    assert.equal(marker?.value, fixture.expectedMarker, fixture.name);
    assert.equal(fence?.meta ?? null, fixture.expectedMeta, fixture.name);
  }
});

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
