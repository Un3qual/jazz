import assert from 'node:assert/strict';
import test from 'node:test';

import {serializeJsonLd} from '../src/seo/jsonLd.mjs';

test('JSON-LD serialization cannot terminate its script element', () => {
  const value = {
    description: '</script><script id="jsonld-poc">globalThis.pwned = true</script>',
  };

  const serialized = serializeJsonLd(value);

  assert.doesNotMatch(serialized, /<\/script/i);
  assert.deepEqual(JSON.parse(serialized), value);
});
