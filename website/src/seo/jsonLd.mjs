export function serializeJsonLd(value) {
  const serialized = JSON.stringify(value);
  if (serialized === undefined) {
    throw new TypeError('JSON-LD value must have a JSON representation');
  }
  return serialized.replaceAll('<', '\\u003c');
}
