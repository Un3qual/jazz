export function serializeJsonLd(value) {
  return JSON.stringify(value).replaceAll('<', '\\u003c');
}
