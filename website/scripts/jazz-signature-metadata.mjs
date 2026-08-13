function metadataTokens(metastring) {
  const tokens = [];
  let start;
  let quote;
  let escaped = false;

  for (let index = 0; index < metastring.length; index += 1) {
    const character = metastring[index];
    if (start === undefined) {
      if (/\s/u.test(character)) {
        continue;
      }
      start = index;
    }

    if (quote !== undefined) {
      if (escaped) {
        escaped = false;
      } else if (character === '\\') {
        escaped = true;
      } else if (character === quote) {
        quote = undefined;
      }
    } else if (character === '"' || character === "'") {
      quote = character;
    } else if (/\s/u.test(character)) {
      tokens.push(metastring.slice(start, index));
      start = undefined;
    }
  }

  if (start !== undefined) {
    tokens.push(metastring.slice(start));
  }

  return tokens;
}

export function withJazzSignatureMetadata(metadata, metastring) {
  return {
    ...metadata,
    jazzSignature:
      metastring !== undefined &&
      metadataTokens(metastring).includes('jazz-signature'),
  };
}
