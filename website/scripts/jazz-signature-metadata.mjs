export function withJazzSignatureMetadata(metadata, metastring) {
  return {
    ...metadata,
    jazzSignature:
      metastring?.split(/\s+/).includes('jazz-signature') ?? false,
  };
}
