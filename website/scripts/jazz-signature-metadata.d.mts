export function withJazzSignatureMetadata<Metadata extends object>(
  metadata: Metadata,
  metastring: string | undefined,
): Metadata & {readonly jazzSignature: boolean};
