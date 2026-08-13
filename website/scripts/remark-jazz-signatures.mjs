const signatureMarker = '<!-- jazz-signature -->';

function isWhitespaceNode(node) {
  return node.type === 'text' && /^\s*$/.test(node.value ?? '');
}

function isSignatureMarker(node) {
  return node.type === 'html' && node.value?.trim() === signatureMarker;
}

function appendSignatureMeta(meta) {
  const tokens = meta?.trim() ? meta.trim().split(/\s+/) : [];
  return tokens.includes('jazz-signature')
    ? meta
    : meta
      ? `${meta}${/\s$/.test(meta) ? '' : ' '}jazz-signature`
      : 'jazz-signature';
}

export function addJazzSignatureMetadata(tree) {
  const {children = []} = tree;

  for (let index = 0; index < children.length; index += 1) {
    if (!isSignatureMarker(children[index])) {
      continue;
    }

    let codeIndex = index + 1;
    while (isWhitespaceNode(children[codeIndex])) {
      codeIndex += 1;
    }

    const codeNode = children[codeIndex];
    if (codeNode?.type === 'code' && codeNode.lang === 'jazz') {
      codeNode.meta = appendSignatureMeta(codeNode.meta);
    }
  }

  return tree;
}

export default function remarkJazzSignatures() {
  return addJazzSignatureMetadata;
}
