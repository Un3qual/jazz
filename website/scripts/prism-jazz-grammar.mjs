const identifierContinuation = String.raw`\p{Alphabetic}\p{Number}_'!`;
const leftBoundary = String.raw`(?<![${identifierContinuation}])`;
const rightBoundary = String.raw`(?![${identifierContinuation}])`;

export function registerJazz(PrismObject) {
  PrismObject.languages.jazz = {
    comment: /#.*/,
    string: {
      pattern: /"(?:\\(?:[\\"nrt0]|u\{[0-9a-fA-F]{1,6}\})|[^"\\\r\n])*"/,
      greedy: true,
    },
    char: {
      pattern: /'(?:\\(?:[\\'nrt0]|u\{[0-9a-fA-F]{1,6}\})|[^'\\\r\n])'/,
      greedy: true,
    },
    number: new RegExp(
      `${leftBoundary}\\d+(?:\\.\\d+)?(?:f(?:16|32|64))?${rightBoundary}`,
      'u',
    ),
    capability: new RegExp(
      `${leftBoundary}(?:class|impl)${rightBoundary}`,
      'u',
    ),
    keyword: new RegExp(
      `${leftBoundary}(?:module|import|export|as|data|value|if|then|else|case|operator|precedence|tier|left|right|nonassoc)${rightBoundary}`,
      'u',
    ),
    boolean: new RegExp(
      `${leftBoundary}(?:True|False)${rightBoundary}`,
      'u',
    ),
    signature: new RegExp(
      `${leftBoundary}[\\p{Alphabetic}_][${identifierContinuation}]*(?=\\s*::)`,
      'u',
    ),
    bang: new RegExp(
      `${leftBoundary}[\\p{Alphabetic}_][${identifierContinuation}]*!${rightBoundary}`,
      'u',
    ),
    constructor: new RegExp(
      `${leftBoundary}\\p{Lu}[${identifierContinuation}]*${rightBoundary}`,
      'u',
    ),
    operator: /::|->|[!%&*+\-\/<=>?^|~$]+/,
    punctuation: /[{}\[\](),.@:=\\.]/,
  };
}
