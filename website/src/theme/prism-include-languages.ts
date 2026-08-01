import siteConfig from '@generated/docusaurus.config';
import type * as PrismNamespace from 'prismjs';
import type {Optional} from 'utility-types';

export default function prismIncludeLanguages(
  PrismObject: typeof PrismNamespace,
): void {
  const {
    themeConfig: {prism},
  } = siteConfig;
  const {additionalLanguages} = prism as {additionalLanguages: string[]};

  const prismBefore = globalThis.Prism;
  globalThis.Prism = PrismObject;

  additionalLanguages
    .filter((language) => language !== 'jazz')
    .forEach((language) => {
      if (language === 'php') {
        require('prismjs/components/prism-markup-templating.js');
      }
      require(`prismjs/components/prism-${language}`);
    });

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
    number: /\b\d+(?:\.\d+)?(?:f(?:16|32|64))?\b/,
    capability: /\b(?:class|impl)\b/,
    keyword:
      /\b(?:module|import|export|as|data|value|if|then|else|case|operator|precedence|tier|left|right|nonassoc)\b/,
    boolean: /\b(?:True|False)\b/,
    signature: /\b[a-z_][\w']*(?=\s*::)/i,
    constructor: /\b[A-Z][\w']*\b/,
    bang: /\b[a-z_][\w']*!/i,
    operator: /::|->|[!%&*+\-\/<=>?^|~$]+/,
    punctuation: /[{}[\](),.@:=\\.]/,
  };

  delete (globalThis as Optional<typeof globalThis, 'Prism'>).Prism;
  if (typeof prismBefore !== 'undefined') {
    globalThis.Prism = prismBefore;
  }
}
