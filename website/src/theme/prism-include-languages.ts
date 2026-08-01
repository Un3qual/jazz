import siteConfig from '@generated/docusaurus.config';
import type * as PrismNamespace from 'prismjs';
import type {Optional} from 'utility-types';

import {registerJazz} from '../../scripts/prism-jazz-grammar.mjs';

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

  registerJazz(PrismObject);

  delete (globalThis as Optional<typeof globalThis, 'Prism'>).Prism;
  if (typeof prismBefore !== 'undefined') {
    globalThis.Prism = prismBefore;
  }
}
