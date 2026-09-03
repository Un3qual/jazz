const codeRenderingSources = [
  'website/src/theme/CodeBlock',
  'website/scripts/jazz-highlighter.mjs',
  'website/scripts/jazz-signature-metadata.mjs',
  'website/scripts/jazz-type-links.mjs',
  'editors/vscode-jazz/syntaxes/jazz.tmLanguage.json',
];

export const homepageSources = [
  'website/docusaurus.config.ts',
  'website/src/pages/index.tsx',
  'website/src/pages/index.module.css',
  'website/src/components',
  'website/src/seo/jsonLd.mjs',
  'website/src/generated/factorial.ts',
  'website/scripts/sync-factorial.mjs',
  'examples/functions/factorial.jz',
  'scripts/example-cases.tsv',
  ...codeRenderingSources,
];

export const documentationSharedSources = [
  'website/docusaurus.config.ts',
  'website/src/seo/jsonLd.mjs',
  'website/src/theme/DocItem/Layout',
  ...codeRenderingSources,
];
