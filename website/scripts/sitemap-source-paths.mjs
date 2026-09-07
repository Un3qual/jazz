const codeRenderingSources = [
  'website/src/theme/CodeBlock',
  'website/scripts/jazz-highlighter.mjs',
  'website/scripts/jazz-signature-metadata.mjs',
  'website/scripts/jazz-type-links.mjs',
  'editors/vscode-jazz/syntaxes/jazz.tmLanguage.json',
];

const globalRenderingSources = [
  'website/sidebars.ts',
  'website/src/css/custom.css',
  'website/src/theme/Navbar/Logo',
  'website/src/theme/SearchBar',
  'website/scripts/pagefind-search-model.mjs',
  'website/package.json',
  'website/pnpm-lock.yaml',
];

export const homepageSources = [
  'website/docusaurus.config.ts',
  ...globalRenderingSources,
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
  ...globalRenderingSources,
  'website/src/seo/jsonLd.mjs',
  'website/src/theme/DocItem/Layout',
  ...codeRenderingSources,
];

export const documentationNavigationGroups = [
  {
    routes: ['', 'getting-started', 'language', 'compiler', 'project'],
    sources: [
      'docs/index.md',
      'docs/getting-started',
      'docs/language',
      'docs/compiler',
      'docs/project',
    ],
  },
  {
    routes: ['standard-library'],
    sources: ['docs/standard-library'],
  },
  {
    routes: ['reference'],
    sources: ['docs/reference'],
  },
];
