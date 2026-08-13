import type {SidebarsConfig} from '@docusaurus/plugin-content-docs';

const sidebars: SidebarsConfig = {
  learnSidebar: [
    'index',
    {
      type: 'category',
      label: 'Getting started',
      items: [
        'getting-started/overview',
        'getting-started/installation',
        'getting-started/first-program',
        'getting-started/cli',
      ],
    },
    {
      type: 'category',
      label: 'Language',
      items: [
        'language/overview',
        'language/source-and-blocks',
        'language/bindings-and-functions',
        'language/types-and-signatures',
        'language/algebraic-data-types-and-patterns',
        'language/control-flow',
        'language/modules',
        'language/operators',
        'language/capabilities',
        'language/purity',
      ],
    },
    {
      type: 'category',
      label: 'Compiler',
      items: [
        'compiler/architecture',
        'compiler/pipeline',
        'compiler/bootstrapping',
      ],
    },
    {
      type: 'category',
      label: 'Project',
      items: [
        'project/status',
        'project/roadmap',
        'project/governance',
        'project/contributing',
      ],
    },
  ],
  standardLibrarySidebar: [
    'standard-library/overview',
    'standard-library/prelude',
    {
      type: 'category',
      label: 'Data',
      collapsed: false,
      items: [
        'standard-library/maybe',
        'standard-library/result',
        'standard-library/nonempty',
      ],
    },
    {
      type: 'category',
      label: 'Collections',
      collapsed: false,
      items: [
        'standard-library/list',
        'standard-library/dictionary',
        'standard-library/queue',
        'standard-library/map',
        'standard-library/set',
      ],
    },
    {
      type: 'category',
      label: 'Text',
      collapsed: false,
      items: ['standard-library/char', 'standard-library/text'],
    },
    {
      type: 'category',
      label: 'System',
      collapsed: false,
      items: ['standard-library/io', 'standard-library/io-error'],
    },
  ],
  referenceSidebar: [
    'reference/lexical-grammar',
    'reference/expression-grammar',
    'reference/module-resolution',
    'reference/cli',
    'reference/diagnostics',
    'reference/runtime-values',
  ],
};

export default sidebars;
