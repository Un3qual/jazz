import type {SidebarsConfig} from '@docusaurus/plugin-content-docs';

const sidebars: SidebarsConfig = {
  jazzSidebar: [
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
      label: 'Standard library',
      items: [
        'standard-library/overview',
        'standard-library/prelude',
        'standard-library/list',
        'standard-library/maybe-result-nonempty',
        'standard-library/dictionary',
        'standard-library/queue',
        'standard-library/map-and-set',
        'standard-library/char-and-text',
        'standard-library/io',
      ],
    },
    {
      type: 'category',
      label: 'Reference',
      items: [
        'reference/lexical-grammar',
        'reference/expression-grammar',
        'reference/module-resolution',
        'reference/cli',
        'reference/diagnostics',
        'reference/runtime-values',
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
};

export default sidebars;
