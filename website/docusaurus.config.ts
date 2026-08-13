import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';

import remarkJazzSignatures from './scripts/remark-jazz-signatures.mjs';

const config: Config = {
  title: 'Jazz',
  titleDelimiter: '·',
  tagline: 'A statically typed functional language with practical syntax',
  favicon: 'img/favicon.svg',
  url: 'https://un3qual.github.io',
  baseUrl: '/jazz/',
  organizationName: 'un3qual',
  projectName: 'jazz',
  trailingSlash: false,
  onBrokenLinks: 'throw',
  markdown: {
    format: 'md',
    hooks: {
      onBrokenMarkdownLinks: 'throw',
    },
  },
  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },
  presets: [
    [
      'classic',
      {
        docs: {
          path: '../docs',
          routeBasePath: 'docs',
          sidebarPath: './sidebars.ts',
          remarkPlugins: [remarkJazzSignatures],
        },
        blog: false,
        theme: {
          customCss: './src/css/custom.css',
        },
      } satisfies Preset.Options,
    ],
  ],
  themeConfig: {
    image: 'img/social-card.png',
    metadata: [
      {
        name: 'description',
        content: 'Documentation for Jazz, a statically typed functional language with practical syntax.',
      },
      {name: 'theme-color', content: '#171824'},
    ],
    colorMode: {
      defaultMode: 'light',
      respectPrefersColorScheme: true,
    },
    navbar: {
      logo: {
        alt: 'Jazz',
        src: 'img/jazz-wordmark.svg',
        srcDark: 'img/jazz-wordmark-dark.svg',
        width: 120,
        height: 48,
      },
      items: [
        {
          type: 'docSidebar',
          sidebarId: 'learnSidebar',
          label: 'Learn',
          position: 'left',
        },
        {
          to: '/docs/language/overview',
          label: 'Language',
          position: 'left',
        },
        {
          type: 'docSidebar',
          sidebarId: 'standardLibrarySidebar',
          label: 'Standard Library',
          position: 'left',
        },
        {
          type: 'docSidebar',
          sidebarId: 'referenceSidebar',
          label: 'Reference',
          position: 'left',
        },
        {
          type: 'search',
          position: 'right',
        },
        {
          href: 'https://github.com/un3qual/jazz',
          label: 'GitHub',
          position: 'right',
        },
      ],
    },
    footer: {
      style: 'dark',
      links: [
        {
          title: 'Learn',
          items: [
            {
              label: 'Getting Started',
              to: '/docs/getting-started/overview',
            },
            {
              label: 'Reference',
              to: '/docs/reference/expression-grammar',
            },
          ],
        },
        {
          title: 'Project',
          items: [
            {
              label: 'Roadmap',
              to: '/docs/project/roadmap',
            },
            {
              label: 'Contributing',
              to: '/docs/project/contributing',
            },
          ],
        },
        {
          title: 'Repository',
          items: [
            {
              label: 'GitHub',
              href: 'https://github.com/un3qual/jazz',
            },
            {
              label: 'Issues',
              href: 'https://github.com/un3qual/jazz/issues',
            },
            {
              label: 'Security',
              href: 'https://github.com/un3qual/jazz/security/policy',
            },
            {
              label: 'License',
              href: 'https://github.com/un3qual/jazz/blob/main/LICENSE',
            },
          ],
        },
      ],
      copyright: `Copyright © ${new Date().getFullYear()} Jazz contributors.`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
    },
  } satisfies Preset.ThemeConfig,
};

export default config;
