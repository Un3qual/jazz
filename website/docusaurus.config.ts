import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';

const config: Config = {
  title: 'Jazz',
  tagline: 'A statically typed functional language with practical syntax',
  url: 'https://un3qual.github.io',
  baseUrl: '/jazz/',
  organizationName: 'un3qual',
  projectName: 'jazz',
  trailingSlash: false,
  onBrokenLinks: 'throw',
  markdown: {
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
        },
        blog: false,
        theme: {
          customCss: './src/css/custom.css',
        },
      } satisfies Preset.Options,
    ],
  ],
  themeConfig: {
    colorMode: {
      respectPrefersColorScheme: true,
    },
    navbar: {
      title: 'Jazz',
      items: [
        {
          type: 'docSidebar',
          sidebarId: 'jazzSidebar',
          label: 'Docs',
          position: 'left',
        },
        {
          to: '/docs/language/overview',
          label: 'Language',
          position: 'left',
        },
        {
          to: '/docs/standard-library/overview',
          label: 'Standard Library',
          position: 'left',
        },
        {
          to: '/docs/project/status',
          label: 'Status',
          position: 'left',
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
