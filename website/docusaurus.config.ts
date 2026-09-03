import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';

import {latestGitDate, withSitemapLastmods} from './scripts/sitemap-lastmod.mjs';
import {
  documentationSharedSources,
  homepageSources,
} from './scripts/sitemap-source-paths.mjs';

const config: Config = {
  title: 'Jazz',
  titleDelimiter: '·',
  tagline: 'A statically typed functional programming language with practical syntax',
  favicon: 'img/favicon.svg',
  url: 'https://un3qual.github.io',
  baseUrl: '/jazz/',
  headTags: [
    {
      tagName: 'script',
      attributes: {
        async: true,
        src: 'https://www.googletagmanager.com/gtag/js?id=G-05ZC42S145',
      },
    },
    {
      tagName: 'script',
      attributes: {},
      innerHTML: `
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag('js', new Date());
        gtag('config', 'G-05ZC42S145');
      `,
    },
  ],
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
        },
        blog: false,
        sitemap: {
          changefreq: null,
          priority: null,
          lastmod: 'date',
          createSitemapItems: async ({defaultCreateSitemapItems, ...params}) => {
            const items = await defaultCreateSitemapItems(params);
            return withSitemapLastmods(
              items,
              'https://un3qual.github.io/jazz/',
              latestGitDate(__dirname, homepageSources),
              'https://un3qual.github.io/jazz/docs',
              latestGitDate(__dirname, documentationSharedSources),
            );
          },
        },
        theme: {
          customCss: './src/css/custom.css',
        },
      } satisfies Preset.Options,
    ],
  ],
  plugins: [
    [
      '@docusaurus/plugin-client-redirects',
      {
        redirects: [
          {
            from: '/docs/standard-library/maybe-result-nonempty',
            to: '/docs/standard-library/maybe',
          },
          {
            from: '/docs/standard-library/map-and-set',
            to: '/docs/standard-library/map',
          },
          {
            from: '/docs/standard-library/char-and-text',
            to: '/docs/standard-library/char',
          },
        ],
      },
    ],
  ],
  themeConfig: {
    image: 'img/social-card.png',
    metadata: [
      {
        name: 'description',
        content: 'Documentation for Jazz, an experimental, statically typed functional programming language.',
      },
      {property: 'og:site_name', content: 'Jazz programming language'},
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
