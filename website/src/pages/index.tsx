import type {ReactNode} from 'react';
import Head from '@docusaurus/Head';
import Layout from '@theme/Layout';

import DocumentationDirectory from '../components/DocumentationDirectory';
import HomepageHeader from '../components/HomepageHeader';
import styles from './index.module.css';
import {serializeJsonLd} from '../seo/jsonLd.mjs';

const title = 'Statically typed functional programming language';
const description =
  'Jazz is an experimental, statically typed functional programming language with type inference, immutable values, algebraic data types, and pattern matching.';
const structuredData = {
  '@context': 'https://schema.org',
  '@type': 'WebSite',
  name: 'Jazz programming language',
  alternateName: 'Jazz',
  url: 'https://un3qual.github.io/jazz/',
  description,
  inLanguage: 'en',
  sameAs: 'https://github.com/un3qual/jazz',
};

export default function Home(): ReactNode {
  return (
    <Layout title={title} description={description}>
      <Head>
        <meta property="og:type" content="website" />
        <script type="application/ld+json">
          {serializeJsonLd(structuredData)}
        </script>
      </Head>
      <div className={styles.page}>
        <HomepageHeader />
        <main className={styles.main}>
          <DocumentationDirectory />
        </main>
      </div>
    </Layout>
  );
}
