import type {ReactNode} from 'react';
import Layout from '@theme/Layout';

import DocumentationDirectory from '../components/DocumentationDirectory';
import HomepageHeader from '../components/HomepageHeader';
import styles from './index.module.css';

export default function Home(): ReactNode {
  return (
    <Layout
      title="A statically typed functional language"
      description="Jazz is a statically typed functional language with practical syntax.">
      <div className={styles.page}>
        <HomepageHeader />
        <main className={styles.main}>
          <DocumentationDirectory />
        </main>
      </div>
    </Layout>
  );
}
