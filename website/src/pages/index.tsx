import type {ReactNode} from 'react';
import Link from '@docusaurus/Link';
import Layout from '@theme/Layout';

import styles from './index.module.css';

export default function Home(): ReactNode {
  return (
    <Layout description="Documentation for the Jazz programming language">
      <main className={styles.main}>
        <h1>Jazz</h1>
        <p>A statically typed functional language with practical syntax</p>
        <Link to="/docs/getting-started/overview">Read the documentation</Link>
      </main>
    </Layout>
  );
}
