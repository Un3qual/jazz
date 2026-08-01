import type {ReactNode} from 'react';
import Link from '@docusaurus/Link';

import BrandMark from './BrandMark';
import styles from '../pages/index.module.css';

export default function HomepageHeader(): ReactNode {
  return (
    <header className={styles.heroHeader}>
      <section className={styles.hero} aria-labelledby="jazz-home-title">
        <div className={styles.heroCopy}>
          <p className={styles.heroKicker}>Jazz programming language</p>
          <h1 id="jazz-home-title">Jazz</h1>
          <p className={styles.heroPromise}>
            A statically typed functional language with practical syntax.
          </p>
          <div className={styles.heroActions}>
            <Link
              className={styles.primaryAction}
              to="/docs/getting-started/overview">
              Get started
            </Link>
            <Link className={styles.secondaryAction} to="/docs/language/overview">
              Read the language guide
            </Link>
          </div>
        </div>
        <BrandMark />
      </section>
    </header>
  );
}
