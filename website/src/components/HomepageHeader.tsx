import type {ReactNode} from 'react';
import Link from '@docusaurus/Link';

import BrandMark from './BrandMark';
import CodeProof from './CodeProof';
import styles from '../pages/index.module.css';

export default function HomepageHeader(): ReactNode {
  return (
    <header className={styles.intro} aria-labelledby="jazz-home-title">
      <div className={styles.introInner}>
        <div className={styles.introCopy}>
          <div className={styles.titleLockup}>
            <BrandMark />
            <h1 className={styles.introTitle} id="jazz-home-title">
              Jazz
            </h1>
          </div>
          <p className={styles.introSummary}>
            A statically typed functional language for building programs from
            expressions and immutable values.
          </p>
          <p className={styles.introDetail}>
            Type inference keeps local code concise. Algebraic data types,
            pattern matching, modules, and capability constraints make program
            structure explicit.
          </p>
          <div className={styles.introActions}>
            <Link
              className={styles.primaryAction}
              to="/docs/getting-started/overview">
              Getting started
            </Link>
            <Link className={styles.secondaryAction} to="/docs/language/overview">
              Language guide
            </Link>
          </div>
        </div>
        <CodeProof />
      </div>
    </header>
  );
}
