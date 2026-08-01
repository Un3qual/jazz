import type {ReactNode} from 'react';
import Link from '@docusaurus/Link';

import styles from '../pages/index.module.css';

export default function HomepageFooterCta(): ReactNode {
  return (
    <section className={styles.closing} aria-labelledby="closing-heading">
      <div className={styles.closingSection}>
        <p className={styles.sectionEyebrow}>The next phrase is yours</p>
        <h2 id="closing-heading">Build Jazz. Follow its progress.</h2>
        <div className={styles.closingLinks}>
          <Link className={styles.primaryAction} to="/docs/getting-started/installation">
            Build the compiler
          </Link>
          <Link className={styles.textLink} to="/docs/project/status">
            Read current status
          </Link>
        </div>
      </div>
    </section>
  );
}
