import type {ReactNode} from 'react';
import CodeBlock from '@theme/CodeBlock';

import {factorialSource} from '../generated/factorial';
import styles from '../pages/index.module.css';

export default function CodeProof(): ReactNode {
  return (
    <section className={styles.proof} aria-labelledby="proof-heading">
      <div className={styles.sectionEyebrow}>Checked example · 720</div>
      <div className={styles.proofLayout}>
        <div className={styles.proofCopy}>
          <h2 id="proof-heading">Readable from signature to result.</h2>
          <p>
            This program is synchronized directly from the repository example
            and verified by the compiler test suite.
          </p>
        </div>
        <figure className={styles.codeFigure}>
          <CodeBlock language="jazz" title="factorial.jz">
            {factorialSource}
          </CodeBlock>
          <figcaption>
            <span>factorial 6</span>
            <strong>720</strong>
          </figcaption>
        </figure>
      </div>
    </section>
  );
}
