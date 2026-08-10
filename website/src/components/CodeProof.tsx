import type {ReactNode} from 'react';
import CodeBlock from '@theme/CodeBlock';

import {
  factorialExpectedOutput,
  factorialInvocation,
  factorialSource,
} from '../generated/factorial';
import styles from '../pages/index.module.css';

export default function CodeProof(): ReactNode {
  return (
    <section className={styles.proof} aria-labelledby="proof-heading">
      <div className={styles.sectionEyebrow}>
        Checked example · {factorialExpectedOutput}
      </div>
      <div className={styles.proofLayout}>
        <div className={styles.proofCopy}>
          <h2 id="proof-heading">Readable from signature to result.</h2>
          <p>
            This program is synchronized directly from the repository example
            and verified by the compiler-backed example check.
          </p>
        </div>
        <figure className={styles.codeFigure}>
          <CodeBlock language="jazz" title="factorial.jz">
            {factorialSource}
          </CodeBlock>
          <figcaption>
            <span>{factorialInvocation}</span>
            <strong>{factorialExpectedOutput}</strong>
          </figcaption>
        </figure>
      </div>
    </section>
  );
}
