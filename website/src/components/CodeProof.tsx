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
    <figure className={styles.codeFigure} aria-label="Factorial example">
      <CodeBlock language="jazz" title="factorial.jz">
        {factorialSource}
      </CodeBlock>
      <figcaption>
        <code>{factorialInvocation}</code>
        <span aria-label="Result">{factorialExpectedOutput}</span>
      </figcaption>
    </figure>
  );
}
