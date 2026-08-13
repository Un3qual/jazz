import type {ReactNode} from 'react';
import useBaseUrl from '@docusaurus/useBaseUrl';

import styles from '../pages/index.module.css';

export default function BrandMark(): ReactNode {
  return (
    <div className={styles.brandPlane} aria-hidden="true">
      <img
        className={styles.brandMark}
        src={useBaseUrl('/img/jazz-mark-dark.svg')}
        alt=""
      />
    </div>
  );
}
