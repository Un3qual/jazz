import type {ReactNode} from 'react';
import clsx from 'clsx';
import useBaseUrl from '@docusaurus/useBaseUrl';

import styles from '../pages/index.module.css';

type BrandMarkProps = {
  className?: string;
};

export default function BrandMark({className}: BrandMarkProps): ReactNode {
  return (
    <div className={clsx(styles.brandPlane, className)} aria-hidden="true">
      <img
        className={styles.brandMark}
        src={useBaseUrl('/img/jazz-mark-dark.svg')}
        alt=""
      />
    </div>
  );
}
