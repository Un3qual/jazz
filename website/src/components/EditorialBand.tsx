import type {ReactNode} from 'react';
import Link from '@docusaurus/Link';

import styles from '../pages/index.module.css';

type EditorialBandProps = {
  index: string;
  title: string;
  children: ReactNode;
  links: ReadonlyArray<{label: string; to: string}>;
};

export default function EditorialBand({
  index,
  title,
  children,
  links,
}: EditorialBandProps): ReactNode {
  return (
    <article className={styles.editorialBand}>
      <span className={styles.bandIndex} aria-hidden="true">
        {index}
      </span>
      <div className={styles.bandCopy}>
        <h3>{title}</h3>
        <p>{children}</p>
      </div>
      <nav className={styles.bandLinks} aria-label={`${title} documentation`}>
        {links.map(({label, to}) => (
          <Link className={styles.textLink} to={to} key={to}>
            {label}
          </Link>
        ))}
      </nav>
    </article>
  );
}
