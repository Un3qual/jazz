import {type ReactNode} from 'react';
import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';
import clsx from 'clsx';
import type {Token} from 'prism-react-renderer';
import type {Props} from '@theme/CodeBlock/Line/Token';

import styles from './styles.module.css';

interface JazzToken extends Token {
  destination?: string;
}

export default function CodeBlockLineToken({
  line,
  token,
  ...props
}: Props): ReactNode {
  const destination = (token as JazzToken).destination;
  const href = useBaseUrl(destination ?? '/');

  if (!destination) {
    return <span {...props} />;
  }

  return (
    <Link
      {...props}
      to={href}
      data-jazz-type-link="true"
      className={clsx(props.className, styles.typeLink)}
    />
  );
}
