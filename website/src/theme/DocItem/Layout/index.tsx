import React, {type ReactNode} from 'react';
import clsx from 'clsx';
import {useDoc} from '@docusaurus/plugin-content-docs/client';
import {useWindowSize} from '@docusaurus/theme-common';
import ContentVisibility from '@theme/ContentVisibility';
import DocBreadcrumbs from '@theme/DocBreadcrumbs';
import DocItemContent from '@theme/DocItem/Content';
import DocItemFooter from '@theme/DocItem/Footer';
import type {Props} from '@theme/DocItem/Layout';
import DocItemPaginator from '@theme/DocItem/Paginator';
import DocItemTOCDesktop from '@theme/DocItem/TOC/Desktop';
import DocItemTOCMobile from '@theme/DocItem/TOC/Mobile';
import DocVersionBadge from '@theme/DocVersionBadge';
import DocVersionBanner from '@theme/DocVersionBanner';

import styles from './styles.module.css';

function useDocTOC() {
  const {frontMatter, toc} = useDoc();
  const windowSize = useWindowSize();
  const hidden = frontMatter.hide_table_of_contents;
  const canRender = !hidden && toc.length > 0;

  return {
    mobile: canRender ? <DocItemTOCMobile /> : undefined,
    desktop:
      canRender && (windowSize === 'desktop' || windowSize === 'ssr') ? (
        <DocItemTOCDesktop />
      ) : undefined,
  };
}

export default function DocItemLayout({children}: Props): ReactNode {
  const docTOC = useDocTOC();
  const {metadata} = useDoc();

  return (
    <div className={clsx('row', styles.docRow)}>
      <div className={clsx('col', docTOC.desktop && styles.docItemCol)}>
        <ContentVisibility metadata={metadata} />
        <DocVersionBanner />
        <div className={styles.docItemContainer}>
          <article>
            <DocBreadcrumbs />
            <DocVersionBadge />
            {docTOC.mobile}
            <DocItemContent>{children}</DocItemContent>
            <DocItemFooter />
          </article>
          <DocItemPaginator />
        </div>
      </div>
      {docTOC.desktop ? (
        <aside className={clsx('col col--3', styles.tocColumn)} aria-label="On this page">
          {docTOC.desktop}
        </aside>
      ) : null}
    </div>
  );
}
