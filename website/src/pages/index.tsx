import type {ReactNode} from 'react';
import Layout from '@theme/Layout';

import CodeProof from '../components/CodeProof';
import EditorialBand from '../components/EditorialBand';
import HomepageFooterCta from '../components/HomepageFooterCta';
import HomepageHeader from '../components/HomepageHeader';
import styles from './index.module.css';

export default function Home(): ReactNode {
  return (
    <Layout
      title="A statically typed functional language"
      description="Jazz is a statically typed functional language with practical syntax.">
      <div className={styles.page}>
        <HomepageHeader />
        <main className={styles.main}>
          <CodeProof />
          <section className={styles.depth} aria-labelledby="depth-heading">
            <div className={styles.depthIntro}>
              <p className={styles.sectionEyebrow}>Language, in three movements</p>
              <h2 id="depth-heading">Strong ideas. Clear notation.</h2>
            </div>
            <EditorialBand
              index="01"
              title="Types that stay readable"
              links={[
                {label: 'Types and signatures', to: '/docs/language/types-and-signatures'},
              ]}>
              Inference keeps the common path light; signatures make public intent
              explicit.
            </EditorialBand>
            <EditorialBand
              index="02"
              title="Composition without ceremony"
              links={[
                {label: 'Bindings and functions', to: '/docs/language/bindings-and-functions'},
                {
                  label: 'Data and patterns',
                  to: '/docs/language/algebraic-data-types-and-patterns',
                },
              ]}>
              Functions, algebraic data, and ordered patterns make behavior visible
              at the point of use.
            </EditorialBand>
            <EditorialBand
              index="03"
              title="A compiler growing into Jazz"
              links={[
                {label: 'Compiler architecture', to: '/docs/compiler/architecture'},
                {label: 'Bootstrapping', to: '/docs/compiler/bootstrapping'},
              ]}>
              A tested Haskell foundation supports a deliberate path toward hosted
              compiler stages.
            </EditorialBand>
          </section>
          <HomepageFooterCta />
        </main>
      </div>
    </Layout>
  );
}
