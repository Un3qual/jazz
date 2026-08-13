import type {ReactNode} from 'react';
import Link from '@docusaurus/Link';

import styles from '../pages/index.module.css';

const sections = [
  {
    title: 'Learn',
    description: 'Run a program, then learn how Jazz evaluates and organizes code.',
    links: [
      {label: 'Getting started', to: '/docs/getting-started/overview'},
      {label: 'First program', to: '/docs/getting-started/first-program'},
      {label: 'Language guide', to: '/docs/language/overview'},
    ],
  },
  {
    title: 'Standard Library',
    description: 'Look up types and functions in Prelude and the bundled modules.',
    links: [
      {label: 'Module index', to: '/docs/standard-library/overview'},
      {label: 'Prelude', to: '/docs/standard-library/prelude'},
      {label: 'List', to: '/docs/standard-library/list'},
    ],
  },
  {
    title: 'Reference',
    description: 'Check exact language, runtime, command-line, and diagnostic rules.',
    links: [
      {label: 'Expression grammar', to: '/docs/reference/expression-grammar'},
      {label: 'CLI reference', to: '/docs/reference/cli'},
      {label: 'Diagnostics', to: '/docs/reference/diagnostics'},
    ],
  },
  {
    title: 'Compiler',
    description: 'See how a program moves from source to analysis and execution.',
    links: [
      {label: 'Architecture', to: '/docs/compiler/architecture'},
      {label: 'Pipeline', to: '/docs/compiler/pipeline'},
      {label: 'Bootstrapping', to: '/docs/compiler/bootstrapping'},
    ],
  },
  {
    title: 'Project',
    description: 'Check current capabilities, planned work, and contribution guidance.',
    links: [
      {label: 'Current status', to: '/docs/project/status'},
      {label: 'Roadmap', to: '/docs/project/roadmap'},
      {label: 'Contributing', to: '/docs/project/contributing'},
    ],
  },
] as const;

export default function DocumentationDirectory(): ReactNode {
  return (
    <section className={styles.directory} aria-labelledby="documentation-heading">
      <div className={styles.directoryHeading}>
        <h2 id="documentation-heading">Documentation</h2>
      </div>
      <div className={styles.directoryGrid}>
        {sections.map(({title, description, links}) => (
          <section className={styles.directorySection} key={title}>
            <h3>{title}</h3>
            <p>{description}</p>
            <ul>
              {links.map(({label, to}) => (
                <li key={to}>
                  <Link className={styles.textLink} to={to}>
                    {label}
                  </Link>
                </li>
              ))}
            </ul>
          </section>
        ))}
      </div>
    </section>
  );
}
