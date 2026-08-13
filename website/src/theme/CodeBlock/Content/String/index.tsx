import {type ReactNode} from 'react';
import {useThemeConfig} from '@docusaurus/theme-common';
import {
  CodeBlockContextProvider,
  type CodeBlockMetadata,
  createCodeBlockMetadata,
  useCodeWordWrap,
} from '@docusaurus/theme-common/internal';
import type {Props} from '@theme/CodeBlock/Content/String';
import CodeBlockLayout from '@theme/CodeBlock/Layout';

export interface JazzCodeBlockMetadata extends CodeBlockMetadata {
  jazzSignature: boolean;
}

function hasJazzSignature(metastring: string | undefined): boolean {
  return metastring?.split(/\s+/).includes('jazz-signature') ?? false;
}

function useCodeBlockMetadata(props: Props): JazzCodeBlockMetadata {
  const {prism} = useThemeConfig();
  return {
    ...createCodeBlockMetadata({
      code: props.children,
      className: props.className,
      metastring: props.metastring,
      magicComments: prism.magicComments,
      defaultLanguage: prism.defaultLanguage,
      language: props.language,
      title: props.title,
      showLineNumbers: props.showLineNumbers,
    }),
    jazzSignature: hasJazzSignature(props.metastring),
  };
}

export default function CodeBlockString(props: Props): ReactNode {
  const metadata = useCodeBlockMetadata(props);
  const wordWrap = useCodeWordWrap();
  return (
    <CodeBlockContextProvider metadata={metadata} wordWrap={wordWrap}>
      <CodeBlockLayout />
    </CodeBlockContextProvider>
  );
}
