import type {CSSProperties, ReactNode} from 'react';
import clsx from 'clsx';
import {useColorMode} from '@docusaurus/theme-common';
import {useCodeBlockContext} from '@docusaurus/theme-common/internal';
import OriginalCodeBlockContent from '@theme-original/CodeBlock/Content';
import Line from '@theme/CodeBlock/Line';
import type {Props} from '@theme/CodeBlock/Content';
import type {
  LineInputProps,
  LineOutputProps,
  Token,
  TokenInputProps,
  TokenOutputProps,
} from 'prism-react-renderer';
import type {ThemedToken} from 'shiki/types';

import {tokenizeJazz} from '../../../../scripts/jazz-highlighter.mjs';
import styles from './styles.module.css';

interface JazzToken extends Token {
  color?: string;
  fontStyle?: number;
}

function toJazzToken(token: ThemedToken): JazzToken {
  return {
    types: ['plain'],
    content: token.content,
    color: token.color,
    fontStyle: token.fontStyle,
  };
}

function getLineProps({className, style}: LineInputProps): LineOutputProps {
  return {className: className ?? '', style};
}

function getTokenProps({token}: TokenInputProps): TokenOutputProps {
  const {color, content, fontStyle = 0} = token as JazzToken;
  const decoration = [
    fontStyle & 4 ? 'underline' : undefined,
    fontStyle & 8 ? 'line-through' : undefined,
  ]
    .filter(Boolean)
    .join(' ');

  return {
    className: '',
    children: content,
    style: {
      color,
      fontStyle: fontStyle & 1 ? 'italic' : undefined,
      fontWeight: fontStyle & 2 ? 'bold' : undefined,
      textDecorationLine: decoration || undefined,
    },
  };
}

export default function CodeBlockContent(props: Props): ReactNode {
  const {metadata, wordWrap} = useCodeBlockContext();
  const {colorMode} = useColorMode();

  if (metadata.language !== 'jazz') {
    return <OriginalCodeBlockContent {...props} />;
  }

  const {bg, fg, tokens} = tokenizeJazz(metadata.code, colorMode);
  const lines = tokens.map((line) => line.map(toJazzToken));
  const codeStyle: CSSProperties = {
    counterReset:
      metadata.lineNumbersStart === undefined
        ? undefined
        : `line-count ${metadata.lineNumbersStart - 1}`,
  };

  return (
    <pre
      ref={wordWrap.codeBlockRef}
      tabIndex={0}
      data-jazz-highlighter="textmate"
      className={clsx(
        props.className,
        metadata.className,
        styles.codeBlock,
        'prism-code',
        'thin-scrollbar',
      )}
      style={{backgroundColor: bg, color: fg}}>
      <code
        className={clsx(
          styles.codeBlockLines,
          metadata.lineNumbersStart !== undefined &&
            styles.codeBlockLinesWithNumbering,
        )}
        style={codeStyle}>
        {lines.map((line, index) => (
          <Line
            key={index}
            line={line}
            getLineProps={getLineProps}
            getTokenProps={getTokenProps}
            classNames={metadata.lineClassNames[index]}
            showLineNumbers={metadata.lineNumbersStart !== undefined}
          />
        ))}
      </code>
    </pre>
  );
}
