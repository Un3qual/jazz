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
import {
  getJazzTypeLinkSpans,
  type JazzTypeLinkSpan,
} from '../../../../scripts/jazz-type-links.mjs';
import styles from './styles.module.css';

interface JazzToken extends Token {
  color?: string;
  destination?: string;
  fontStyle?: number;
}

interface JazzSignatureMetadata {
  jazzSignature?: boolean;
}

function toJazzToken(token: ThemedToken): JazzToken {
  return {
    types: ['plain'],
    content: token.content,
    color: token.color,
    fontStyle: token.fontStyle,
  };
}

function splitJazzToken(
  token: ThemedToken,
  tokenStart: number,
  spans: JazzTypeLinkSpan[],
): JazzToken[] {
  if (token.content.length === 0) {
    return [toJazzToken(token)];
  }

  const tokenEnd = tokenStart + token.content.length;
  const intersecting = spans.filter(
    (span) => span.start < tokenEnd && span.end > tokenStart,
  );
  const boundaries = new Set([tokenStart, tokenEnd]);
  for (const span of intersecting) {
    boundaries.add(Math.max(tokenStart, span.start));
    boundaries.add(Math.min(tokenEnd, span.end));
  }

  const ordered = [...boundaries].sort((left, right) => left - right);
  return ordered.slice(0, -1).map((start, index) => {
    const end = ordered[index + 1]!;
    const destination = intersecting.find(
      (span) => span.start <= start && start < span.end,
    )?.destination;
    return {
      ...toJazzToken({
        ...token,
        content: token.content.slice(start - tokenStart, end - tokenStart),
      }),
      destination,
    };
  });
}

function toJazzSignatureLines(
  themedLines: ThemedToken[][],
  source: string,
): JazzToken[][] {
  const spans = getJazzTypeLinkSpans(source);
  let sourceOffset = 0;

  return themedLines.map((line, lineIndex) => {
    const linkedLine = line.flatMap((token) => {
      const split = splitJazzToken(token, sourceOffset, spans);
      sourceOffset += token.content.length;
      return split;
    });

    if (lineIndex < themedLines.length - 1) {
      sourceOffset += source.startsWith('\r\n', sourceOffset) ? 2 : 1;
    }
    return linkedLine;
  });
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
  const jazzSignature = (metadata as typeof metadata & JazzSignatureMetadata)
    .jazzSignature;
  const lines = jazzSignature
    ? toJazzSignatureLines(tokens, metadata.code)
    : tokens.map((line) => line.map(toJazzToken));
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
      data-jazz-signature={jazzSignature || undefined}
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
