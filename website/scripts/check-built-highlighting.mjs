import {readdirSync, readFileSync} from 'node:fs';
import path from 'node:path';

const buildRoot = path.resolve(
  process.argv[2] ?? path.join(import.meta.dirname, '..', 'build'),
);

function htmlFiles(directory) {
  return readdirSync(directory, {withFileTypes: true}).flatMap((entry) => {
    const entryPath = path.join(directory, entry.name);
    if (entry.isDirectory()) {
      return htmlFiles(entryPath);
    }
    return entry.isFile() && entry.name.endsWith('.html') ? [entryPath] : [];
  });
}

const jazzBlocks = htmlFiles(buildRoot).flatMap((file) => {
  const html = readFileSync(file, 'utf8');
  return [
    ...html.matchAll(
      /<pre[^>]*data-jazz-highlighter="textmate"[^>]*>[\s\S]*?<\/pre>/g,
    ),
  ].map(([block]) => block);
});

if (jazzBlocks.length === 0) {
  throw new Error('production build contains no TextMate-highlighted Jazz block');
}

const colors = new Set(
  jazzBlocks.flatMap((block) =>
    [...block.matchAll(/(?:style="|;)color:([^;"<]+)/g)].map(
      (match) => match[1],
    ),
  ),
);

if (colors.size < 4) {
  throw new Error(
    `production Jazz blocks use ${colors.size} token colors; expected at least 4`,
  );
}

console.log(
  `Jazz highlighting check passed (${jazzBlocks.length} blocks, ${colors.size} token colors).`,
);
