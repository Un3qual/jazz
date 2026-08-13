export interface JazzMarkdownNode {
  type: string;
  value?: string;
  lang?: string;
  meta?: string;
}

export interface JazzMarkdownTree {
  children?: JazzMarkdownNode[];
}

export function addJazzSignatureMetadata(tree: JazzMarkdownTree): JazzMarkdownTree;

export default function remarkJazzSignatures(): (
  tree: JazzMarkdownTree,
) => JazzMarkdownTree;
