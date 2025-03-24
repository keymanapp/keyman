import * as fs from 'node:fs';
import { posix as path } from 'node:path';

import { marked, Token } from 'marked';

import { type LinkRef } from './types.js';

let refLinks: any[] = [];

const walkTokens = (token: Token) => {
  if (token.type === 'link' || token.type === 'image') {
    refLinks.push(token);
  }
};

marked.use( { walkTokens });

export function parseFiles(root: string, files: string[]): LinkRef[] {
  const links: LinkRef[] = [];
  for (const file of files) {
    const fullPath = path.join(root, file);
    if (fs.statSync(fullPath).isFile() && fullPath.endsWith('.md')) {
      refLinks = [];
      marked.parse(fs.readFileSync(fullPath, 'utf-8'));
      links.push({ file, links: refLinks, messages: [] });
    }
  }

  return links;
}
