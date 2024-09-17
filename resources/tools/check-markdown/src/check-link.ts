import * as fs from 'node:fs';
import { posix as path } from 'node:path';

import { Tokens } from 'marked';

import { LinkRef, LinkRefMessage } from './types.js';

export function checkLinks(root: string, links: LinkRef[]) {
  let result = true;
  for(const file of links) {
    for(const link of file.links) {
      // verify that the file exists in filesystem at expected location
      result = checkLink(root, file.file, <Tokens.Link>link, file.messages) && result;
    }
  }
  return result;
}

function checkLink(root: string, file: string, token: Tokens.Link | Tokens.Image, messages: LinkRefMessage[]) {
  const parsed = token.href.split('#');
  const href = parsed[0];
  // const anchor = parsed.length > 1 ? parsed[1] : '';

  if(href.startsWith('https:') || href.startsWith('http:')) {
    messages.push({token, type:'info', message: 'External link'});
    return true;

  }
  if(href.startsWith('/')) {
    messages.push({token, type:'info', message: 'Absolute path'});
    return true;
  }

  const p = path.normalize(path.join(path.dirname(file), href));
  if(p.startsWith('../') || p == '..') {
    messages.push({token, type:'info', message: 'Relative path outside root'});
    return true;
  }

  let fullPath = path.join(root, p);
  if(token.type == 'link') {
    if(fs.existsSync(fullPath) && fs.statSync(fullPath).isDirectory()) {
      fullPath = path.join(fullPath, 'index.md');
    } else if(fullPath.endsWith('.md')) {
      messages.push({token, type:'warning', message: 'Link should not have a .md extension'});
    } else {
      fullPath = fullPath + '.md';
    }
  }

  if(!fs.existsSync(fullPath)) {
    fullPath = path.relative(root, fullPath);
    messages.push({token, type:'error', message: `Link target '${fullPath}' does not exist`});
    return false;
  }

  // TODO: check anchor
  return true;
}
