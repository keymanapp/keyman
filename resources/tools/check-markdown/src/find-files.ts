import * as fs from 'node:fs';

export function findFiles(root: string) {
  const files: string[] = fs.readdirSync(root, {
    recursive: true, encoding: 'utf-8'
  }).map(file => file.replace(/\\/g, '/'));

  return files;
}
