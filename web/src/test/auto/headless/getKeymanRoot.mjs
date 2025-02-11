import { createRequire } from 'module';
import * as path from 'path'
const require = createRequire(import.meta.url);

export function getKeymanRoot() {
  let anchorPath = require.resolve('@keymanapp/common-test-resources/index.mjs');
  let root = path.dirname(anchorPath);
  for(let i=0; i < 3; i++) {
    root = path.dirname(root);
  }
  return root;
}