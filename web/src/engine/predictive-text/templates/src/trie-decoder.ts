import { type Wordform2Key } from './common.js';
import { decompressNode } from './trie-compression.js';
import { type InternalNode } from './trie.js';

export function buildDecoder(encodedRoot: string, searchTermKeyer: Wordform2Key) {
  const root = decompressNode(encodedRoot, searchTermKeyer, 0) as InternalNode;
  retrofitInternalNode(root, searchTermKeyer);

  return root;
}

function retrofitInternalNode(node: InternalNode, searchTermKeyer: Wordform2Key) {
  const originalChildren = node.children;

  node.children = {};
  const children = node.children;
  for(let char of node.values) {
    const child = originalChildren[char] as string;
    Object.defineProperty(children, char, {
      configurable: true,
      get: (() => {
        const childNode = decompressNode(child, searchTermKeyer, 0);

        if(childNode.type == 'internal') {
          retrofitInternalNode(childNode, searchTermKeyer);
        }

        Object.defineProperty(children, char, {
          value: childNode
        })
        return childNode;
      })
    });
  }
}