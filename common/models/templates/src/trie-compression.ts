import { Entry, InternalNode, Leaf, Node } from "./trie.js";

// const SINGLE_CHAR_RANGE = Math.pow(2, 16) - 64;
// const ENCODED_NUM_BASE = 64;

// Offsetting by even just 0x0020 avoids control-code chars + avoids VS Code not liking the encoding.
const ENCODED_NUM_BASE = 0x0000;
const SINGLE_CHAR_RANGE = Math.pow(2, 16) - ENCODED_NUM_BASE;

const WEIGHT_WIDTH = 2;
const NODE_SIZE_WIDTH = 2;

export function decompressNumber(str: string, start: number, end: number) {
  end ??= str.length;
  let num = 0;

  for(let i = start; i < end; i++) {
    let val = str.charCodeAt(i);
    num = num * SINGLE_CHAR_RANGE + val - ENCODED_NUM_BASE;
  }

  return num;
}

export function compressNumber(num: number, width?: number) {
  let compressed = '';
  width ||= 1;

  // Note:  JS bit-shift operators assume 32-bit signed ints.
  // JS numbers can easily represent larger ints, though.
  while(width > 0) {
    const piece = num % SINGLE_CHAR_RANGE + ENCODED_NUM_BASE;
    num = Math.floor(num / SINGLE_CHAR_RANGE);

    compressed = String.fromCharCode(piece) + compressed;

    width--;
  }

  if(num) {
    throw new Error(`Could not properly compress ${num} within specified char width of ${width}`);
  }

  return compressed;
}

const ENTRY_HEADER_WIDTH = NODE_SIZE_WIDTH + WEIGHT_WIDTH;

// encoded ENTRY:
// - entryLen: 2 char
//   - total encoded size of all following bits.
//   - as raw, concatenated string data - no JSON.stringify action taken.
// - weight: 2? chars (with quote-offset on each char?)
// - contentLen: safe to infer from all other values
// - content: string: from [header+4] to [header+4 + contentLen - 1]
//
// - key: not encoded; we can regenerate it via the keying function

export function compressEntry(entry: Entry): string {
  const { content, weight } = entry;

  const weightEnc = compressNumber(weight, WEIGHT_WIDTH);

  const entryLenEnc = compressNumber(content.length + ENTRY_HEADER_WIDTH, 2);

  return `${entryLenEnc}${weightEnc}${content}`;
}

export function decompressEntry(str: string, keyFunction: (val: string) => string, baseIndex: number): Entry {
  baseIndex ||= 0;

  const entryLen = decompressNumber(str, baseIndex + 0, baseIndex + NODE_SIZE_WIDTH);
  // c8 ignore start
  if(str.length < baseIndex + entryLen) {
    throw new Error('Parts of the encoded entry are missing');
  }
  // c8 ignore end

  const headerEnd = baseIndex + ENTRY_HEADER_WIDTH;
  const weight = decompressNumber(str, baseIndex + NODE_SIZE_WIDTH, headerEnd);
  const content = str.substring(headerEnd, baseIndex + entryLen);

  return {
    key: keyFunction(content) as any, // needed due to special `SearchKey` type shenanigans in its definition.
    content: content,
    weight: weight
  }
}


// BOTH node types:
// totalLen: 2 chars (fixed position, size) - size of the encoded node.
// weight: number - could be encoded.
// - 2^32 ~= 4*10^9, representable in 2 chars... if it weren't for `"`-escaping.
// - 2^64 ~= 1.8*10^19 - surely far, far more than enough.
// Next char:  indicates BOTH a flag of something and a high-bit indicating 'leaf' or 'internal'.
// - function of other bits will be indicated by their sections.

export const NODE_TYPE_INDEX = NODE_SIZE_WIDTH + WEIGHT_WIDTH;

export function compressNode(node: Node) {
  let encodedSpecifics = node.type == 'leaf' ? compressLeaf(node) : compressInternal(node);
  let weightEnc = compressNumber(node.weight, WEIGHT_WIDTH);
  let charLength = encodedSpecifics.length + NODE_SIZE_WIDTH + WEIGHT_WIDTH;

  return `${compressNumber(charLength, 2)}${weightEnc}${encodedSpecifics}`;
}

export function decompressNode(str: string, baseIndex: number) {
  baseIndex ||= 0;

  const entryLen = decompressNumber(str, baseIndex + 0, baseIndex + NODE_SIZE_WIDTH);
  // c8 ignore start
  if(str.length < baseIndex + entryLen) {
    throw new Error('Parts of the encoded node are missing');
  }
  // c8 ignore end

  const typeFlagSrc = decompressNumber(str, baseIndex + NODE_TYPE_INDEX, baseIndex + NODE_TYPE_INDEX + 1);
  const isLeafType = typeFlagSrc & 0x8000;

  return isLeafType ? decompressLeaf(str, baseIndex) : decompressInternal(str, baseIndex);
}

// encoded LEAF:
// - BOTH-section header
// - entriesCnt: 1 char (fixed position)
//   - type flag overlaps here - high bit of the representing char should be one.
// - entries: full encoding of all contained entries.

function compressLeaf(leaf: Leaf): string {
  const entries = leaf.entries;

  // key, content, weight - per entry
  const entryCntAndType = entries.length | 0x8000;
  // c8 ignore start
  if(entries.length >= 0x8000) {
    throw new Error("Cannot encode leaf:  too many direct entries");
  }
  // c8 ignore end
  let compressedEntries = [compressNumber(entryCntAndType)].concat(entries.map((entry) => {
    // if already compressed,  no need to recompress it.
    return typeof entry == 'string' ? entry : compressEntry(entry);
  }));

  return compressedEntries.join('');
}

function decompressLeaf(str: string, baseIndex: number): Omit<Leaf, 'entries'> & {entries: string[]} {
  const weight = decompressNumber(str, baseIndex + NODE_SIZE_WIDTH, baseIndex + NODE_SIZE_WIDTH + WEIGHT_WIDTH);

  // Assumes string-subsection size check has passed.
  const entryCntSrc = decompressNumber(str, baseIndex + NODE_TYPE_INDEX, baseIndex + NODE_TYPE_INDEX + 1);
  // Remove the type-flag bit indicating 'leaf node'.
  const entryCnt = entryCntSrc & 0x007F;

  let compressedEntries: string[] = [];
  baseIndex = baseIndex + NODE_TYPE_INDEX + 1;
  for(let i = 0; i < entryCnt; i++) {
    const entryWidth = decompressNumber(str, baseIndex, baseIndex+2);
    const nextIndex = baseIndex + entryWidth;
    compressedEntries.push(str.substring(baseIndex, nextIndex));
    baseIndex = nextIndex;
  }

  return {
    type: 'leaf',
    weight: weight,

    // To consider:  is it better to just make a 'lazy span' against the original string?
    // - would use more memory, especially once a Trie is "mostly" decompressed
    //   - current approach 'discards' decoded parts of the original string.
    // - would likely decompress a bit faster.
    entries: compressedEntries
  }
}

// encoded INTERNAL:
// - BOTH-section header
// - valLen: 1 char (fixed position, size) - we shouldn't ever have an array of > 65000, right?
//   - is also the count for children.
//   - type flag overlaps here - high bit of the representing char should be zero.
// - values: string
//   - ezpz - they're already single-char strings.
// - children:  full encoding of next-layer nodes.  Same order as the entries are found within `values`.
//   - that is, no need to insert keys.
//   - first two bits:  length of following bits... so can use that to calculate offset for next entry's
//     encoding start.

function compressInternal(node: InternalNode): string {
  let values = node.values;
  const valueCntAndType = values.length;
  // c8 ignore start
  if(valueCntAndType >= 0x8000) {
    throw new Error("Cannot encode node:  too many direct children");
  }
  // c8 ignore end

  const compressedChildren = values.map((value) => {
    // BIG ERROR DETECTED:  lexical-model compiler is not emitting SENTINEL chars correctly
    // for _some_ cases!  '﷐' shows up for 'most', but not _all_, places where it belongs.
    // Approx 20% error rate!?
    //
    // STRONG SUSPICION: addItemToInternalNode
    // - current line 376:  let char = item.key[index];
    //   - does not validate that the char exists!
    //
    // detected via `sil.km.gcc - 1.0`.
    if(value === null) {
      value = "undefined"; // yes, really.
    }
    const child = node.children[value];

    // c8 ignore start
    if(!child) {
      throw new Error("unexpected empty reference for child");
    }
    // c8 ignore end

    // No need to recompress it if it's already compressed.
    return typeof child == 'string' ? child : compressNode(child);
  });

  // Properly fix the sentinel-value issue.
  values = values.map((value) => value === null ? '\ufdd0' : value);

  const totalArr = [compressNumber(valueCntAndType)].concat(values).concat(compressedChildren);
  return totalArr.join('');
}

function decompressInternal(str: string, baseIndex: number): Omit<InternalNode, 'children'> & { children: {[char: string]: string} } {
  const weight = decompressNumber(str, baseIndex + NODE_SIZE_WIDTH, baseIndex + NODE_SIZE_WIDTH + WEIGHT_WIDTH);

  // Assumes string-subsection size check has passed.
  const childCnt = decompressNumber(str, baseIndex + NODE_TYPE_INDEX, baseIndex + NODE_TYPE_INDEX + 1);

  baseIndex = baseIndex + NODE_TYPE_INDEX + 1;
  let nextIndex = baseIndex + childCnt;
  const values = str.substring(baseIndex, nextIndex).split('');
  baseIndex = nextIndex;

  let compressedChildren: {[char: string]: string} = {};
  for(let i = 0; i < childCnt; i++) {
    const childWidth = decompressNumber(str, baseIndex, baseIndex+2);
    nextIndex = baseIndex + childWidth;
    compressedChildren[values[i]] = str.substring(baseIndex, nextIndex);
    baseIndex = nextIndex;
  }

  return {
    type: 'internal',
    weight: weight,
    values: values,
    // To consider:  is it better to just make a 'lazy span' against the original string?
    // - would use more memory, especially once a Trie is "mostly" decompressed
    // - would likely decompress a bit faster.
    children: compressedChildren
  }
}

// Finally...
//
// encoded ROOT:
// - JSON.stringify(encoded INTERNAL form) - to be loadable in the file.
// - loads to a decoded InternalNode equivalent, but with still-encoded children.
// - upon a request _for_ the children, decodes them.
//   - they are requested as an array.
//   - fully decodes leaf nodes, does one decode layer for internals (stopping at still-encoded [grand]children)
