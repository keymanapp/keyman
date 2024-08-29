import { List } from './list.js';
import { Strs } from './strs.js';
import { decodeNumber } from './utils.js';

type Flick = { dirSequence: readonly string[], keyId: string };
type FlickSet = { flicks: Flick[], flickId: string };
type VkeyMapping = { code: number, modifiers: number, keyIndex: number };
export type KeySpec = {
  to: string;
  isGap: boolean;
  id: string;
  switch: string;
  width: number;
  longpress: readonly string[];
  defaultLongpressId: string;
  multitap: readonly string[];
  flicks: Flick[];
  keyCode: number,
  modifiers: number
};

export class Keys {
  readonly name = 'keys';
  readonly data: Uint8Array;
  readonly keys: Readonly<Map<string, KeySpec>>;

  constructor(rawData: Uint8Array, strs: Strs, list: List) {
    this.data = rawData;

    const keyCount    = decodeNumber(rawData,  8);
    const flicksCount = decodeNumber(rawData, 12);
    const flickCount  = decodeNumber(rawData, 16);

    const FLICK_GROUP_LEN = 12;
    const FLICK_LEN = 8;

    const keyTableOffset = 24;
    const flicksTableOffset = keyTableOffset + keyCount * 36; // 9 entries * 4 bytes ea
    const flickTableOffset  = flicksTableOffset + flicksCount * FLICK_GROUP_LEN;
    const kmapTableOffset = flickTableOffset + flickCount * FLICK_LEN;

    // keys.keys subtable wants to index within keys.flicks subtable.
    // So let's build the latter first.

    const flickSets = this.processFlickSubtables(strs, list, keyTableOffset);
    const vkeyMap = this.processKeymap(kmapTableOffset);
    const keybag = this.processKeybag(strs, list, flickSets, vkeyMap);

    const keyMap: Map<string, KeySpec> = new Map();
    for(let key of keybag) {
      keyMap.set(key.id, key);
    }
    this.keys = keyMap;
  }

  private processFlickSubtables(strs: Strs, list: List, baseOffset: number) {
    const rawData = this.data;

    const keyCount    = decodeNumber(rawData,  8);
    const flicksCount = decodeNumber(rawData, 12);

    const FLICK_GROUP_LEN = 12;
    const FLICK_LEN = 8;

    const flicksTableOffset = baseOffset + keyCount * 36; // 9 entries * 4 bytes ea
    const flickTableOffset  = flicksTableOffset + flicksCount * FLICK_GROUP_LEN;

    // keys.keys subtable wants to index within keys.flicks subtable.
    // So let's build the latter first.

    const flickSets: FlickSet[] = [];
    for(let i = 0; i < flicksCount; i++) {
      const rowOffset = flicksTableOffset + i * FLICK_GROUP_LEN;
      const elemCount = decodeNumber(rawData, rowOffset);
      const firstIndex = decodeNumber(rawData, rowOffset + 4);
      const stringIndex = decodeNumber(rawData, rowOffset + 8);

      if(i == 0) {
        if(elemCount == 0 && firstIndex == 0 && stringIndex == 0) {
          continue;
        } else {
          throw new Error("Empty flick set expected at index 0 in `keys.flicks` subtable");
        }
      }

      // TODO:  no good current fixture available with data for this.
      const id = strs.entries[decodeNumber(rawData, rowOffset + 8)];
      const baseFlickOffset = flickTableOffset + firstIndex * FLICK_LEN;
      const flicks: Flick[] = [];
      for(let j = 0; j < elemCount; j++) {
        const flickOffset = baseFlickOffset + j * FLICK_LEN;
        const directions = list.entries[decodeNumber(rawData, flickOffset)];
        const keyId = strs.entries[decodeNumber(rawData, flickOffset + 4)];
        flicks.push({
          dirSequence: directions,
          keyId: keyId
        });
      }

      flickSets.push({
        flicks: flicks,
        flickId: id
      });
    }

    return flickSets;
  }

  private processKeymap(offset: number) {
    const kmapCount = decodeNumber(this.data, 20);

    const mappings: VkeyMapping[] = [];
    for(let i=0; i < kmapCount; i++) {
      const rowOffset = offset + 12 * i;

      mappings.push({
        code:      decodeNumber(this.data, rowOffset),
        modifiers: decodeNumber(this.data, rowOffset + 4),
        keyIndex:  decodeNumber(this.data, rowOffset + 8)
      });
    }

    return mappings;
  }

  private processKeybag(strs: Strs, list: List, flickSets: FlickSet[], keyMapping: VkeyMapping[]) {
    // | ∆ | Bits | Name        | Description                              |
    // |---|------|-------------|------------------------------------------|
    // | 0 |  32  | ident       | `key2`                                   |
    // | 4 |  32  | size        | int: Length of section                   |
    // | 8 |  32  | keyCount    | int: Number of keys                      |
    // |12 |  32  | flicksCount | int: Number of flick lists               |
    // |16 |  32  | flickCount  | int: Number of flick elements            |
    // |20 |  32  | kmapCount   | int: Number of kmap elements             |
    // |24 | var  | keys        | keys sub-table                           |
    // | - | var  | flicks      | flick lists sub-table                    |
    // | - | var  | flick       | flick elements sub-table                 |
    // | - | var  | kmap        | key map sub-table                        |
    const rawData = this.data;

    const keyCount    = decodeNumber(rawData,  8);
    const keyTableOffset = 24;

    const keys: KeySpec[] = [];
    for(let i=0; i < keyCount; i++) {
      const rowOffset = keyTableOffset + i * 36; // 9 cols per row in keys.keys subtable.

      // is either an index into `strs` OR a UTF-32LE codepoint.
      const toVal = decodeNumber(rawData, rowOffset);
      const flags = decodeNumber(rawData, rowOffset + 4);
      const to = (flags & 1) ? strs.entries[toVal] : String.fromCodePoint(toVal);

      const isGap = !!(flags & 2);
      const id = strs.entries[decodeNumber(rawData, rowOffset + 8)];
      const switchLayer = strs.entries[decodeNumber(rawData, rowOffset + 12)];

      const width = decodeNumber(rawData, rowOffset + 16);
      const longpressListIndex = decodeNumber(rawData, rowOffset + 20);
      const longpress = longpressListIndex == 0 ? [] : list.entries[longpressListIndex];

      const defaultLongpressId = strs.entries[decodeNumber(rawData, rowOffset + 24)];

      const multitapListIndex = decodeNumber(rawData, rowOffset + 28);
      const multitap = multitapListIndex == 0 ? [] : list.entries[multitapListIndex];

      const flickSet = flickSets[decodeNumber(rawData, rowOffset + 32)];

      const keySpec: KeySpec = {
        to, isGap, id, switch: switchLayer, width, longpress, defaultLongpressId, multitap, flicks: flickSet?.flicks,
        keyCode: 0,
        modifiers: 0
      }

      keys.push(keySpec);
    }

    for(let entry of keyMapping) {
      const keyObj = keys[entry.keyIndex];
      keyObj.keyCode = entry.code;
      keyObj.modifiers = entry.modifiers;
    }

    return keys;
  }
}