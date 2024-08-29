import { decodeNumber, decodeSectName } from './utils.js';

import { Keys } from './keys.js';
import { Layr } from './layr.js';
import { List } from './list.js';
import { Sect } from './sect.js';
import { Strs } from './strs.js';

class UnusedSection {
  readonly name: string;
  readonly data: Uint8Array;;

  constructor(rawData: Uint8Array, name: string) {
    this.data = rawData;
    this.name = name;
  }
}

type Section = Keys | Layr | List | Sect | Strs | UnusedSection;

export class LdmlKeyboardObject {
  readonly dataBuffer: Uint8Array;

  readonly keys: Keys;
  readonly layr: Layr;
  readonly list: List;
  readonly sect: Sect;
  readonly strs: Strs;

  constructor(rawData: Uint8Array, startIndex: number) {
    let kmxPlusLen = decodeNumber(rawData, startIndex + 8);
    this.dataBuffer = new Uint8Array(rawData.buffer, rawData.byteOffset + startIndex, kmxPlusLen);
    this.sect = this.getSection(this.dataBuffer, 0) as Sect;
    if(this.sect.name != 'sect') {
      throw new Error("Provided KMX+ data did not start with the `sect` table!")
    }

    /*
     * Preferred order:
     * - strs
     * - list - needs strs
     * - keys - needs lsts
     * - layr - needs keys, others.
     */
    this.strs = this.getSection(this.dataBuffer, this.sect.sectionOffsets.get('strs')) as Strs;
    this.list = this.getSection(this.dataBuffer, this.sect.sectionOffsets.get('list')) as List;
    this.keys = this.getSection(this.dataBuffer, this.sect.sectionOffsets.get('keys')) as Keys;
    this.layr = this.getSection(this.dataBuffer, this.sect.sectionOffsets.get('layr')) as Layr;
  }

  protected getSection(rawData: Uint8Array, startIndex: number): Section {
    const sectName: string = decodeSectName(rawData, startIndex);
    const sectLen = decodeNumber(rawData, startIndex + 4);
    new Uint8Array()
    const sectionData = new Uint8Array(
      rawData.buffer,
      rawData.byteOffset + startIndex,
      sectLen /* length, not end! */
    );

    switch(sectName) {
      // case 'disp': // TODO
      case 'keys':
        return new Keys(sectionData, this.strs, this.list);
      case 'layr':
        return new Layr(sectionData, this.strs, this.keys);
      case 'list':
        return new List(sectionData, this.strs);
      case 'sect':
        return new Sect(sectionData);
      case 'strs':
        return new Strs(sectionData);
      default:
        return new UnusedSection(sectionData, sectName);
    }
  }
}