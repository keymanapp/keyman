import { decodeNumber, decodeSectName } from './utils.js';

export class Sect {
  readonly name = 'sect';
  readonly data: Uint8Array;

  readonly sectionOffsets: Readonly<Map<string, number>>;

  constructor(rawData: Uint8Array) {
    this.data = rawData;
    const sectOffsetMap = this.sectionOffsets = new Map<string, number>();

    const sectCount = decodeNumber(rawData, 12);
    for(let i = 0; i < sectCount; i++) {
      const tableRowOffset = 16 + 8*i;

      const sectName = decodeSectName(rawData, tableRowOffset);
      const sectOffset = decodeNumber(rawData, tableRowOffset + 4);

      sectOffsetMap.set(sectName, sectOffset);
    }
  }
}