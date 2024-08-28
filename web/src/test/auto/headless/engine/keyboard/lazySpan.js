import { assert } from 'chai';
import fs from 'fs';

import { LazySpan } from 'keyman/engine/keyboard';

const ldmlFixture = fs.readFileSync('../resources/fixtures/imperial_aramaic.kmx', 'binary');

const simpleString = "the quick brown fox jumped over the lazy dog";

const SINGLE_CHAR_RANGE = 256;
const ENCODED_NUM_BASE = 0;

// Little-endian, and we have to rebuild the number byte-by-byte.
export function decodeNumber(str, start, end) {
  end ??= str.length;
  let num = 0;

  for(let i = end - 1; i >= start; i--) {
    let val = str.charCodeAt(i);
    num = num * SINGLE_CHAR_RANGE + val - ENCODED_NUM_BASE;
  }

  return num;
}

describe("LazySpan", () => {
  it("handles simple, common-case init", () => {
    const span = new LazySpan(simpleString, 4, 9);
    assert.equal(span.text, 'quick');

    const fullSpan = new LazySpan(simpleString, 0, simpleString.length);
    assert.equal(fullSpan.text, simpleString);
  });

  it("correctly builds sub-spans", () => {
    const fullSpan = new LazySpan(simpleString, 0, simpleString.length);
    const quickSpan = fullSpan.substring(4, 9);
    assert.equal(quickSpan.text, 'quick');

    const latterHalf = new LazySpan(simpleString, simpleString.indexOf('jumped'), simpleString.length);
    const overSpan = latterHalf.substring(7, 11);
    assert.equal(overSpan.text, 'over');
  });

  it('ldml parsing scratchspace', () => {
    const kmxPlusOffset = ldmlFixture.indexOf('sect');
    const kmxPlusLength = decodeNumber(ldmlFixture, kmxPlusOffset + 8, kmxPlusOffset + 12);

    // The KMX+ part of the fixture is the 'tail' of its file - once started, it is the remainder.
    assert.equal(kmxPlusLength + kmxPlusOffset, ldmlFixture.length);

    // Sections

    const sectTableLength = decodeNumber(ldmlFixture, kmxPlusOffset + 4, kmxPlusOffset + 8);
    const sectTableSpan = new LazySpan(ldmlFixture, kmxPlusOffset, kmxPlusOffset + sectTableLength);

    const sectionMap = new Map();
    sectionMap.set('sect', sectTableSpan);

    const sectCount = decodeNumber(ldmlFixture, sectTableSpan.start + 12, sectTableSpan.start + 16);

    // First entry's base offset: 16
    const sectNames = [];
    for(let i = 0; i < sectCount; i++) {
      const tableRowOffset = sectTableSpan.start + 16 + 8*i;
      const sectName = new LazySpan(ldmlFixture, tableRowOffset, tableRowOffset + 4);
      sectNames.push(sectName);

      const sectOffset = decodeNumber(ldmlFixture, tableRowOffset + 4, tableRowOffset + 8);

      const nameFromTable = new LazySpan(ldmlFixture, kmxPlusOffset + sectOffset, kmxPlusOffset + sectOffset + 4);
      const tableLength = decodeNumber(ldmlFixture, kmxPlusOffset + sectOffset + 4, kmxPlusOffset + sectOffset + 8);
      const tableSpan = new LazySpan(ldmlFixture, kmxPlusOffset + sectOffset, kmxPlusOffset + sectOffset + tableLength)

      sectionMap.set(sectName.text, tableSpan);

      // Should result in a run-time error when converted to a proper func.
      assert.equal(nameFromTable.text, sectName.text);
    }

    assert.sameMembers(sectNames.map((span) => span.text), ['elem', 'keys', 'layr', 'list', 'loca', 'meta', 'strs', 'uset', 'vars']);
    assert.sameMembers(sectionMap.keys(), ['sect', 'elem', 'keys', 'layr', 'list', 'loca', 'meta', 'strs', 'uset', 'vars']);
  });
});