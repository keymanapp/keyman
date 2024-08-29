import { convertLayerList, LdmlKeyboardObject } from 'keyman/engine/keyboard';

import { assert } from 'chai';
import fs from 'fs';

/** @type {string} */
const ldmlFixtureAsAscii = fs.readFileSync('../resources/fixtures/imperial_aramaic.kmx', 'binary');
const ldmlFixture = new Uint8Array([...ldmlFixtureAsAscii].map((c) => c.charCodeAt(0)));

describe("LdmlKeyboardObject", () => {
  it("correctly determines the bounds of the kmx+ section given a starting offset", () => {
    const kmxPlusOffset = ldmlFixtureAsAscii.indexOf('sect');
    const kbdObj = new LdmlKeyboardObject(ldmlFixture, kmxPlusOffset);
    assert.equal(kbdObj.dataBuffer.length + kmxPlusOffset, ldmlFixture.length);
  });

  it("correctly detects the top-level sections", () => {
    const kmxPlusOffset = ldmlFixtureAsAscii.indexOf('sect');
    const kbdObj = new LdmlKeyboardObject(ldmlFixture, kmxPlusOffset);

    const sects = [ 'keys', 'layr', 'list', 'sect', 'strs' ];
    for(let sect of sects) {
      assert.isObject(kbdObj[sect]);
      assert.equal(kbdObj[sect].name, sect);
    }
  });

  it("correctly builds a touch-layout from an LDML layer spec", () => {
    const kmxPlusOffset = ldmlFixtureAsAscii.indexOf('sect');
    const kbdObj = new LdmlKeyboardObject(ldmlFixture, kmxPlusOffset);

    const layout = convertLayerList(kbdObj.layr.layerLists[0], kbdObj.keys);

    console.log(JSON.stringify(layout, null, 2));
  });
});