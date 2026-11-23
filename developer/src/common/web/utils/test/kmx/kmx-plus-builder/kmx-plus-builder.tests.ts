/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * KMX+ file reader builder unit tests
 */

import 'mocha';
import { assert } from 'chai';
import { hextobinFromFile } from '@keymanapp/hextobin';
import { KMXPlusVersion } from '@keymanapp/ldml-keyboard-constants';
import { makePathToCommonFixture } from '../../helpers/index.js';
import { KMX, KMXPlus, KMXPlusFileReader } from '@keymanapp/common-types';
import KMXPlusBuilder from '../../../src/types/kmx/kmx-plus-builder/kmx-plus-builder.js';

describe('KMXPlusBuilder', function() {
  [[17, KMXPlusVersion.Version17], [19, KMXPlusVersion.Version19]].forEach( ([versionMajor, version]) => {
    it(`should round trip a v${versionMajor} KMX+ file into memory, using KMXPlusFileReader, and back`, async function() {
      // TODO-LDML: this really is not sufficient to verify that the builders
      //            are writing out what they should be writing but it is a bit
      //            of a sanity check, verifying against the reader, so much
      //            better than nothing!
      const path = makePathToCommonFixture('keyboards', 'kmx-plus', `basic-${versionMajor}.txt`);
      const input = hextobinFromFile(path, null, { silent: true });
      const reader = new KMXPlusFileReader();
      const file = new KMXPlus.KMXPlusFile(version);
      file.kmxplus = reader.readFromKmx(input);
      const writer = new KMXPlusBuilder(file);
      const output = writer.compile();

      const kmx = new KMX.KMXFile();
      const binaryKmxPlusHeader = kmx.COMP_KEYBOARD_KMXPLUSINFO.fromBuffer(input.slice(KMX.KMXFile.COMP_KEYBOARD_SIZE));
      const kmxplusInput = input.slice(binaryKmxPlusHeader.dpKMXPlus, binaryKmxPlusHeader.dpKMXPlus + binaryKmxPlusHeader.dwKMXPlusSize);

      assert.deepEqual(output, kmxplusInput);
    });
  });
});