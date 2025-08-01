/*
 * Keyman is 2025 copyright (C) SIL International. MIT License.
 *
 * Created by S. Schmitt on 2025-05-12
 *
 * Tests for KeylayoutToKmnConverter, KeylayoutFileReader, KmnFileWriter
 *
 */

import 'mocha';
import { assert } from 'chai';
import { compilerTestCallbacks, compilerTestOptions, makePathToFixture } from './helpers/index.js';
import { KeylayoutToKmnConverter } from '../src/keylayout-to-kmn/keylayout-to-kmn-converter.js';


describe('KeylayoutToKmnConverter', function () {

  before(function () {
    compilerTestCallbacks.clear();
  });

  // todo remove
  describe('RunAllFiles', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    [
      [makePathToFixture('../data/Italian.keylayout')],
      [makePathToFixture('../data/Italian_command.keylayout')],
      [makePathToFixture('../data/Swiss_French.keylayout')],
      [makePathToFixture('../data/Spanish.keylayout')],
      [makePathToFixture('../data/Swiss_German.keylayout')],
      [makePathToFixture('../data/US.keylayout')],
      [makePathToFixture('../data/Polish.keylayout')],
      [makePathToFixture('../data/French.keylayout')],
      [makePathToFixture('../data/Latin_American.keylayout')],
      // [makePathToFixture('../data/German_complete.keylayout')],
      [makePathToFixture('../data/German_complete_reduced.keylayout')],
      //[makePathToFixture('../data/German_Standard.keylayout')],
    ].forEach(function (files_) {
      sut.run(files_[0]);
      assert.isTrue(true);
    });
  });

  describe('RunTestFiles resulting in errors ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    [
      ['../data/Test_DifferentAmountMapSelect_KeyMapERROR.keylayout', 5292040],
      ['../data/Test_DifferentAmountMapSelect_KeyMapERROR_1.keylayout', 5292040],
      ['../data/Test_MissingkeyMapERROR.keylayout', 5246977],
      ['../data/Test_MissingkeyERROR.keylayout', 5246977],
      ['../data/Test_MissingLayoutsERROR.keylayout', 5292036],
      ['../data/Test_MissingmodifierMapERROR.keylayout', 5292036],
      ['../data/Test_MissingkeyMapSetERROR.keylayout', 5292036],
      ['../data/Test_MissingActionsERROR.keylayout', 5292036],
      ['../data/Test_MissingTerminatorsERROR.keylayout', 5292036],
      ['../data/Test_MissingAllERROR.keylayout', 5292036],
    ].forEach(function (files_) {
      it(files_ + " should give error ", async function () {
        sut.run(makePathToFixture(files_[0] as string));
        assert.isTrue(compilerTestCallbacks.messages.length > 0);
        assert.isTrue(String(compilerTestCallbacks.messages[0].code) === String(files_[1]));
      });
    });
  });

  describe('RunSpecialTestFiles', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    [
      [makePathToFixture('../data/Test_C0.keylayout')],
      [makePathToFixture('../data/Test_C1.keylayout')],
      [makePathToFixture('../data/Test_C2.keylayout')],
      [makePathToFixture('../data/Test_C2_several.keylayout')],
      [makePathToFixture('../data/Test_C3.keylayout')],
      [makePathToFixture('../data/Test_C3_several.keylayout')],
      [makePathToFixture('../data/Test_C0_C1_C2_C3.keylayout')],

      [makePathToFixture('../data/Test_duplicate_missing_keycode.keylayout')],
      [makePathToFixture('../data/Test_characters.keylayout')],
      [makePathToFixture('../data/Test_modifier.keylayout')],
      [makePathToFixture('../data/Test_modifierNoCaps.keylayout')],
      [makePathToFixture('../data/Test_onlyOneKeymap.keylayout')],
      [makePathToFixture('../data/Test_nr_elements.keylayout')],
      [makePathToFixture('../data/Test.keylayout')],
    ].forEach(function (files_) {
      it(files_ + " should give no errors ", async function () {
        sut.run(files_[0]);
        assert.isTrue(compilerTestCallbacks.messages.length === 0);
      });
    });
  });


});
