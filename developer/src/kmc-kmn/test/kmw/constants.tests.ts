/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2025-10-16
 *
 * Key cap special values (such as "*Shift*") are defined in multiple modules;
 * this data is not currently in a common module, as it would create unhelpful
 * dependencies or require significant refactoring. So, instead, for now we just
 * verify that the values line up. Note that the _imported_ files are copied in
 * by build.sh before running the tests in order to avoid pathing issues.
 *
 * This is testing that the values in these files are equivalent:
 *   developer/src/tike/xml/layoutbuilder/constants.js
 *   web/src/engine/osk/src/specialCharacters.ts
 *   developer/src/kmc-kmn/kmw-compiler/constants.ts
 *   core/include/ldml/keyman_core_ldml.ts
 *   developer/docs/help/reference/file-types/keyman-touch-layout.md
 */
import * as fs from 'node:fs';
import * as path from 'node:path';
import { fileURLToPath } from 'node:url';

import 'mocha';
import {assert} from 'chai';

import keymanWebSpecialCharacters from "./_imported_web_osk_specialCharacters.js";
import { CSpecialText17, CSpecialText14, CSpecialText10, CSpecialText17ZWNJ } from "../../src/kmw-compiler/constants.js";
import { builder } from "./_imported_layoutbuilder_constants.js";
import { constants as coreLdmlConstants } from "@keymanapp/ldml-keyboard-constants";

/** Verify key cap constants across 4 modules: KMW treated as primary */
describe('Key cap special text values from KeymanWeb', function() {

  it('should match key cap special text in Developer Touch Layout Builder', function() {
    // The key cap special text objects in these two files should be exactly equal:
    //   developer/src/tike/xml/layoutbuilder/constants.js
    //   web/src/engine/osk/src/specialCharacters.ts
    assert.deepEqual(builder.specialCharacters, keymanWebSpecialCharacters);
  });

  it('should match key cap special text in Developer kmc-kmn KMW compiler', function() {
    // These two files should have the same strings for key caps:
    //   developer/src/kmc-kmn/kmw-compiler/constants.ts
    //   web/src/engine/osk/src/specialCharacters.ts

    // No values to compare here - just key names

    // TODO: the following key cap strings are not verified in the compiler,
    //       why? It also appears that the compiler does not verify when an
    //       unrecognized key cap string is used
    const specialCharactersPatch = Object.keys(keymanWebSpecialCharacters).filter(e =>
      e !== "*LAlt*" &&
      e !== "*RAlt*" &&
      e !== "*LCtrl*" &&
      e !== "*RCtrl*" &&
      e !== "*LAltCtrl*" &&
      e !== "*RAltCtrl*" &&
      e !== "*LAltCtrlShift*" &&
      e !== "*RAltCtrlShift*" &&
      e !== "*AltShift*" &&
      e !== "*CtrlShift*" &&
      e !== "*AltCtrlShift*" &&
      e !== "*LAltShift*" &&
      e !== "*RAltShift*" &&
      e !== "*LCtrlShift*" &&
      e !== "*RCtrlShift*"
    ).sort();

    const compilerSpecialCharacters = [
      ...CSpecialText10.split('\0'),
      ...CSpecialText14.split('\0'),
      ...CSpecialText17.split('\0'),
      CSpecialText17ZWNJ,
    ].filter(e => e !== "") // remove blanks coming from the splitting
    .sort();

    assert.deepEqual(compilerSpecialCharacters, specialCharactersPatch);
  });

  // TODO-EMBED-OSK-IN-KMX: enable once we have complete the Core LDML headers
  it.skip('should match key cap special text in Core constants', function() {
    // These two files should have the same constant values for key caps:
    //   core/include/ldml/keyman_core_ldml.ts
    //   web/src/engine/osk/src/specialCharacters.ts

    // The Core constants and KeymanWeb constants vary a little:
    //
    // 1. We need to special case *ABC* and *abc* because they have identical
    //    values but we are case-insensitive in our LDML definitions
    const specialCharactersPatch: any = {...keymanWebSpecialCharacters};
    specialCharactersPatch['*abc_lower*'] = specialCharactersPatch['*abc*'];
    delete specialCharactersPatch['*abc*'];

    specialCharactersPatch['*ABC_upper*'] = specialCharactersPatch['*ABC*'];
    delete specialCharactersPatch['*ABC*'];

    // 2. Map all the "*Name*" key names to "dis2_key_cap_name"...
    const dis2_key_cap_expected: any = {};
    for(const key of Object.keys(specialCharactersPatch)) {
      const newKey = key.replace(/^\*(.+)\*$/, 'dis2_key_cap_$1').toLowerCase();
      dis2_key_cap_expected[newKey] = specialCharactersPatch[key];
    }

    // 3. We only want to compare the dis2_key_cap_ values from the
    //    coreLdmlConstants object
    const coreConstantsFiltered: any = {};
    for(const key of Object.keys(coreLdmlConstants)) {
      if(key.match(/^dis2_key_cap_/)) {
        coreConstantsFiltered[key] = (<any>coreLdmlConstants)[key];
      }
    }

    assert.deepEqual(coreConstantsFiltered, dis2_key_cap_expected);
  });

  it('should be documented correctly', function() {
    // These two files should have the same constant values for key caps:
    //   developer/docs/help/reference/file-types/keyman-touch-layout.md
    //   web/src/engine/osk/src/specialCharacters.ts
    const helpFile =
      path.join(path.dirname(fileURLToPath(import.meta.url)), '../../../../../docs/help/reference/file-types/keyman-touch-layout.md');
    const helpLines = fs.readFileSync(helpFile, 'utf-8').replaceAll(/\r\n/g, '\n').split('\n');

    // Find the relevant section in the file between start:special_key_caps and
    // end:special_key_caps
    const line0 = helpLines.findIndex(line => line.includes('start:special_key_caps'));
    assert.notEqual(line0, -1);
    const line1 = helpLines.findIndex(line => line.includes('end:special_key_caps'));
    assert.notEqual(line1, -1);

    const content = helpLines.slice(line0+1, line1);

    const markdownConstants: any = {};

    // Iterate over <td> cells in the relevant section of the file and build an
    // array of values, assuming the format of the markdown doesn't change
    let nextCap: string = null;
    for(const line of content) {
      const mCap = line.match(/<td markdown="1">`(\*.+\*)`<\/td>/i);
      if(mCap) {
        nextCap = mCap[1];
        continue;
      }
      const mVal = line.match(/<td class="special-osk">&#xE(.+);<\/td>/);
      if(mVal) {
        markdownConstants[nextCap] = parseInt(mVal[1], 16);
      }
    }

    assert.deepEqual(markdownConstants, keymanWebSpecialCharacters);
  });
});
