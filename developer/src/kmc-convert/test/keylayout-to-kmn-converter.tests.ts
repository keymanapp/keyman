/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by S. Schmitt on 2025-05-12
 *
 * Tests for KeylayoutToKmnConverter, KeylayoutFileReader, KmnFileWriter
 *
 */

import 'mocha';
import { assert } from 'chai';
import * as NodeAssert from 'node:assert';
import { compilerTestCallbacks, compilerTestOptions, makePathToFixture } from './helpers/index.js';
import { ActionStateOutput, KeylayoutFileData, KeylayoutToKmnConverter, Rule } from '../src/keylayout-to-kmn/keylayout-to-kmn-converter.js';
import { KeylayoutFileReader } from '../src/keylayout-to-kmn/keylayout-file-reader.js';
import { ConverterMessages } from '../src/converter-messages.js';

describe('KeylayoutToKmnConverter', function () {

  before(function () {
    compilerTestCallbacks.clear();
  });

  describe('RunTestFiles resulting in errors ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    [
      [makePathToFixture('../data/Test_DifferentAmountOfMapSelectInKeyMapERROR.keylayout')],
      [makePathToFixture('../data/Test_DifferentAmountOfMapSelectInKeyMapERROR_1.keylayout')],
      [makePathToFixture('../data/Test_MissingkeyERROR.keylayout')],
      [makePathToFixture('../data/Test_MissingkeyMapERROR.keylayout')],
      [makePathToFixture('../data/Test_MissingLayoutsERROR.keylayout')],
      [makePathToFixture('../data/Test_MissingmodifierMapERROR.keylayout')],
      [makePathToFixture('../data/Test_MissingkeyMapSetERROR.keylayout')],
      [makePathToFixture('../data/Test_MissingActionsERROR.keylayout')],
      [makePathToFixture('../data/Test_MissingTerminatorsERROR.keylayout')],
      [makePathToFixture('../data/Test_MissingAllERROR.keylayout')],
    ].forEach(function (files) {
      it(files + " should give an error ", async function () {
        sut.run(files[0]);
        assert.isTrue(compilerTestCallbacks.messages.length > 0);
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
      [makePathToFixture('../data/Test_maxKeyCode.keylayout')],
      [makePathToFixture('../data/Test_messages.keylayout')],
      [makePathToFixture('../data/Test_messages_controlCharacter.keylayout')],
      [makePathToFixture('../data/Test_messages_superior_C2.keylayout')],
      [makePathToFixture('../data/Test_messages_superior_C3.keylayout')],
      [makePathToFixture('../data/Test_duplicate_missing_keycode.keylayout')],
      [makePathToFixture('../data/Test_modifier.keylayout')],
      [makePathToFixture('../data/Test_modifierNoCaps.keylayout')],
      [makePathToFixture('../data/Test_differentAmountOfKeysInBehaviours.keylayout')],
      [makePathToFixture('../data/Test_duplicate_missing_keys.keylayout')],
      [makePathToFixture('../data/Test_duplicate_keys.keylayout')],
      [makePathToFixture('../data/Test_ambiguous_keys.keylayout')],
      [makePathToFixture('../data/Test_nr_elements.keylayout')],
      [makePathToFixture('../data/Test.keylayout')],
    ].forEach(function (files) {
      it(files + " should give no errors ", async function () {
        sut.run(files[0]);
        assert.isTrue(compilerTestCallbacks.messages.length === 0);
      });
    });
  });
/*=======
*/
  /*describe('RunSpecialTestFiles - create Info', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    [
      [makePathToFixture('../data/Test_characters.keylayout')],
      [makePathToFixture('../data/Test_onlyOneKeymap.keylayout')],
    ].forEach(function (files) {
      it(files + " should give Info: unsupported characters ", async function () {
        sut.run(files[0]);
        assert.isTrue(compilerTestCallbacks.messages.length === 1);
        assert.isTrue(compilerTestCallbacks.messages[0].code === (CompilerErrorSeverity.Info | CompilerErrorNamespace.Converter | 0x0007));
      });
    });
  });*/


  describe('run() ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);

    it('run() should throw on null input file name and null output file name', async function () {
      // note, could use 'chai as promised' library to make this more fluent:
      const result = sut.run(null, null);
      assert.isNotNull(result);
      assert.equal(compilerTestCallbacks.messages.length, 1);
      assert.deepEqual(compilerTestCallbacks.messages[0], ConverterMessages.Error_FileNotFound({ inputFilename: null }));
    });

    it('run() should throw on null input file name and empty output file name', async function () {
      const result = sut.run(null, '');
      assert.isNotNull(result);
      assert.equal(compilerTestCallbacks.messages.length, 1);
      assert.deepEqual(compilerTestCallbacks.messages[0], ConverterMessages.Error_FileNotFound({ inputFilename: null }));
    });

    it('run() should throw on null input file name and unknown output file name', async function () {
      const result = sut.run(null, 'X');
      assert.isNotNull(result);
      assert.equal(compilerTestCallbacks.messages.length, 1);
      assert.deepEqual(compilerTestCallbacks.messages[0], ConverterMessages.Error_FileNotFound({ inputFilename: null }));
    });

    it('run() should throw on unavailable input file name and null output file name', async function () {
      const inputFilename = makePathToFixture('../data/Unavailable.keylayout');
      const result = sut.run(inputFilename, null);
      assert.isNotNull(result);
      assert.equal(compilerTestCallbacks.messages.length, 2);
      assert.deepEqual(compilerTestCallbacks.messages[0], ConverterMessages.Error_UnableToRead({ inputFilename }));
      assert.equal(compilerTestCallbacks.messages[1].code, 5246984);
    });

    it('run() should return on available input file name and null output file name', async function () {
      this.timeout(5000); // allow longer time for this test
      const inputFilename = makePathToFixture('../data/Test.keylayout');
      const result = sut.run(inputFilename, null);
      assert.isNotNull(result);
      await NodeAssert.doesNotReject(async () => sut.run(inputFilename, null));
      assert.equal(compilerTestCallbacks.messages.length, 0);
    });

    it('run() should return on correct input file name and empty output file name ', async function () {
      const inputFilename = makePathToFixture('../data/Test.keylayout');
      await NodeAssert.doesNotReject(async () => sut.run(inputFilename, ''));
      assert.equal(compilerTestCallbacks.messages.length, 0);
    });

    it('run() should return on correct input file name and null output file name', async function () {
      const inputFilename = makePathToFixture('../data/Test.keylayout');
      await NodeAssert.doesNotReject(async () => sut.run(inputFilename, null));
      assert.equal(compilerTestCallbacks.messages.length, 0);
    });

    it('run() should return on correct input file name and given output file name ', async function () {
      const inputFilename = makePathToFixture('../data/Test.keylayout');
      const outputFilename = makePathToFixture('../data/OutputName.kmn');
      await NodeAssert.doesNotReject(async () => sut.run(inputFilename, outputFilename));
      assert.equal(compilerTestCallbacks.messages.length, 0);
    });

    it('run() return on correct input file extention and unsupperted output file extention', async function () {
      const inputFilename = makePathToFixture('../data/Test.keylayout');
      const outputFilename = makePathToFixture('../data/OutputXName.B');
      await NodeAssert.doesNotReject(async () => sut.run(inputFilename, outputFilename));
      assert.equal(compilerTestCallbacks.messages.length, 0);
    });
  });

  describe('convert() ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sutR = new KeylayoutFileReader(compilerTestCallbacks);

    // ProcessedData from usable file
    const inputFilename = makePathToFixture('../data/Test.keylayout');
    const read = sutR.read(inputFilename);
    const converted = sut.convertBound.convert(read, inputFilename.replace(/\.keylayout$/, '.kmn'));

    // ProcessedData from unavailable file
    const inputFilenameUnavailable = makePathToFixture('../data/X.keylayout');
    const readUnavailable = sutR.read(inputFilenameUnavailable);
    const convertedUnavailable = sut.convertBound.convert(readUnavailable, inputFilenameUnavailable.replace(/\.keylayout$/, '.kmn'));

    // ProcessedData from empty file
    const inputFilenameEmpty = makePathToFixture('');
    const readEmpty = sutR.read(inputFilenameEmpty);
    const convertedEmpty = sut.convertBound.convert(readEmpty, inputFilenameEmpty);

    it('should return converted array on correct input', async function () {
      assert.isTrue(converted.arrayOfRules.length !== 0);
    });

    it('should return empty on empty input', async function () {
      assert.isTrue((convertedEmpty.keylayoutFilename === ''
        && convertedEmpty.arrayOfModifiers.length === 0
        && convertedEmpty.arrayOfRules.length === 0));
    });

    it('should return empty on only name as input', async function () {
      assert.isTrue((convertedUnavailable.keylayoutFilename === ''
        && convertedUnavailable.arrayOfModifiers.length === 0
        && convertedUnavailable.arrayOfRules.length === 0));
    });

    it('should return empty on only modifiers as input', async function () {
      const convertedMod = sut.convertBound.convert({
        keylayoutFilename: '',
        arrayOfModifiers: [['caps'], ['Shift'], ['command']],
        arrayOfRules: []
      }, '');
      assert.isTrue((convertedMod.keylayoutFilename === ''
        && convertedMod.arrayOfModifiers.length === 0
        && convertedMod.arrayOfRules.length === 0));
    });

    it('should return empty on only rules as input', async function () {
      const convertedRule = sut.convertBound.convert({
        keylayoutFilename: '',
        arrayOfModifiers: [],
        arrayOfRules: [['C0', '', '', 0, 0, '', '', 0, 0, 'CAPS', 'K_A', 'A']]
      }, '');
      assert.isTrue((convertedRule.keylayoutFilename === ''
        && convertedRule.arrayOfModifiers.length === 0
        && convertedRule.arrayOfRules.length === 0));
    });

    it('should return empty array of rules on null input', async function () {
      const convertedRule = sut.convertBound.convert(null, 'ABC.kmn');
      assert.isTrue(convertedRule.arrayOfRules.length === 0);
    });
  });

  describe('createKmnModifier ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);

    [
      ['NCAPS', true, 'NCAPS'],
      ['NCAPS shift', true, 'NCAPS SHIFT'],
      ['shift', true, 'NCAPS SHIFT'],
      ['leftshift', true, 'NCAPS SHIFT'],
      ['rightShift', true, 'NCAPS SHIFT'],
      ['shift rightShift?', true, 'NCAPS SHIFT'],
      ['rightShift?', true, 'NCAPS'],
      ['shift Shift?', true, 'NCAPS SHIFT'],
      ['shift rightShift? caps? rightOption? rightControl', true, 'NCAPS SHIFT RCTRL'],
      ['leftshift rightShift? caps? rightOption? rightControl', true, 'NCAPS SHIFT RCTRL'],
      ['anycontrol', true, 'NCAPS CTRL'],
      ['shift?', true, 'NCAPS'],
      ['?', true, 'NCAPS'],
      ['?', false, ''],
      ['caps', true, 'CAPS'],
      ['', true, 'NCAPS'],
      [' ', false, ''],
      ['wrongModifierName', false, 'wrongModifierName'],
      ['shift', false, 'SHIFT'],
      ['shift command', true, 'NCAPS SHIFT command'],
      ['rshift', true, 'NCAPS SHIFT'],
      ['rshift', false, 'SHIFT'],
      ['rightshift', true, 'NCAPS SHIFT'],
      ['riGhtsHift', true, 'NCAPS SHIFT'],
      ['LEFTCONTROL', true, 'NCAPS LCTRL'],
      ['RCONTROL', true, 'NCAPS RCTRL'],
      ['leftoption', true, 'NCAPS LALT'],
      ['loption', true, 'NCAPS LALT'],
      ['rightoption', true, 'NCAPS RALT'],
      ['roption', true, 'NCAPS RALT'],
    ].forEach(function (values) {
      it(('should convert "' + values[0] + '"').padEnd(36, " ") + 'to "' + values[2] + '"', async function () {
        const result = sut.createKmnModifier(values[0] as string, values[1] as boolean);
        assert.equal(result, values[2]);
      });
    });
  });

  describe('isAcceptableKeymanModifier ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);

    [
      ['NCAPS', true],
      ['NxCAPS', false],
      ['SHIFT', true],
      ['ALT', true],
      ['RALT', true],
      ['LALT', true],
      ['CTRL', true],
      ['LCTRL', true],
      ['RCTRL', true],
      ['LCTRL CAPS', true],
      ['LCTRL X', false],
      ['', true],
      [null, false],
    ].forEach(function (values) {
      it(("isAcceptableKeymanModifier(" + values[0] + ")").padEnd(38, " ") + ' should return ' + values[1], async function () {
        const result = sut.isAcceptableKeymanModifier(values[0] as string);
        assert.equal(result, values[1]);
      });
    });
  });

  describe('mapUkeleleKeycodeToVK ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    [
      [0x00, 'K_A'],
      [0x31, 'K_SPACE'],
      [0x18, 'K_EQUAL'],
      [0x10, 'K_Y'],
      [0x18, 'K_EQUAL'],
      [0x21, 'K_LBRKT'],
      [0x999, ''],
      [-1, ''],
      [null, ''],
      [undefined, ''],
      [, ''],
    ].forEach(function (values) {
      it(("mapUkeleleKeycodeToVK(" + values[0] + ")").padEnd(26, " ") + "should return " + "'" + values[1] + "'", async function () {
        const result = sut.mapUkeleleKeycodeToVK(values[0] as number);
        assert.equal(result, values[1]);
      });
    });
  });

  describe('checkIfCapsIsUsed ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);

    [
      [[['caps', 'xxx'], ['yyy']], true],
      [[['Caps', 'xxx'], ['yyy']], true],
      [[['Caps?', 'xxx'], ['yyy']], true],
      [[['caps?', 'xxx'], ['yyy']], true],
      [[['CaPs', 'xxx'], ['yyy']], true],
      [[['zzz', 'xxx'], ['yyy']], false],
      [[['', ''], ['']], false],
      [[null], false],
    ].forEach(function (values) {
      it(("checkIfCapsIsUsed([['caps', 'xxx'], ['yyy']])").padEnd(45, " ") + "should return " + "'" + values[1] + "'", async function () {
        const result = sut.checkIfCapsIsUsed(values[0] as string[][]) === values[1];
        assert.isTrue(result);
      });
    });
  });

  describe('getModifierArrayFromKeyModifierArray ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sutR = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Test.keylayout');
    const read = sutR.read(inputFilename);
    const converted = sut.convertBound.convert(read, inputFilename.replace(/\.keylayout$/, '.kmn'));
    [
      [[{ key: '0', behaviour: 0 }], [['', 'shift? caps? ']]],
      [[{ key: '0', behaviour: 2 }], [['shift? leftShift caps? ', 'anyShift caps?', 'shift leftShift caps ', 'shift? rightShift caps? ']]],
      [[{ key: '0', behaviour: 999 }], [null]],
      [[{ key: '999', behaviour: null }], [null]],
      [[{ key: '0', behaviour: -999 }], [null]],
      [[{ key: '0', behaviour: null }], [null]],
      [[], []],

    ].forEach(function (values) {
      it((values[1] !== null) ?
        ("getModifierArrayFromKeyModifierArray('" + JSON.stringify(values[0]) + "')").padEnd(68, " ") + " should return '" + JSON.stringify(values[1]) + "'" :
        ("getModifierArrayFromKeyModifierArray('" + JSON.stringify(values[0]) + "')").padEnd(68, " ") + " should return '" + "null" + "'", async function () {
          const result = sut.getModifierArrayFromKeyModifierArray(converted.arrayOfModifiers, values[0] as KeylayoutFileData[]);
          assert.deepStrictEqual(JSON.stringify(result), JSON.stringify(values[1]));
        });
    });
  });

  describe('getKeyModifierArrayFromActionID ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sutR = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Test.keylayout');
    const read = sutR.read(inputFilename);
    [
      ['A_16', [{ "key": "32", "behaviour": "5" }]],
      ['A_19', [{ "key": "45", "behaviour": "5" }]],
      ['A_18', [{ "key": "24", "behaviour": "0" }, { "key": "24", "behaviour": "5" }]],
      ['unknown', []],
      [undefined, []],
      [null, []],
      [' ', []],
      ['', []],
    ].forEach(function (values) {
      let outstring = '[ ';
      for (let i = 0; i < values[1].length; i++) {
        outstring = outstring + "[ " + JSON.stringify(values[1][i]) + "], ";
      }
      it(("getKeyModifierArrayFromActionID('" + values[0] + "')").padEnd(57, " ") + ' should return ' + outstring.substring(0, outstring.lastIndexOf(']') + 2) + " ]", async function () {
        const result = sut.getKeyModifierArrayFromActionID(read, String(values[0]));
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });
  });

  describe('getActionIdFromActionNext ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sutR = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Test.keylayout');
    const read = sutR.read(inputFilename);

    [
      ['none', ''],
      ['0', ''],
      ['A_18', ''],
      ['1', 'A_16'],
      ['2', 'A_8'],
      ['3', 'A_17'],
      ['', ''],
      [' ', ''],
      ['99', ''],
      [null, ''],
      [undefined, ''],
      ['unknown', ''],
    ].forEach(function (values) {
      it(("getActionIdFromActionNext('" + values[0] + "')").padEnd(49, " ") + ' should return ' + "'" + values[1] + "'", async function () {
        const result = sut.getActionIdFromActionNext(read, String(values[0]));
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });
  });
  describe('getActionIndexFromActionId ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sutR = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Test.keylayout');
    const read = sutR.read(inputFilename);

    [
      ['none', 0],
      ['A_16', 8],
      ['A_18', 10],
      ['A_19', 11],
      ['0', 0],
      ['', 0],
      [' ', 0],
      [null, 0],
      [undefined, 0],
      ['unknown', 0],
    ].forEach(function (values) {
      it(("getActionIndexFromActionId('" + values[0] + "')").padEnd(50, " ") + ' should return ' + values[1], async function () {
        const result = sut.getActionIndexFromActionId(read, String(values[0]));
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });
  });
  describe('getOutputFromActionIdNone ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sutR = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Test.keylayout');
    const read = sutR.read(inputFilename);

    [
      ['A_14', 'u'],
      ['', ''],
      [' ', ''],
      ['A_18', ''],
      ['unknown', ''],
    ].forEach(function (values) {
      it(
        ("getOutputFromActionIdNone('" + values[0] + "')").padEnd(56, " ") + ' should return ' + "'" + values[1] + "'", async function () {
          const result = sut.getOutputFromActionIdNone(read, String(values[0]));
          assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
        });
    });

    [[null, ''],
    [undefined, ''],
    [99, ''],
    ].forEach(function (values) {
      it(("getOutputFromActionIdNone('" + values[0] + "')").padEnd(56, " ") + ' should return ' + values[1], async function () {
        const result = sut.getOutputFromActionIdNone(read, String(values[0]));
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });
  });
  describe('getKeyBehaviourModOutputArrayFromKeyActionBehaviourOutputArray ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sutR = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Test.keylayout');
    const read = sutR.read(inputFilename);

    const b1KeycodeArr: KeylayoutFileData[] = [
      { keyCode: '49', key: 'K_SPACE', actionId: 'A_0', behaviour: '0', outchar: 'ˆ' },
      { keyCode: '49', key: 'K_SPACE', actionId: 'A_0', behaviour: '1', outchar: 'ˆ' },
      { keyCode: '49', key: 'K_SPACE', actionId: 'A_0', behaviour: '2', outchar: 'ˆ' },
      { keyCode: '6', key: 'K_Z', actionId: 'A_0', behaviour: '4', outchar: 'ˆ' },
      { keyCode: '25', key: 'K_9', actionId: 'A_0', behaviour: '4', outchar: 'ˆ' },
      { keyCode: '43', key: 'K_COMMA', actionId: 'A_0', behaviour: '4', outchar: 'ˆ' },
      { keyCode: '49', key: 'K_SPACE', actionId: 'A_0', behaviour: '3', outchar: 'ˆ' },
      { keyCode: '0', key: 'K_A', actionId: 'A_1', behaviour: '2', outchar: 'Â' },
      { keyCode: '0', key: 'K_A', actionId: 'A_1', behaviour: '1', outchar: 'Â' },
      { keyCode: '14', key: 'K_E', actionId: 'A_10', behaviour: '0', outchar: 'ê' },
      { keyCode: '34', key: 'K_I', actionId: 'A_11', behaviour: '0', outchar: 'î' },
      { keyCode: '31', key: 'K_O', actionId: 'A_13', behaviour: '0', outchar: 'ô' },
      { keyCode: '32', key: 'K_U', actionId: 'A_14', behaviour: '0', outchar: 'û' },
      { keyCode: '14', key: 'K_E', actionId: 'A_2', behaviour: '2', outchar: 'Ê' },
      { keyCode: '14', key: 'K_E', actionId: 'A_2', behaviour: '1', outchar: 'Ê' },
      { keyCode: '34', key: 'K_I', actionId: 'A_3', behaviour: '2', outchar: 'Î' },
      { keyCode: '34', key: 'K_I', actionId: 'A_3', behaviour: '1', outchar: 'Î' },
      { keyCode: '31', key: 'K_O', actionId: 'A_5', behaviour: '2', outchar: 'Ô' },
      { keyCode: '31', key: 'K_O', actionId: 'A_5', behaviour: '1', outchar: 'Ô' },
      { keyCode: '32', key: 'K_U', actionId: 'A_6', behaviour: '2', outchar: 'Û' },
      { keyCode: '32', key: 'K_U', actionId: 'A_6', behaviour: '1', outchar: 'Û' },
      { keyCode: '0', key: 'K_A', actionId: 'A_9', behaviour: '0', outchar: 'â' }

    ];
    const b1ModifierKeyArr: KeylayoutFileData[] = [
      { actionId: 'A_0', key: 'K_SPACE', behaviour: '0', modifier: 'NCAPS', outchar: 'ˆ' },
      { actionId: 'A_0', key: 'K_SPACE', behaviour: '1', modifier: 'CAPS', outchar: 'ˆ' },
      { actionId: 'A_0', key: 'K_SPACE', behaviour: '2', modifier: 'NCAPS SHIFT', outchar: 'ˆ' },
      { actionId: 'A_0', key: 'K_SPACE', behaviour: '2', modifier: 'SHIFT CAPS', outchar: 'ˆ' },
      { actionId: 'A_0', key: 'K_Z', behaviour: '4', modifier: 'NCAPS SHIFT RALT', outchar: 'ˆ' },
      { actionId: 'A_0', key: 'K_9', behaviour: '4', modifier: 'NCAPS SHIFT RALT', outchar: 'ˆ' },
      { actionId: 'A_0', key: 'K_COMMA', behaviour: '4', modifier: 'NCAPS SHIFT RALT', outchar: 'ˆ' },
      { actionId: 'A_0', key: 'K_SPACE', behaviour: '3', modifier: 'NCAPS RALT CTRL', outchar: 'ˆ' },
      { actionId: 'A_0', key: 'K_SPACE', behaviour: '3', modifier: 'NCAPS CTRL', outchar: 'ˆ' },
      { actionId: 'A_1', key: 'K_A', behaviour: '2', modifier: 'NCAPS SHIFT', outchar: 'Â' },
      { actionId: 'A_1', key: 'K_A', behaviour: '2', modifier: 'SHIFT CAPS', outchar: 'Â' },
      { actionId: 'A_1', key: 'K_A', behaviour: '1', modifier: 'CAPS', outchar: 'Â' },
      { actionId: 'A_10', key: 'K_E', behaviour: '0', modifier: 'NCAPS', outchar: 'ê' },
      { actionId: 'A_11', key: 'K_I', behaviour: '0', modifier: 'NCAPS', outchar: 'î' },
      { actionId: 'A_13', key: 'K_O', behaviour: '0', modifier: 'NCAPS', outchar: 'ô' },
      { actionId: 'A_14', key: 'K_U', behaviour: '0', modifier: 'NCAPS', outchar: 'û' },
      { actionId: 'A_2', key: 'K_E', behaviour: '2', modifier: 'NCAPS SHIFT', outchar: 'Ê' },
      { actionId: 'A_2', key: 'K_E', behaviour: '2', modifier: 'SHIFT CAPS', outchar: 'Ê' },
      { actionId: 'A_2', key: 'K_E', behaviour: '1', modifier: 'CAPS', outchar: 'Ê' },
      { actionId: 'A_3', key: 'K_I', behaviour: '2', modifier: 'NCAPS SHIFT', outchar: 'Î' },
      { actionId: 'A_3', key: 'K_I', behaviour: '2', modifier: 'SHIFT CAPS', outchar: 'Î' },
      { actionId: 'A_3', key: 'K_I', behaviour: '1', modifier: 'CAPS', outchar: 'Î' },
      { actionId: 'A_5', key: 'K_O', behaviour: '2', modifier: 'NCAPS SHIFT', outchar: 'Ô' },
      { actionId: 'A_5', key: 'K_O', behaviour: '2', modifier: 'SHIFT CAPS', outchar: 'Ô' },
      { actionId: 'A_5', key: 'K_O', behaviour: '1', modifier: 'CAPS', outchar: 'Ô' },
      { actionId: 'A_6', key: 'K_U', behaviour: '2', modifier: 'NCAPS SHIFT', outchar: 'Û' },
      { actionId: 'A_6', key: 'K_U', behaviour: '2', modifier: 'SHIFT CAPS', outchar: 'Û' },
      { actionId: 'A_6', key: 'K_U', behaviour: '1', modifier: 'CAPS', outchar: 'Û' },
      { actionId: 'A_9', key: 'K_A', behaviour: '0', modifier: 'NCAPS', outchar: 'â' }
    ];

    [[b1KeycodeArr, b1ModifierKeyArr],
    [[{ keyCode: '49', key: 'K_SPACE', actionId: 'A_0', behaviour: '0', modifier: '0', outchar: 'ˆ' }], [{ actionId: 'A_0', key: 'K_SPACE', behaviour: '0', modifier: 'NCAPS', outchar: 'ˆ' }]],
    [[{ keyCode: '49', key: 'K_SPACE', actionId: 'A_0', behaviour: '0', modifier: '', outchar: 'ˆ' }], [{ actionId: 'A_0', key: 'K_SPACE', behaviour: '0', modifier: 'NCAPS', outchar: 'ˆ' }]],
    [[{ keyCode: '49', key: 'K_SPACE', actionId: '', behaviour: '0', modifier: '0', outchar: 'ˆ' }], [{ actionId: '', key: 'K_SPACE', behaviour: '0', modifier: 'NCAPS', outchar: 'ˆ' }]],
    [[{ keyCode: '49', key: '', actionId: 'A_0', behaviour: '0', modifier: '0', outchar: 'ˆ' }], [{ actionId: 'A_0', key: '', behaviour: '0', modifier: 'NCAPS', outchar: 'ˆ' }]],
    [[{ keyCode: '', key: 'K_SPACE', actionId: 'A_0', behaviour: '0', modifier: '0', outchar: 'ˆ' }], [{ actionId: 'A_0', key: 'K_SPACE', behaviour: '0', modifier: 'NCAPS', outchar: 'ˆ' }]],
    [[{ keyCode: '', key: '', actionId: '', behaviour: '0', modifier: '', outchar: '' }], [{ actionId: '', key: '', behaviour: '0', modifier: 'NCAPS', outchar: '' }]],
    ].forEach(function (values) {
      const isCapsUsed = true;
      const stringIn = "getKeyBehaviourModOutputArrayFromKeyActionBehaviourOutputArray(['" + "', '" + "', '" + values[0][0].keyCode + "', '" + values[0][0].key + "', '" + values[0][0].actionId + "', '" + values[0][0].modifier + "', '" + values[0][0].outchar + "'])";
      const stringOut = "['" + "', '" + "', '" + values[1][0].key + "', '" + values[1][0].actionId + "', '" + "', '" + values[1][0].modifier + "', '" + values[1][0].outchar + "']";

      it((JSON.stringify(values[1]).length > 60) ? 'an array of objects should return an array of objects' :
        stringIn.padEnd(74, " ") + ' should return ' + stringOut, async function () {
          const result = sut.getKeyBehaviourModOutputArrayFromKeyActionBehaviourOutputArray(read, values[0], isCapsUsed);
          assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
        });
    });

    [[{ keyCode: '49', key: 'K_SPACE', actionId: 'A_0', behaviour: '0', modifier: '0', outchar: 'ˆ' }, { actionId: 'A_0', key: 'K_SPACE', behaviour: '0', modifier: '', outchar: 'ˆ' }],
    [{ keyCode: '', key: 'K_SPACE', actionId: 'A_0', behaviour: '0', modifier: '0', outchar: 'ˆ' }, { actionId: 'A_0', key: 'K_SPACE', behaviour: '0', modifier: '', outchar: 'ˆ' }],
    [{ keyCode: '', key: '', actionId: '', behaviour: '0', modifier: '', outchar: '' }, { actionId: '', key: '', behaviour: '0', modifier: '', outchar: '' }],
    ].forEach(function (values) {
      const isCapsUsed = false;
      const stringIn = "getKeyBehaviourModOutputArrayFromKeyActionBehaviourOutputArray([ '" + values[0].keyCode + "', '" + values[0].key + "', '" + values[0].actionId + "', '" + values[0].modifier + "', '" + values[0].outchar + "'])";
      const stringOut = "['" + values[1].actionId + "', '" + "', '" + values[1].modifier + "', '" + values[1].key + "', '" + values[1].outchar + "']";

      it(stringIn.padEnd(74, " ") + ' should return ' + stringOut, async function () {
        const result = sut.getKeyBehaviourModOutputArrayFromKeyActionBehaviourOutputArray(read, [values[0]], isCapsUsed);
        assert.equal(JSON.stringify(result), JSON.stringify([values[1]]));
      });
    });

    [[[], []],
    [undefined, []],
    [null, []],
    ].forEach(function (values) {
      const isCaps = true;
      it(("getKeyBehaviourModOutputArrayFromKeyActionBehaviourOutputArray([" + values[0] + "])").padEnd(74, " ") + ' should return ' + "[" + values[1] + "]", async function () {
        const result = sut.getKeyBehaviourModOutputArrayFromKeyActionBehaviourOutputArray(read, values[0], isCaps);
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });
  });
  describe('getActionStateOutputArrayFromActionState ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sutR = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Test.keylayout');
    const read = sutR.read(inputFilename);

    [['1', [
      { "id": "A_0", "state": "1", "output": "ˆ" },
      { "id": "A_1", "state": "1", "output": "Â" },
      { "id": "A_10", "state": "1", "output": "ê" },
      { "id": "A_11", "state": "1", "output": "î" },
      { "id": "A_13", "state": "1", "output": "ô" },
      { "id": "A_14", "state": "1", "output": "û" },
      { "id": "A_2", "state": "1", "output": "Ê" },
      { "id": "A_3", "state": "1", "output": "Î" },
      { "id": "A_5", "state": "1", "output": "Ô" },
      { "id": "A_6", "state": "1", "output": "Û" },
      { "id": "A_9", "state": "1", "output": "â" }
    ],],
    ['2', [
      { "id": "A_0", "state": "2", "output": "`" },
      { "id": "A_1", "state": "2", "output": "À" },
      { "id": "A_10", "state": "2", "output": "è" },
      { "id": "A_11", "state": "2", "output": "ì" },
      { "id": "A_13", "state": "2", "output": "ò" },
      { "id": "A_14", "state": "2", "output": "ù" },
      { "id": "A_2", "state": "2", "output": "È" },
      { "id": "A_3", "state": "2", "output": "Ì" },
      { "id": "A_5", "state": "2", "output": "Ò" },
      { "id": "A_6", "state": "2", "output": "Ù" },
      { "id": "A_9", "state": "2", "output": "à" }
    ],],
    ['789', [],],
    ['', [],],
    [' ', [],],
    [123, [],],
    [null, [],],
    [undefined, [],],
    ].forEach(function (values) {
      it((JSON.stringify(values[1]).length > 30) ?
        ("getActionStateOutputArrayFromActionState('" + values[0] + "')").padEnd(60, " ") + ' should return an array of objects' :
        ("getActionStateOutputArrayFromActionState('" + values[0] + "')").padEnd(60, " ") + ' should return ' + "'" + JSON.stringify(values[1]) + "'", async function () {
          const result = sut.getActionStateOutputArrayFromActionState(read, String(values[0]));
          assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
        });
    });
  });
  describe('getActionOutputBehaviourKeyModiFromActionIDStateOutput ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sutR = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Test.keylayout');
    const read = sutR.read(inputFilename);
    const converted = sut.convertBound.convert(read, inputFilename.replace(/\.keylayout$/, '.kmn'));

    [
      ['A_1', 'A', true,
        [{ "outchar": "A", "actionId": "A_1", "behaviour": "1", "key": "K_A", "modifier": "CAPS" },
        { "outchar": "A", "actionId": "A_1", "behaviour": "2", "key": "K_A", "modifier": "NCAPS SHIFT" },
        { "outchar": "A", "actionId": "A_1", "behaviour": "2", "key": "K_A", "modifier": "SHIFT CAPS" }]
      ],
      ['A_1', 'A', false,
        [{ "outchar": "A", "actionId": "A_1", "behaviour": "1", "key": "K_A", "modifier": "CAPS" },
        { "outchar": "A", "actionId": "A_1", "behaviour": "2", "key": "K_A", "modifier": "SHIFT" },
        { "outchar": "A", "actionId": "A_1", "behaviour": "2", "key": "K_A", "modifier": "SHIFT CAPS" }]
      ],
      ['A_9', 'a', true, [{ "outchar": "a", "actionId": "A_9", "behaviour": "0", "key": "K_A", "modifier": "NCAPS" }]],
      ['A_9', 'a', false, [{ "outchar": "a", "actionId": "A_9", "behaviour": "0", "key": "K_A", "modifier": "" }]],
      ['A_9', 'a', , [{ "outchar": "a", "actionId": "A_9", "behaviour": "0", "key": "K_A", "modifier": "" }]],
      ['A_9', '', true, [{ "outchar": "", "actionId": "A_9", "behaviour": "0", "key": "K_A", "modifier": "NCAPS" }]],
      ['A_9', '', false, [{ "outchar": "", "actionId": "A_9", "behaviour": "0", "key": "K_A", "modifier": "" }]],
      ['', 'a', true, []],
      ['', 'a', false, []],
      ['', '', , []],

    ].forEach(function (values) {
      it((JSON.stringify(values[3]).length > 35) ?
        ("getActionOutputBehaviourKeyModiFromActionIDStateOutput('" + values[0] + "', '" + values[1] + "', " + values[2] + ")").padEnd(67, " ") + ' should return an array of objects' :
        ("getActionOutputBehaviourKeyModiFromActionIDStateOutput('" + values[0] + "', '" + values[1] + "', " + values[2] + ")").padEnd(67, " ") + ' should return ' + "'" + JSON.stringify(values[3]) + "'", async function () {
          const result = sut.getActionOutputBehaviourKeyModiFromActionIDStateOutput(read, converted.arrayOfModifiers, String(values[0]), String(values[1]), Boolean(values[2]));
          assert.equal(JSON.stringify(result), JSON.stringify(values[3]));
        });
    });
  });
  describe('getKeyActionOutputArrayFromActionStateOutputArray ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sutR = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Test.keylayout');
    const read = sutR.read(inputFilename);

    const b6ActionIdArr: ActionStateOutput[] = [
      { "id": "A_0", "state": "1", "output": "ˆ" },
      { "id": "A_1", "state": "1", "output": "Â" },
      { "id": "A_10", "state": "1", "output": "ê" },
      { "id": "A_11", "state": "1", "output": "î" },
      { "id": "A_13", "state": "1", "output": "ô" },
      { "id": "A_14", "state": "1", "output": "û" },
      { "id": "A_2", "state": "1", "output": "Ê" },
      { "id": "A_3", "state": "1", "output": "Î" },
      { "id": "A_5", "state": "1", "output": "Ô" },
      { "id": "A_6", "state": "1", "output": "Û" },
      { "id": "A_9", "state": "1", "output": "â" }
    ];

    const b1KeycodeArr: KeylayoutFileData[] = [
      { keyCode: '49', key: 'K_SPACE', actionId: 'A_0', behaviour: '0', outchar: 'ˆ' },
      { keyCode: '49', key: 'K_SPACE', actionId: 'A_0', behaviour: '1', outchar: 'ˆ' },
      { keyCode: '49', key: 'K_SPACE', actionId: 'A_0', behaviour: '2', outchar: 'ˆ' },
      { keyCode: '49', key: 'K_SPACE', actionId: 'A_0', behaviour: '3', outchar: 'ˆ' },
      { keyCode: '6', key: 'K_Z', actionId: 'A_0', behaviour: '4', outchar: 'ˆ' },
      { keyCode: '25', key: 'K_9', actionId: 'A_0', behaviour: '4', outchar: 'ˆ' },
      { keyCode: '43', key: 'K_COMMA', actionId: 'A_0', behaviour: '4', outchar: 'ˆ' },
      { keyCode: '0', key: 'K_A', actionId: 'A_1', behaviour: '1', outchar: 'Â' },
      { keyCode: '0', key: 'K_A', actionId: 'A_1', behaviour: '2', outchar: 'Â' },
      { keyCode: '14', key: 'K_E', actionId: 'A_10', behaviour: '0', outchar: 'ê' },
      { keyCode: '34', key: 'K_I', actionId: 'A_11', behaviour: '0', outchar: 'î' },
      { keyCode: '31', key: 'K_O', actionId: 'A_13', behaviour: '0', outchar: 'ô' },
      { keyCode: '32', key: 'K_U', actionId: 'A_14', behaviour: '0', outchar: 'û' },
      { keyCode: '14', key: 'K_E', actionId: 'A_2', behaviour: '1', outchar: 'Ê' },
      { keyCode: '14', key: 'K_E', actionId: 'A_2', behaviour: '2', outchar: 'Ê' },
      { keyCode: '34', key: 'K_I', actionId: 'A_3', behaviour: '1', outchar: 'Î' },
      { keyCode: '34', key: 'K_I', actionId: 'A_3', behaviour: '2', outchar: 'Î' },
      { keyCode: '31', key: 'K_O', actionId: 'A_5', behaviour: '1', outchar: 'Ô' },
      { keyCode: '31', key: 'K_O', actionId: 'A_5', behaviour: '2', outchar: 'Ô' },
      { keyCode: '32', key: 'K_U', actionId: 'A_6', behaviour: '1', outchar: 'Û' },
      { keyCode: '32', key: 'K_U', actionId: 'A_6', behaviour: '2', outchar: 'Û' },
      { keyCode: '0', key: 'K_A', actionId: 'A_9', behaviour: '0', outchar: 'â' }
    ];

    [[b6ActionIdArr, b1KeycodeArr],
    ].forEach(function (values) {
      it(("getKeyActionOutputArrayFromActionStateOutputArray([['" + JSON.stringify(values[0]) + "'],..])").padEnd(73, " ") + '1 should return an array of objects', async function () {
        const result = sut.getKeyActionOutputArrayFromActionStateOutputArray(read, values[0] as ActionStateOutput[]);
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });

    const oneEntryResult = [
      { keyCode: '49', key: 'K_SPACE', actionId: 'A_0', behaviour: '0', outchar: 'ˆ' },
      { keyCode: '49', key: 'K_SPACE', actionId: 'A_0', behaviour: '1', outchar: 'ˆ' },
      { keyCode: '49', key: 'K_SPACE', actionId: 'A_0', behaviour: '2', outchar: 'ˆ' },
      { keyCode: '49', key: 'K_SPACE', actionId: 'A_0', behaviour: '3', outchar: 'ˆ' },
      { keyCode: '6', key: 'K_Z', actionId: 'A_0', behaviour: '4', outchar: 'ˆ' },
      { keyCode: '25', key: 'K_9', actionId: 'A_0', behaviour: '4', outchar: 'ˆ' },
      { keyCode: '43', key: 'K_COMMA', actionId: 'A_0', behaviour: '4', outchar: 'ˆ' }
    ];

    const oneEntryResultNoOutput = [
      { keyCode: '49', key: 'K_SPACE', actionId: 'A_0', behaviour: '0', outchar: '' },
      { keyCode: '49', key: 'K_SPACE', actionId: 'A_0', behaviour: '1', outchar: '' },
      { keyCode: '49', key: 'K_SPACE', actionId: 'A_0', behaviour: '2', outchar: '' },
      { keyCode: '49', key: 'K_SPACE', actionId: 'A_0', behaviour: '3', outchar: '' },
      { keyCode: '6', key: 'K_Z', actionId: 'A_0', behaviour: '4', outchar: '' },
      { keyCode: '25', key: 'K_9', actionId: 'A_0', behaviour: '4', outchar: '' },
      { keyCode: '43', key: 'K_COMMA', actionId: 'A_0', behaviour: '4', outchar: '' },
    ];

    [[[{ "id": "A_0", "state": "1", "output": "ˆ" }], oneEntryResult],
    [[{ "id": "A_0", "state": "1", "output": "" }], oneEntryResultNoOutput],
    [[{ "id": "A_0", "state": "", "output": "ˆ" }], oneEntryResult],
    ].forEach(function (values) {
      it(("getKeyActionOutputArrayFromActionStateOutputArray(['" + JSON.stringify(values[0]) + "'])").padEnd(73, " ") + ' should return an array of objects', async function () {
        const result = sut.getKeyActionOutputArrayFromActionStateOutputArray(read, values[0] as ActionStateOutput[]);
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });

    [[[{ "id": "", "state": "1", "output": "ˆ" }], []],
    [[{ "id": "", "state": "", "output": "" }], []],
    [[{ "id": " ", "state": " ", "output": "" }], []],

    ].forEach(function (values) {
      it(("getKeyActionOutputArrayFromActionStateOutputArray(" + JSON.stringify(values[0]) + ")").padEnd(73, " ") + ' should return ' + "'[" + JSON.stringify(values[1]) + "]'", async function () {
        const result = sut.getKeyActionOutputArrayFromActionStateOutputArray(read, values[0] as ActionStateOutput[]);
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });

    [[, []],
    [undefined, []],
    [null, []],
    ].forEach(function (values) {
      it(("getKeyActionOutputArrayFromActionStateOutputArray(" + JSON.stringify(values[0]) + ")").padEnd(73, " ") + ' should return ' + "'[" + JSON.stringify(values[1]) + "]'", async function () {
        const result = sut.getKeyActionOutputArrayFromActionStateOutputArray(read, values[0] as ActionStateOutput[]);
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });
  });

  describe('createRuleData ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sutR = new KeylayoutFileReader(compilerTestCallbacks);

    [
      [
        ['../data/Test_C0.keylayout'],
        [new Rule("C0", '', '', 0, 0, '', '', 0, 0, 'NCAPS', 'K_A', new TextEncoder().encode('a')),
        new Rule("C0", '', '', 0, 0, '', '', 0, 0, 'CAPS', 'K_A', new TextEncoder().encode('A')),
        new Rule("C0", '', '', 0, 0, '', '', 0, 0, 'NCAPS', 'K_S', new TextEncoder().encode('s')),
        new Rule("C0", '', '', 0, 0, '', '', 0, 0, 'NCAPS', 'K_D', new TextEncoder().encode('d'))]
      ],
      [
        ['../data/Test_C1.keylayout'],
        [new Rule("C1", '', '', 0, 0, '', '', 0, 0, 'NCAPS', 'K_S', new TextEncoder().encode('s')),
        new Rule("C1", '', '', 0, 0, '', '', 0, 0, 'CAPS', 'K_S', new TextEncoder().encode('S'))]
      ],
      [
        ['../data/Test_C2.keylayout'],
        [new Rule("C2", '', '', 0, 0, 'NCAPS', 'K_U', 1, 1, 'CAPS', 'K_A', new TextEncoder().encode('Â'))],
      ],
      [
        ['../data/Test_C3.keylayout'],
        [new Rule("C3", 'NCAPS SHIFT', 'K_D', 2, 1, 'NCAPS', 'K_U', 1, 2, 'CAPS', 'K_A', new TextEncoder().encode('Â'))]
      ],

      [
        ['../data/Test_C3_several.keylayout'],
        [new Rule("C3", 'NCAPS RALT', 'K_8', 3, 1, 'CAPS', 'K_U', 1, 3, 'NCAPS', 'K_A', new TextEncoder().encode('â')),
        new Rule("C3", 'NCAPS RALT', 'K_8', 3, 0, 'CAPS', 'K_U', 1, 0, 'NCAPS RALT', 'K_A', new TextEncoder().encode('â')),
        new Rule("C3", 'NCAPS RALT', 'K_8', 3, 0, 'NCAPS RALT', 'K_U', 2, 2, 'NCAPS', 'K_A', new TextEncoder().encode('â')),
        new Rule("C3", 'NCAPS RALT', 'K_8', 3, 0, 'NCAPS RALT', 'K_U', 2, 0, 'NCAPS RALT', 'K_A', new TextEncoder().encode('â'))]
      ],
      [
        ['../data/Test_C0_C1_C2_C3.keylayout'],
        [new Rule("C0", '', '', 0, 0, '', '', 0, 0, 'CAPS', 'K_A', new TextEncoder().encode('A')),
        new Rule("C2", '', '', 0, 0, 'NCAPS RALT', 'K_EQUAL', 1, 1, 'CAPS', 'K_D', new TextEncoder().encode('Â')),
        new Rule("C1", '', '', 0, 0, '', '', 0, 0, 'CAPS', 'K_S', new TextEncoder().encode('S')),
        new Rule("C1", '', '', 0, 0, '', '', 0, 0, 'NCAPS RALT', 'K_U', new TextEncoder().encode('S')),
        new Rule("C3", 'NCAPS RALT', 'K_8', 6, 1, 'CAPS', 'K_S', 2, 6, 'CAPS', 'K_D', new TextEncoder().encode('Â')),
        new Rule("C3", 'NCAPS RALT', 'K_8', 6, 0, 'CAPS', 'K_U', 3, 3, 'CAPS', 'K_D', new TextEncoder().encode('Â')),
        new Rule("C3", 'NCAPS RALT', 'K_8', 6, 0, 'NCAPS RALT', 'K_S', 4, 4, 'CAPS', 'K_D', new TextEncoder().encode('Â')),
        new Rule("C3", 'NCAPS RALT', 'K_8', 6, 0, 'NCAPS RALT', 'K_U', 5, 5, 'CAPS', 'K_D', new TextEncoder().encode('Â')),
        new Rule("C1", '', '', 0, 0, '', '', 0, 0, 'CAPS', 'K_S', new TextEncoder().encode('S')),
        new Rule("C1", '', '', 0, 0, '', '', 0, 0, 'NCAPS RALT', 'K_U', new TextEncoder().encode('S')),
        new Rule("C3", 'NCAPS RALT', 'K_8', 6, 0, 'CAPS', 'K_S', 2, 0, 'CAPS', 'K_D', new TextEncoder().encode('Â')),
        new Rule("C3", 'NCAPS RALT', 'K_8', 6, 0, 'CAPS', 'K_U', 3, 0, 'CAPS', 'K_D', new TextEncoder().encode('Â')),
        new Rule("C3", 'NCAPS RALT', 'K_8', 6, 0, 'NCAPS RALT', 'K_S', 4, 0, 'CAPS', 'K_D', new TextEncoder().encode('Â')),
        new Rule("C3", 'NCAPS RALT', 'K_8', 6, 0, 'NCAPS RALT', 'K_U', 5, 0, 'CAPS', 'K_D', new TextEncoder().encode('Â')),
        new Rule("C1", '', '', 0, 0, '', '', 0, 0, 'CAPS', 'K_U', new TextEncoder().encode('U')),]
      ],
    ].forEach(function (values: any) {
      it('data of \'' + values[0] + "' passed into createRuleData() " + 'should create an array of rules', async function () {
        const inputFilename = makePathToFixture(values[0][0]);
        const read = sutR.read(inputFilename);
        const outArray = sut.convertBound.convert(read, inputFilename.replace(/\.keylayout$/, '.kmn'));
        assert.deepEqual(outArray.arrayOfRules[0], values[1][0]);
      });
    });
  });

});
