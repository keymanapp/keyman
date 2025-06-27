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
import { KeylayoutFileReader } from '../src/keylayout-to-kmn/keylayout-file-reader.js';
import { ConverterMessages } from '../src/converter-messages.js';
import * as NodeAssert from 'node:assert';

describe('KeylayoutToKmnConverter', function () {

  before(function () {
    compilerTestCallbacks.clear();
  });
  // todo remove
  describe('RunOneFile', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const inputFilename = makePathToFixture('../data/Italian.keylayout');
    sut.run(inputFilename);
    assert.isTrue(true);
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
      [makePathToFixture('../data/German_Complete.keylayout')],
      /* [makePathToFixture('../data/German_complete_reduced.keylayout')],
       [makePathToFixture('../data/German_Standard.keylayout')],*/
    ].forEach(function (files_) {
      sut.run(files_[0]);
      assert.isTrue(true);
    });
  });
  // todo remove
  describe('RunOneFile', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const inputFilename = makePathToFixture('../data/Test.keylayout');
    sut.run(inputFilename);
    assert.isTrue(true);
  });

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
      assert.deepEqual(compilerTestCallbacks.messages[0], ConverterMessages.Error_FileNotFound({ inputFilename: inputFilename }));
      assert.deepEqual(compilerTestCallbacks.messages[1], ConverterMessages.Error_UnableToRead({ inputFilename: inputFilename }));
    });

    it('run() should return on correct input file name and empty output file name ', async function () {
      const inputFilename = makePathToFixture('../data/Test.keylayout');
      await NodeAssert.doesNotReject(async () => sut.run(inputFilename, ''));
    });

    it('run() should return on correct input file name and null output file name', async function () {
      const inputFilename = makePathToFixture('../data/Test.keylayout');
      await NodeAssert.doesNotReject(async () => sut.run(inputFilename, null));
    });

    it('run() should return on correct input file name and given output file name ', async function () {
      const inputFilename = makePathToFixture('../data/Test.keylayout');
      const outputFilename = makePathToFixture('../data/OutputName.kmn');
      await NodeAssert.doesNotReject(async () => sut.run(inputFilename, outputFilename));
    });

    it('run() should throw on incorrect input file extention and output file extention', async function () {
      const inputFilename = makePathToFixture('../data/Test_command.A');
      const outputFilename = makePathToFixture('../data/data/OutputXName.B');
      const result = sut.run(inputFilename, outputFilename);
      assert.isNotNull(result);
      assert.equal(compilerTestCallbacks.messages.length, 2);
      assert.deepEqual(compilerTestCallbacks.messages[0], ConverterMessages.Error_FileNotFound({ inputFilename: inputFilename }));
      assert.deepEqual(compilerTestCallbacks.messages[1], ConverterMessages.Error_UnableToRead({ inputFilename: inputFilename }));
    });

    it('run() return on correct input file extention and unsupperted output file extention', async function () {
      const inputFilename = makePathToFixture('../data/Test.keylayout');
      const outputFilename = makePathToFixture('../data/OutputXName.B');
      await NodeAssert.doesNotReject(async () => sut.run(inputFilename, outputFilename));
    });
  });

  describe('convert() ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Test.keylayout');
    const read = sut_r.read(inputFilename);
    const converted = sut.convert_bound.convert(read, inputFilename.replace(/\.keylayout$/, '.kmn'));

    // empty convert_object from unavailable file name
    const inputFilename_unavailable = makePathToFixture('X.keylayout');
    const read_unavailable = sut_r.read(inputFilename_unavailable);
    const converted_unavailable = sut.convert_bound.convert(read_unavailable, inputFilename_unavailable.replace(/\.keylayout$/, '.kmn'));

    // empty convert_object from empty filename
    const inputFilename_empty = makePathToFixture('');
    const read_empty = sut_r.read(inputFilename_empty);
    const converted_empty = sut.convert_bound.convert(read_empty, inputFilename_empty.replace(/\.keylayout$/, '.kmn'));

    it('should return converted array on correct input', async function () {
      assert.isTrue(converted.arrayOf_Rules.length !== 0);
    });

    it('should return empty on empty input', async function () {
      assert.isTrue((converted_empty.keylayout_filename === ''
        && converted_empty.arrayOf_Modifiers.length === 0
        && converted_empty.arrayOf_Rules.length === 0));
    });

    it('should return empty on only name as input', async function () {
      assert.isTrue((converted_unavailable.keylayout_filename === ''
        && converted_unavailable.arrayOf_Modifiers.length === 0
        && converted_unavailable.arrayOf_Rules.length === 0));
    });

    it('should return empty on only modifiers as input', async function () {
      const converted_mod = sut.convert_bound.convert({
        keylayout_filename: '',
        arrayOf_Modifiers: [['caps'], ['Shift'], ['command']],
        arrayOf_Rules: []
      }, '');
      assert.isTrue((converted_mod.keylayout_filename === ''
        && converted_mod.arrayOf_Modifiers.length === 0
        && converted_mod.arrayOf_Rules.length === 0));
    });

    it('should return empty on only rules as input', async function () {
      const converted_rule = sut.convert_bound.convert({
        keylayout_filename: '',
        arrayOf_Modifiers: [],
        arrayOf_Rules: [['C0', '', '', 0, 0, '', '', 0, 0, 'CAPS', 'K_A', 'A']]
      }, '');
      assert.isTrue((converted_rule.keylayout_filename === ''
        && converted_rule.arrayOf_Modifiers.length === 0
        && converted_rule.arrayOf_Rules.length === 0));
    });
  });

  describe('create_kmn_modifier ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);

    [
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
    ].forEach(function (values) {
      it(('should convert "' + values[0] + '"').padEnd(36, " ") + 'to "' + values[2] + '"', async function () {
        const result = sut.create_kmn_modifier(values[0] as string, values[1] as boolean);
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

  describe('map_UkeleleKC_To_VK ', function () {
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
      it(("map_UkeleleKC_To_VK(" + values[0] + ")").padEnd(26, " ") + "should return " + "'" + values[1] + "'", async function () {
        const result = sut.map_UkeleleKC_To_VK(values[0] as number);
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

  describe('get_Modifier_array__From__KeyModifier_array ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Test.keylayout');
    const read = sut_r.read(inputFilename);
    const converted = sut.convert_bound.convert(read, inputFilename.replace(/\.keylayout$/, '.kmn'));

    [
      [[['0', 0]], [['', 'shift? caps? ']]],
      // Italian:  [[['0', 1]], [['anyShift caps?', 'shift? rightShift caps? ', 'shift? leftShift caps? ', 'shift leftShift caps ']]],
      [[['0', 2]], [['shift? leftShift caps? ', 'anyShift caps?', 'shift leftShift caps ', 'shift? rightShift caps? ']]],
      [[['0', 999]], [null]],
      [[['999',]], [null]],
      [[['0', -999]], [null]],
      [[['0']], [null]],

    ].forEach(function (values) {
      it((values[1][0] !== null) ?
        ("get_Modifier_array__From__KeyModifier_array(['" + values[0][0][0] + "', '" + values[0][0][1] + "'])").padEnd(68, " ") + " should return ['" + values[1][0][0] + "', '" + values[1][0][1] + "']" :
        ("get_Modifier_array__From__KeyModifier_array(['" + values[0][0][0] + "', '" + values[0][0][1] + "'])").padEnd(68, " ") + " should return ['" + values[1][0] + "']", async function () {
          const result = sut.get_Modifier_array__From__KeyModifier_array(converted.arrayOf_Modifiers, values[0]);
          assert.deepStrictEqual(JSON.stringify(result), JSON.stringify(values[1]));
        });
    });

    [[[[null]], [null]],
    [[[undefined]], [null]],
    [[[]], [null]],
    ].forEach(function (values) {
      it(("get_Modifier_array__From__KeyModifier_array([" + values[0][0] + "])").padEnd(68, " ") + " should return ['" + values[1][0] + "']", async function () {
        const result = sut.get_Modifier_array__From__KeyModifier_array(converted.arrayOf_Modifiers, values[0]);
        assert.deepStrictEqual(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });
  });

  describe('get_KeyModifier_array__From__ActionID ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Test.keylayout');
    const read = sut_r.read(inputFilename);

    [
      // Italian:   ['a16', [['32', 3]]],
      // Italian:   ['a19', [['45', 3]]],
      // Italian:   ['a18', [['24', 0], ['24', 3]]],
      ['A_16', [['32', 5]]],
      ['A_19', [['45', 5]]],
      ['A_18', [['24', 0], ['24', 5]]],

      ['unknown', []],
      [undefined, []],
      [null, []],
      [' ', []],
      ['', []],
    ].forEach(function (values) {

      let outstring = '[ ';
      for (let i = 0; i < values[1].length; i++) {
        outstring = outstring + "[ '" + values[1][i][0] + "', " + values[1][i][1].toString() + "], ";
      }

      it(("get_KeyModifier_array__From__ActionID('" + values[0] + "')").padEnd(57, " ") + ' should return ' + outstring.substring(0, outstring.lastIndexOf(']') + 2) + " ]", async function () {
        const result = sut.get_KeyModifier_array__From__ActionID(read, String(values[0]));
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });
  });

  describe('get_ActionID__From__ActionNext ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Test.keylayout');
    const read = sut_r.read(inputFilename);

    [
      ['none', ''],
      // Italian: ['a18', ''],
      ['0', ''],
      // Italian:  ['1', 'a16'],
      // Italian:  // Italian: ['2', 'a8'],
      // Italian:  ['3', 'a17'],

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
      it(("get_ActionID__From__ActionNext('" + values[0] + "')").padEnd(49, " ") + ' should return ' + "'" + values[1] + "'", async function () {
        const result = sut.get_ActionID__From__ActionNext(read, String(values[0]));
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });
  });

  describe('get_ActionIndex__From__ActionId ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Test.keylayout');
    const read = sut_r.read(inputFilename);

    [
      ['none', 0],
      // Italian:  ['a16', 8],
      // Italian:  ['a18', 10],
      // Italian:  ['a19', 11],
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
      it(("get_ActionIndex__From__ActionId('" + values[0] + "')").padEnd(50, " ") + ' should return ' + values[1], async function () {
        const result = sut.get_ActionIndex__From__ActionId(read, String(values[0]));
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });
  });

  describe('get_Output__From__ActionId_None ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Test.keylayout');
    const read = sut_r.read(inputFilename);

    [
      // Italian: ['a14', 'u'],
      ['A_14', 'u'],
      ['', ''],
      [' ', ''],
      // Italian: ['a18', undefined],
      ['A_18', undefined],
      ['unknown', ''],
    ].forEach(function (values) {
      it(
        ("get_Output__From__ActionId_None('" + values[0] + "')").padEnd(56, " ") + ' should return ' + "'" + values[1] + "'", async function () {
          const result = sut.get_Output__From__ActionId_None(read, String(values[0]));
          assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
        });
    });

    [[null, ''],
    [undefined, ''],
    [99, ''],
    ].forEach(function (values) {
      it(("get_Output__From__ActionId_None('" + values[0] + "')").padEnd(56, " ") + ' should return ' + values[1], async function () {
        const result = sut.get_Output__From__ActionId_None(read, String(values[0]));
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });
  });

  describe('get_KeyMBehaviourModOutputArray__from__KeyActionBehaviourOutput_array ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Test.keylayout');
    const read = sut_r.read(inputFilename);
    /*Italian: 
       
     const b1_keycode_arr = [
        ['49', 'K_SPACE', 'a0', '0', 'ˆ'],
        ['49', 'K_SPACE', 'a0', '1', 'ˆ'],
        ['49', 'K_SPACE', 'a0', '2', 'ˆ'],
        ['6', 'K_Z', 'a0', '4', 'ˆ'],
        ['25', 'K_9', 'a0', '4', 'ˆ'],
        ['43', 'K_COMMA', 'a0', '4', 'ˆ'],
        ['49', 'K_SPACE', 'a0', '7', 'ˆ'],
        ['0', 'K_A', 'a1', '1', 'Â'],
        ['0', 'K_A', 'a1', '2', 'Â'],
        ['14', 'K_E', 'a10', '0', 'ê'],
        ['34', 'K_I', 'a11', '0', 'î'],
        ['31', 'K_O', 'a13', '0', 'ô'],
        ['32', 'K_U', 'a14', '0', 'û'],
        ['14', 'K_E', 'a2', '1', 'Ê'],
        ['14', 'K_E', 'a2', '2', 'Ê'],
        ['34', 'K_I', 'a3', '1', 'Î'],
        ['34', 'K_I', 'a3', '2', 'Î'],
        ['31', 'K_O', 'a5', '1', 'Ô'],
        ['31', 'K_O', 'a5', '2', 'Ô'],
        ['32', 'K_U', 'a6', '1', 'Û'],
        ['32', 'K_U', 'a6', '2', 'Û'],
        ['0', 'K_A', 'a9', '0', 'â']
      ];
      const b1_modifierKey_arr = [
        ['K_SPACE', 'a0', '0', 'NCAPS', 'ˆ'],
        ['K_SPACE', 'a0', '1', 'NCAPS SHIFT', 'ˆ'],
        ['K_SPACE', 'a0', '1', 'SHIFT CAPS', 'ˆ'],
        ['K_SPACE', 'a0', '2', 'CAPS', 'ˆ'],
        ['K_Z', 'a0', '4', 'NCAPS SHIFT RALT', 'ˆ'],
        ['K_9', 'a0', '4', 'NCAPS SHIFT RALT', 'ˆ'],
        ['K_COMMA', 'a0', '4', 'NCAPS SHIFT RALT', 'ˆ'],
        ['K_SPACE', 'a0', '7', 'NCAPS CTRL', 'ˆ'],
        ['K_SPACE', 'a0', '7', 'NCAPS RALT CTRL', 'ˆ'],
        ['K_A', 'a1', '1', 'NCAPS SHIFT', 'Â'],
        ['K_A', 'a1', '1', 'SHIFT CAPS', 'Â'],
        ['K_A', 'a1', '2', 'CAPS', 'Â'],
        ['K_E', 'a10', '0', 'NCAPS', 'ê'],
        ['K_I', 'a11', '0', 'NCAPS', 'î'],
        ['K_O', 'a13', '0', 'NCAPS', 'ô'],
        ['K_U', 'a14', '0', 'NCAPS', 'û'],
        ['K_E', 'a2', '1', 'NCAPS SHIFT', 'Ê'],
        ['K_E', 'a2', '1', 'SHIFT CAPS', 'Ê'],
        ['K_E', 'a2', '2', 'CAPS', 'Ê'],
        ['K_I', 'a3', '1', 'NCAPS SHIFT', 'Î'],
        ['K_I', 'a3', '1', 'SHIFT CAPS', 'Î'],
        ['K_I', 'a3', '2', 'CAPS', 'Î'],
        ['K_O', 'a5', '1', 'NCAPS SHIFT', 'Ô'],
        ['K_O', 'a5', '1', 'SHIFT CAPS', 'Ô'],
        ['K_O', 'a5', '2', 'CAPS', 'Ô'],
        ['K_U', 'a6', '1', 'NCAPS SHIFT', 'Û'],
        ['K_U', 'a6', '1', 'SHIFT CAPS', 'Û'],
        ['K_U', 'a6', '2', 'CAPS', 'Û'],
        ['K_A', 'a9', '0', 'NCAPS', 'â']
      ];
  
      [[b1_keycode_arr, b1_modifierKey_arr],
      [[['49', 'K_SPACE', 'a0', '0', 'ˆ']], [['K_SPACE', 'a0', '0', 'NCAPS', 'ˆ']]],
      [[['49', 'K_SPACE', 'a0', '0', '']], [['K_SPACE', 'a0', '0', 'NCAPS', '']]],
      [[['49', 'K_SPACE', 'a0', '', 'ˆ']], [['K_SPACE', 'a0', '', 'NCAPS', 'ˆ']]],
      [[['49', 'K_SPACE', '', '0', 'ˆ']], [['K_SPACE', '', '0', 'NCAPS', 'ˆ']]],
      [[['49', '', 'a0', '0', 'ˆ']], [['', 'a0', '0', 'NCAPS', 'ˆ']]],
      [[['', 'K_SPACE', 'a0', '0', 'ˆ']], [['K_SPACE', 'a0', '0', 'NCAPS', 'ˆ']]],
      [[['', 'K_SPACE', 'a0', '0', 'ˆ']], [['K_SPACE', 'a0', '0', 'NCAPS', 'ˆ']]],
      [[['', '', '', '', '']], [['', '', '', 'NCAPS', '']]],
    */

    const b1_keycode_arr = [
      ['49', 'K_SPACE', 'A_0', '0', 'ˆ'],
      ['49', 'K_SPACE', 'A_0', '1', 'ˆ'],
      ['49', 'K_SPACE', 'A_0', '2', 'ˆ'],
      ['6', 'K_Z', 'A_0', '4', 'ˆ'],
      ['25', 'K_9', 'A_0', '4', 'ˆ'],
      ['43', 'K_COMMA', 'A_0', '4', 'ˆ'],
      ['49', 'K_SPACE', 'A_0', '3', 'ˆ'],
      ['0', 'K_A', 'A_1', '2', 'Â'],
      ['0', 'K_A', 'A_1', '1', 'Â'],
      ['14', 'K_E', 'A_10', '0', 'ê'],
      ['34', 'K_I', 'A_11', '0', 'î'],
      ['31', 'K_O', 'A_13', '0', 'ô'],
      ['32', 'K_U', 'A_14', '0', 'û'],
      ['14', 'K_E', 'A_2', '2', 'Ê'],
      ['14', 'K_E', 'A_2', '1', 'Ê'],
      ['34', 'K_I', 'A_3', '2', 'Î'],
      ['34', 'K_I', 'A_3', '1', 'Î'],
      ['31', 'K_O', 'A_5', '2', 'Ô'],
      ['31', 'K_O', 'A_5', '1', 'Ô'],
      ['32', 'K_U', 'A_6', '2', 'Û'],
      ['32', 'K_U', 'A_6', '1', 'Û'],
      ['0', 'K_A', 'A_9', '0', 'â']/**/
    ];
    const b1_modifierKey_arr = [
      ['K_SPACE', 'A_0', '0', 'NCAPS', 'ˆ'],
      ['K_SPACE', 'A_0', '1', 'CAPS', 'ˆ'],
      ['K_SPACE', 'A_0', '2', 'NCAPS SHIFT', 'ˆ'],
      ['K_SPACE', 'A_0', '2', 'SHIFT CAPS', 'ˆ'],
      ['K_Z', 'A_0', '4', 'NCAPS SHIFT RALT', 'ˆ'],
      ['K_9', 'A_0', '4', 'NCAPS SHIFT RALT', 'ˆ'],
      ['K_COMMA', 'A_0', '4', 'NCAPS SHIFT RALT', 'ˆ'],
      ['K_SPACE', 'A_0', '3', 'NCAPS RALT CTRL', 'ˆ'],
      ['K_SPACE', 'A_0', '3', 'NCAPS CTRL', 'ˆ'],
      ['K_A', 'A_1', '2', 'NCAPS SHIFT', 'Â'],
      ['K_A', 'A_1', '2', 'SHIFT CAPS', 'Â'],
      ['K_A', 'A_1', '1', 'CAPS', 'Â'],
      ['K_E', 'A_10', '0', 'NCAPS', 'ê'],
      ['K_I', 'A_11', '0', 'NCAPS', 'î'],
      ['K_O', 'A_13', '0', 'NCAPS', 'ô'],
      ['K_U', 'A_14', '0', 'NCAPS', 'û'],
      ['K_E', 'A_2', '2', 'NCAPS SHIFT', 'Ê'],
      ['K_E', 'A_2', '2', 'SHIFT CAPS', 'Ê'],
      ['K_E', 'A_2', '1', 'CAPS', 'Ê'],
      ['K_I', 'A_3', '2', 'NCAPS SHIFT', 'Î'],
      ['K_I', 'A_3', '2', 'SHIFT CAPS', 'Î'],
      ['K_I', 'A_3', '1', 'CAPS', 'Î'],
      ['K_O', 'A_5', '2', 'NCAPS SHIFT', 'Ô'],
      ['K_O', 'A_5', '2', 'SHIFT CAPS', 'Ô'],
      ['K_O', 'A_5', '1', 'CAPS', 'Ô'],
      ['K_U', 'A_6', '2', 'NCAPS SHIFT', 'Û'],
      ['K_U', 'A_6', '2', 'SHIFT CAPS', 'Û'],
      ['K_U', 'A_6', '1', 'CAPS', 'Û'],
      ['K_A', 'A_9', '0', 'NCAPS', 'â']/**/
    ];

    [[b1_keycode_arr, b1_modifierKey_arr],
    [[['49', 'K_SPACE', 'A_0', '0', 'ˆ']], [['K_SPACE', 'A_0', '0', 'NCAPS', 'ˆ']]],
    [[['49', 'K_SPACE', 'A_0', '0', '']], [['K_SPACE', 'A_0', '0', 'NCAPS', '']]],
    [[['49', 'K_SPACE', 'A_0', '', 'ˆ']], [['K_SPACE', 'A_0', '', 'NCAPS', 'ˆ']]],
    [[['49', 'K_SPACE', '', '0', 'ˆ']], [['K_SPACE', '', '0', 'NCAPS', 'ˆ']]],
    [[['49', '', 'A_0', '0', 'ˆ']], [['', 'A_0', '0', 'NCAPS', 'ˆ']]],
    [[['', 'K_SPACE', 'A_0', '0', 'ˆ']], [['K_SPACE', 'A_0', '0', 'NCAPS', 'ˆ']]],
    [[['', 'K_SPACE', 'A_0', '0', 'ˆ']], [['K_SPACE', 'A_0', '0', 'NCAPS', 'ˆ']]],
    [[['', '', '', '', '']], [['', '', '', 'NCAPS', '']]],
    ].forEach(function (values) {

      const isCaps_used = true;
      const string_in = "get_KeyMBehaviourModOutputArray__from__KeyActionBehaviourOutput_array(['" + "', '" + values[0][0][0] + "', '" + values[0][0][1] + "', '" + values[0][0][2] + "', '" + values[0][0][3] + "', '" + values[0][0][4] + "'])";
      const string_out = "['" + "', '" + values[1][0][0] + "', '" + values[1][0][1] + "', '" + values[1][0][2] + "', '" + values[1][0][3] + "', '" + values[1][0][4] + "']";

      it((JSON.stringify(values[1]).length > 60) ? 'an array of objects should return an array of objects' :
        string_in.padEnd(74, " ") + ' should return ' + string_out, async function () {
          const result = sut.get_KeyMBehaviourModOutputArray__from__KeyActionBehaviourOutput_array(read, values[0], isCaps_used);
          assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
        });
    });

    /* Italian:      [[[['49', 'K_SPACE', 'a0', '0', 'ˆ']], [['K_SPACE', 'a0', '0', '', 'ˆ']]],
     [[['49', 'K_SPACE', 'a0', '0', '']], [['K_SPACE', 'a0', '0', '', '']]],
     [[['', 'K_SPACE', 'a0', '0', 'ˆ']], [['K_SPACE', 'a0', '0', '', 'ˆ']]],
     [[['', '', '', '', '']], [['', '', '', '', '']]],
     ].forEach(function (values) {       */

    [[[['49', 'K_SPACE', 'A_0', '0', 'ˆ']], [['K_SPACE', 'A_0', '0', '', 'ˆ']]],
    [[['49', 'K_SPACE', 'A_0', '0', '']], [['K_SPACE', 'A_0', '0', '', '']]],
    [[['', 'K_SPACE', 'A_0', '0', 'ˆ']], [['K_SPACE', 'A_0', '0', '', 'ˆ']]],
    [[['', '', '', '', '']], [['', '', '', '', '']]],
    ].forEach(function (values) {

      const isCaps_used = false;
      const string_in = "get_KeyMBehaviourModOutputArray__from__KeyActionBehaviourOutput_array(['" + "', '" + values[0][0][0] + "', '" + values[0][0][1] + "', '" + values[0][0][2] + "', '" + values[0][0][3] + "', '" + values[0][0][4] + "'])";
      const string_out = "['" + "', '" + values[1][0][0] + "', '" + values[1][0][1] + "', '" + values[1][0][2] + "', '" + values[1][0][3] + "', '" + values[1][0][4] + "']";

      it(string_in.padEnd(74, " ") + ' should return ' + string_out, async function () {
        const result = sut.get_KeyMBehaviourModOutputArray__from__KeyActionBehaviourOutput_array(read, values[0], isCaps_used);
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });

    [[[], []],
    [undefined, []],
    [null, []],
    ].forEach(function (values) {
      const isCaps = true;
      it(("get_KeyMBehaviourModOutputArray__from__KeyActionBehaviourOutput_array([" + values[0] + "])").padEnd(74, " ") + ' should return ' + "[" + values[1] + "]", async function () {
        const result = sut.get_KeyMBehaviourModOutputArray__from__KeyActionBehaviourOutput_array(read, values[0], isCaps);
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });
  });

  describe('get_ActionStateOutput_array__From__ActionState ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Test.keylayout');
    const read = sut_r.read(inputFilename);

    /*  
      Italian:[['1', [
        ['a0', '1', 'ˆ'],
        ['a1', '1', 'Â'],
        ['a10', '1', 'ê'],
        ['a11', '1', 'î'],
        ['a13', '1', 'ô'],
        ['a14', '1', 'û'],
        ['a2', '1', 'Ê'],
        ['a3', '1', 'Î'],
        ['a5', '1', 'Ô'],
        ['a6', '1', 'Û'],
        ['a9', '1', 'â']],],
      ['2', [
        ['a0', '2', '`'],
        ['a1', '2', 'À'],
        ['a10', '2', 'è'],
        ['a11', '2', 'ì'],
        ['a13', '2', 'ò'],
        ['a14', '2', 'ù'],
        ['a2', '2', 'È'],
        ['a3', '2', 'Ì'],
        ['a5', '2', 'Ò'],
        ['a6', '2', 'Ù'],
        ['a9', '2', 'à']],],
    */
    [['1', [
      ['A_0', '1', 'ˆ'],
      ['A_1', '1', 'Â'],
      ['A_10', '1', 'ê'],
      ['A_11', '1', 'î'],
      ['A_13', '1', 'ô'],
      ['A_14', '1', 'û'],
      ['A_2', '1', 'Ê'],
      ['A_3', '1', 'Î'],
      ['A_5', '1', 'Ô'],
      ['A_6', '1', 'Û'],
      ['A_9', '1', 'â']],],
    ['2', [
      ['A_0', '2', '`'],
      ['A_1', '2', 'À'],
      ['A_10', '2', 'è'],
      ['A_11', '2', 'ì'],
      ['A_13', '2', 'ò'],
      ['A_14', '2', 'ù'],
      ['A_2', '2', 'È'],
      ['A_3', '2', 'Ì'],
      ['A_5', '2', 'Ò'],
      ['A_6', '2', 'Ù'],
      ['A_9', '2', 'à']],],
    ['789', [],],
    ['', [],],
    [' ', [],],
    [123, [],],
    [null, [],],
    [undefined, [],],
    ].forEach(function (values) {
      it((JSON.stringify(values[1]).length > 30) ?
        ("get_ActionStateOutput_array__From__ActionState('" + values[0] + "')").padEnd(60, " ") + ' should return an array of objects' :
        ("get_ActionStateOutput_array__From__ActionState('" + values[0] + "')").padEnd(60, " ") + ' should return ' + "'" + JSON.stringify(values[1]) + "'", async function () {
          const result = sut.get_ActionStateOutput_array__From__ActionState(read, String(values[0]));
          assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
        });
    });
  });

  describe('get_ActionOutputBehaviourKeyModi_From__ActionIDStateOutput ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Test.keylayout');
    const read = sut_r.read(inputFilename);
    const converted = sut.convert_bound.convert(read, inputFilename.replace(/\.keylayout$/, '.kmn'));
    /*  Italian:      [
        ['a1', 'A', true, [
          ['a1', 'A', 'a1', '1', 'K_A', 'NCAPS SHIFT'],
          ['a1', 'A', 'a1', '1', 'K_A', 'SHIFT CAPS'],
          ['a1', 'A', 'a1', '2', 'K_A', 'CAPS']]
        ],
        ['a1', 'A', false, [
          ['a1', 'A', 'a1', '1', 'K_A', 'SHIFT'],
          ['a1', 'A', 'a1', '1', 'K_A', 'SHIFT CAPS'],
          ['a1', 'A', 'a1', '2', 'K_A', 'CAPS']]
        ],
        ['a9', 'a', true, [['a9', 'a', 'a9', '0', 'K_A', 'NCAPS']]],
        ['a9', 'a', false, [['a9', 'a', 'a9', '0', 'K_A', '']]],
        ['a9', 'a', , [['a9', 'a', 'a9', '0', 'K_A', '']]],
        ['a9', '', true, [['a9', '', 'a9', '0', 'K_A', 'NCAPS']]],
        ['a9', '', false, [['a9', '', 'a9', '0', 'K_A', '']]],
        ['', 'a', true, []],
        ['', 'a', false, []],
        ['', '', , []],
      */

    [
      ['A_1', 'A', true, [
        ['A_1', 'A', 'A_1', '1', 'K_A', 'CAPS'],
        ['A_1', 'A', 'A_1', '2', 'K_A', 'NCAPS SHIFT'],
        ['A_1', 'A', 'A_1', '2', 'K_A', 'SHIFT CAPS'],
      ]
      ],
      ['A_1', 'A', false, [
        ['A_1', 'A', 'A_1', '1', 'K_A', 'CAPS'],
        ['A_1', 'A', 'A_1', '2', 'K_A', 'SHIFT'],
        ['A_1', 'A', 'A_1', '2', 'K_A', 'SHIFT CAPS']]
      ],
      ['A_9', 'a', true, [['A_9', 'a', 'A_9', '0', 'K_A', 'NCAPS']]],
      ['A_9', 'a', false, [['A_9', 'a', 'A_9', '0', 'K_A', '']]],
      ['A_9', 'a', , [['A_9', 'a', 'A_9', '0', 'K_A', '']]],
      ['A_9', '', true, [['A_9', '', 'A_9', '0', 'K_A', 'NCAPS']]],
      ['A_9', '', false, [['A_9', '', 'A_9', '0', 'K_A', '']]],
      ['', 'a', true, []],
      ['', 'a', false, []],
      ['', '', , []],

    ].forEach(function (values) {
      it((JSON.stringify(values[3]).length > 35) ?
        ("get_ActionOutputBehaviourKeyModi_From__ActionIDStateOutput('" + values[0] + "', '" + values[1] + "', " + values[2] + ")").padEnd(67, " ") + ' should return an array of objects' :
        ("get_ActionOutputBehaviourKeyModi_From__ActionIDStateOutput('" + values[0] + "', '" + values[1] + "', " + values[2] + ")").padEnd(67, " ") + ' should return ' + "'" + JSON.stringify(values[3]) + "'", async function () {
          const result = sut.get_ActionOutputBehaviourKeyModi_From__ActionIDStateOutput(read, converted.arrayOf_Modifiers, String(values[0]), String(values[1]), Boolean(values[2]));
          assert.equal(JSON.stringify(result), JSON.stringify(values[3]));
        });
    });
  });

  describe('get_KeyActionOutput_array__From__ActionStateOutput_array ', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Test.keylayout');
    const read = sut_r.read(inputFilename);

    /* Italian: const b6_actionId_arr = [
        ['a0', '1', 'ˆ'],
        ['a1', '1', 'Â'],
        ['a10', '1', 'ê'],
        ['a11', '1', 'î'],
        ['a13', '1', 'ô'],
        ['a14', '1', 'û'],
        ['a2', '1', 'Ê'],
        ['a3', '1', 'Î'],
        ['a5', '1', 'Ô'],
        ['a6', '1', 'Û'],
        ['a9', '1', 'â']
      ];
      const b1_keycode_arr = [
        ['49', 'K_SPACE', 'a0', '0', 'ˆ'],
        ['49', 'K_SPACE', 'a0', '1', 'ˆ'],
        ['49', 'K_SPACE', 'a0', '2', 'ˆ'],
        ['6', 'K_Z', 'a0', '4', 'ˆ'],
        ['25', 'K_9', 'a0', '4', 'ˆ'],
        ['43', 'K_COMMA', 'a0', '4', 'ˆ'],
        ['49', 'K_SPACE', 'a0', '7', 'ˆ'],
        ['0', 'K_A', 'a1', '1', 'Â'],
        ['0', 'K_A', 'a1', '2', 'Â'],
        ['14', 'K_E', 'a10', '0', 'ê'],
        ['34', 'K_I', 'a11', '0', 'î'],
        ['31', 'K_O', 'a13', '0', 'ô'],
        ['32', 'K_U', 'a14', '0', 'û'],
        ['14', 'K_E', 'a2', '1', 'Ê'],
        ['14', 'K_E', 'a2', '2', 'Ê'],
        ['34', 'K_I', 'a3', '1', 'Î'],
        ['34', 'K_I', 'a3', '2', 'Î'],
        ['31', 'K_O', 'a5', '1', 'Ô'],
        ['31', 'K_O', 'a5', '2', 'Ô'],
        ['32', 'K_U', 'a6', '1', 'Û'],
        ['32', 'K_U', 'a6', '2', 'Û'],
        ['0', 'K_A', 'a9', '0', 'â']
      ];
    */
    const b6_actionId_arr = [
      ['A_0', '1', 'ˆ'],
      ['A_1', '1', 'Â'],
      ['A_10', '1', 'ê'],
      ['A_11', '1', 'î'],
      ['A_13', '1', 'ô'],
      ['A_14', '1', 'û'],
      ['A_2', '1', 'Ê'],
      ['A_3', '1', 'Î'],
      ['A_5', '1', 'Ô'],
      ['A_6', '1', 'Û'],
      ['A_9', '1', 'â']
    ];
    const b1_keycode_arr = [


      ['49', 'K_SPACE', 'A_0', '0', 'ˆ'],
      ['49', 'K_SPACE', 'A_0', '1', 'ˆ'],
      ['49', 'K_SPACE', 'A_0', '2', 'ˆ'],
      ['49', 'K_SPACE', 'A_0', '3', 'ˆ'],
      ['6', 'K_Z', 'A_0', '4', 'ˆ'],
      ['25', 'K_9', 'A_0', '4', 'ˆ'],
      ['43', 'K_COMMA', 'A_0', '4', 'ˆ'],

      ['0', 'K_A', 'A_1', '1', 'Â'],
      ['0', 'K_A', 'A_1', '2', 'Â'],
      ['14', 'K_E', 'A_10', '0', 'ê'],
      ['34', 'K_I', 'A_11', '0', 'î'],
      ['31', 'K_O', 'A_13', '0', 'ô'],
      ['32', 'K_U', 'A_14', '0', 'û'],
      ['14', 'K_E', 'A_2', '1', 'Ê'],
      ['14', 'K_E', 'A_2', '2', 'Ê'],
      ['34', 'K_I', 'A_3', '1', 'Î'],
      ['34', 'K_I', 'A_3', '2', 'Î'],
      ['31', 'K_O', 'A_5', '1', 'Ô'],
      ['31', 'K_O', 'A_5', '2', 'Ô'],
      ['32', 'K_U', 'A_6', '1', 'Û'],
      ['32', 'K_U', 'A_6', '2', 'Û'],
      ['0', 'K_A', 'A_9', '0', 'â']
    ];

    [[b6_actionId_arr, b1_keycode_arr],
    ].forEach(function (values) {
      it(("get_KeyActionOutput_array__From__ActionStateOutput_array([['" + values[0][0][0] + "', '" + values[0][0][1] + "', '" + values[0][0][2] + "'],..])").padEnd(73, " ") + '1 should return an array of objects', async function () {
        const result = sut.get_KeyActionOutput_array__From__ActionStateOutput_array(read, values[0]);
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });

    //-----------------
    /* Italian:  const oneEntryResult = [
        ['49', 'K_SPACE', 'a0', '0', 'ˆ'],
        ['49', 'K_SPACE', 'a0', '1', 'ˆ'],
        ['49', 'K_SPACE', 'a0', '2', 'ˆ'],
        ['6', 'K_Z', 'a0', '4', 'ˆ'],
        ['25', 'K_9', 'a0', '4', 'ˆ'],
        ['43', 'K_COMMA', 'a0', '4', 'ˆ'],
        ['49', 'K_SPACE', 'a0', '7', 'ˆ']
      ];
      const oneEntryResultNoOutput = [
        ['49', 'K_SPACE', 'a0', '0', ''],
        ['49', 'K_SPACE', 'a0', '1', ''],
        ['49', 'K_SPACE', 'a0', '2', ''],
        ['6', 'K_Z', 'a0', '4', ''],
        ['25', 'K_9', 'a0', '4', ''],
        ['43', 'K_COMMA', 'a0', '4', ''],
        ['49', 'K_SPACE', 'a0', '7', '']
      ]      ;
    */

    const oneEntryResult = [
      ['49', 'K_SPACE', 'A_0', '0', 'ˆ'],
      ['49', 'K_SPACE', 'A_0', '1', 'ˆ'],
      ['49', 'K_SPACE', 'A_0', '2', 'ˆ'],
      ['49', 'K_SPACE', 'A_0', '3', 'ˆ'],
      ['6', 'K_Z', 'A_0', '4', 'ˆ'],
      ['25', 'K_9', 'A_0', '4', 'ˆ'],
      ['43', 'K_COMMA', 'A_0', '4', 'ˆ']
    ];
    const oneEntryResultNoOutput = [
      ['49', 'K_SPACE', 'A_0', '0', ''],
      ['49', 'K_SPACE', 'A_0', '1', ''],
      ['49', 'K_SPACE', 'A_0', '2', ''],
      ['49', 'K_SPACE', 'A_0', '3', ''],
      ['6', 'K_Z', 'A_0', '4', ''],
      ['25', 'K_9', 'A_0', '4', ''],
      ['43', 'K_COMMA', 'A_0', '4', '']
    ];

    [[[['A_0', '1', 'ˆ']], oneEntryResult],
    [[['A_0', '1', '']], oneEntryResultNoOutput],
    [[['A_0', '', 'ˆ']], oneEntryResult],
    ].forEach(function (values) {
      it(("get_KeyActionOutput_array__From__ActionStateOutput_array(['" + values[0][0][0] + "', '" + values[0][0][1] + "', '" + values[0][0][2] + "'])").padEnd(73, " ") + '2 should return an array of objects', async function () {
        const result = sut.get_KeyActionOutput_array__From__ActionStateOutput_array(read, values[0]);
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });

    //-------------------------------------------------------------------------------------
    [[[['', '1', 'ˆ']], []],
    [[['', '', '']], []],
    [[[' ', ' ', '']], []],
    ].forEach(function (values) {
      it(("get_KeyActionOutput_array__From__ActionStateOutput_array(['" + values[0][0][0] + "', '" + values[0][0][1] + "', '" + values[0][0][2] + "'])").padEnd(73, " ") + ' should return ' + "'[" + values[1] + "]'", async function () {
        const result = sut.get_KeyActionOutput_array__From__ActionStateOutput_array(read, values[0]);
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });

    //-------------------------------------------------------------------------------------
    [[[], []],
    [undefined, []],
    [null, []],
    ].forEach(function (values) {
      it(("get_KeyActionOutput_array__From__ActionStateOutput_array(" + values[0] + ")").padEnd(73, " ") + ' should return ' + "'[" + values[1] + "]'", async function () {
        const result = sut.get_KeyActionOutput_array__From__ActionStateOutput_array(read, values[0]);
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });
  });

});
