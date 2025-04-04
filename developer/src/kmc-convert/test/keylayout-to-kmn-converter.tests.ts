/*
 * Keyman is 2025 copyright (C) SIL International. MIT License.
 *
 * Tests for KeylayoutToKmnConverter, KeylayoutFileReader, KmnFileWriter
 *
 */

import 'mocha';
import { compilerTestCallbacks, compilerTestOptions, makePathToFixture } from './helpers/index.js';
import { KeylayoutToKmnConverter } from '../src/keylayout-to-kmn/keylayout-to-kmn-converter.js';
import { KeylayoutFileReader } from '../src/keylayout-to-kmn/keylayout-file-reader.js';
import { rejects, doesNotReject } from "node:assert";


describe('KeylayoutToKmnConverter', function () {

  before(function () {
    compilerTestCallbacks.clear();
  });

  describe('run()', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);

    it('run() should throw on null input file name and null output file name', async function () {
      await rejects(async () => sut.run(null, null));
    });

    it('run() chai as promisedshould throw on null input file name and empty output file name', async function () {
      await rejects(async () => sut.run(null, ""));
    });

    it('run() chai as promisedshould throw on null input file name and unknown output file name', async function () {
      await rejects(async () => sut.run(null, "X"));
    });

    it('run() should throw on unavailable input file name and null output file name', async function () {
      const inputFilename = makePathToFixture('../data/Unavailable.keylayout');
      await rejects(async () => sut.run(inputFilename, null));
    });

    it('run() should return on correct input file name and empty output file name ', async function () {
      const inputFilename = makePathToFixture('../data/Italian.keylayout');
      await doesNotReject(async () => sut.run(inputFilename, ""));
    });

    it('run() should return on correct input file name and null output file name', async function () {
      const inputFilename = makePathToFixture('../data/Italian.keylayout');
      await doesNotReject(async () => sut.run(inputFilename, null));
    });

    it('run() should return on correct input file name and given output file name ', async function () {
      const inputFilename = makePathToFixture('../data/Italian.keylayout');
      const outputFilename = makePathToFixture('../../data/OutputName.kmn');
      await doesNotReject(async () => sut.run(inputFilename, outputFilename));
    });

    it('run() should throw on incorrect input file extention and output file extention', async function () {
      const inputFilename = makePathToFixture('../data/Italian_command.A');
      const outputFilename = makePathToFixture('../../data/OutputXName.B');
      await rejects(async () => sut.run(inputFilename, outputFilename));
    });

    it('run() return on correct input file extention and unsupperted output file extention', async function () {
      const inputFilename = makePathToFixture('../data/Italian.keylayout');
      const outputFilename = makePathToFixture('../../data/OutputXName.B');
      await doesNotReject(async () => sut.run(inputFilename, outputFilename));
    });
  });

  describe('convert()', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Italian.keylayout');
    const read = sut_r.read(inputFilename);
    const converted = sut.convert(read);

    // empty convert_object from unavailable file name
    const inputFilename_unavailable = makePathToFixture('X.keylayout');
    const read_unavailable = sut_r.read(inputFilename_unavailable);
    const converted_unavailable = sut.convert(read_unavailable);

    // empty convert_object from empty filename
    const inputFilename_empty = makePathToFixture('');
    const read_empty = sut_r.read(inputFilename_empty);
    const converted_empty = sut.convert(read_empty);

    it('should return converted array on correct input', async function () {
      await (doesNotReject(async () => (converted.arrayOf_Rules.length !== 0)));
    });

    it('should return empty on empty input', async function () {
      await (doesNotReject(async () => (converted_empty.keylayout_filename === ''
        && converted_empty.arrayOf_Modifiers.length === 0
        && converted_empty.arrayOf_Rules.length === 0)));
    });

    it('should return empty on only name as input', async function () {
      await (doesNotReject(async () => (converted_unavailable.keylayout_filename === ''
        && converted_unavailable.arrayOf_Modifiers.length === 0
        && converted_unavailable.arrayOf_Rules.length === 0)));
    });

    it('should return empty on only modifiers as input', async function () {
      const converted_mod = sut.convert({
        keylayout_filename: '',
        arrayOf_Modifiers: [['caps'], ['Shift'], ['command']],
        arrayOf_Rules: []
      });
      await (doesNotReject(async () => (converted_mod.keylayout_filename === ''
        && converted_mod.arrayOf_Modifiers.length === 0
        && converted_mod.arrayOf_Rules.length === 0)));
    });

    it('should return empty on only rules as input', async function () {
      const converted_rule = sut.convert({
        keylayout_filename: '',
        arrayOf_Modifiers: [],
        arrayOf_Rules: [["C0", "", "", 0, 0, "", "", 0, 0, "CAPS", "K_A", "A"]]
      });
      await (doesNotReject(async () => (converted_rule.keylayout_filename === ''
        && converted_rule.arrayOf_Modifiers.length === 0
        && converted_rule.arrayOf_Rules.length === 0)));
    });
  });

  describe('create_kmn_modifier', function () {
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
        await doesNotReject(async () => (sut.create_kmn_modifier(values[0] as string, values[1] as boolean) === values[2]));
      });
    });
  });

  describe('isAcceptableKeymanModifier', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    [
      ["NCAPS", true],
      ["NxCAPS", false],
      ["SHIFT", true],
      ["ALT", true],
      ["RALT", true],
      ["LALT", true],
      ["CTRL", true],
      ["LCTRL", true],
      ["RCTRL", true],
      ["LCTRL CAPS", true],
      ["LCTRL X", false],
      ["", true],
      [null, false],
    ].forEach(function (values) {

      it(("isAcceptableKeymanModifier(" + values[0] + ")").padEnd(38, " ") + ' should return ' + values[1], async function () {
        await doesNotReject(async () => sut.isAcceptableKeymanModifier(values[0] as string) === values[1]);
      });


    });
  });

  describe('map_UkeleleKC_To_VK', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    [
      [0x00, "K_A"],
      [0x31, "K_SPACE"],
      [0x18, "K_EQUAL"],
      [0x10, "K_Y"],
      [0x18, "K_EQUAL"],
      [0x21, "K_LBRKT"],
      [0x999, ""],
      [-1, ""],
      [null, ""],
    ].forEach(function (values) {
      it(("map_UkeleleKC_To_VK(" + values[0] + ")").padEnd(26, " ") + "should return " + "'" + values[1] + "'", async function () {
        await doesNotReject(async () => sut.map_UkeleleKC_To_VK(values[0] as number) === values[1]);
      });
    });
  });

  describe('checkIfCapsIsUsed', function () {
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
      it(("checkIfCapsIsUsed(" + values[0] + ")").padEnd(26, " ") + "should return " + "'" + values[1] + "'", async function () {
        await doesNotReject(async () => sut.checkIfCapsIsUsed(values[0] as string[][]) === values[1]);
      });
    });
  });

  describe('get_Modifier_array__From__KeyModifier_array', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Italian.keylayout');
    const read = sut_r.read(inputFilename);
    const converted = sut.convert(read);

    // strings as input
    [[[['0', 0]], [['', 'shift? caps? ']]],
    [[['0', 1]], [['anyShift caps?', 'shift? rightShift caps? ', 'shift? leftShift caps? ', 'shift leftShift caps ']]],
    [[['0', 999]], [null]],
    [[['999',]], [null]],
    [[['0', -999]], [null]],
    [[['0']], [null]],
    ].forEach(function (values) {
      it((values[1][0] !== null) ?
        ("get_Modifier_array__From__KeyModifier_array(['" + values[0][0][0] + "', '" + values[0][0][1] + "'])").padEnd(68, " ") + " should return ['" + values[1][0][0] + "', '" + values[1][0][1] + "']" :
        ("get_Modifier_array__From__KeyModifier_array(['" + values[0][0][0] + "', '" + values[0][0][1] + "'])").padEnd(68, " ") + " should return ['" + values[1][0] + "']", async function () {
          const result = sut.get_Modifier_array__From__KeyModifier_array(converted.arrayOf_Modifiers, values[0]);
          await doesNotReject(async () => JSON.stringify(result) === JSON.stringify(values[1]));
        });
    });

    // null, undefined or empty as input
    [[[[null]], [null]],
    [[[undefined]], [null]],
    [[[]], [null]],
    ].forEach(function (values) {
      it(("get_Modifier_array__From__KeyModifier_array([" + values[0][0] + "])").padEnd(68, " ") + " should return ['" + values[1][0] + "']", async function () {
        const result = sut.get_Modifier_array__From__KeyModifier_array(converted.arrayOf_Modifiers, values[0]);
        await doesNotReject(async () => JSON.stringify(result) === JSON.stringify(values[1]));
      });
    });
  });

  describe('get_KeyModifier_array__From__ActionID', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Italian.keylayout');
    const read = sut_r.read(inputFilename);
    [
      ["a16", [['32', 3]]],
      ["a19", [['45', 3]]],
      ["a18", [['24', 0], ['24', 3]]],
      ["unknown", []],
      [undefined, []],
      [null, []],
      [" ", []],
      ["", []],
    ].forEach(function (values) {
      let outstring = "[ ";
      for (let i = 0; i < values[1].length; i++) {
        outstring = outstring + "[ '" + values[1][i][0] + "', " + values[1][i][1].toString() + "], ";
      }

      it(("get_KeyModifier_array__From__ActionID('" + values[0] + "')").padEnd(57, " ") + ' should return ' + outstring.substring(0, outstring.lastIndexOf(']') + 2) + " ]", async function () {
        const result = sut.get_KeyModifier_array__From__ActionID(read, String(values[0]));
        await doesNotReject(async () => JSON.stringify(result) === JSON.stringify(values[1]));
      });
    });
  });

  describe('get_ActionID__From__ActionNext', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Italian.keylayout');
    const read = sut_r.read(inputFilename);
    [
      ["none", ""],
      ["a18", ""],
      ["0", ""],
      ["1", "a16"],
      ["2", "a8"],
      ["3", "a17"],
      ["", ""],
      [" ", ""],
      ["99", ""],
      [null, ""],
      [undefined, ""],
      ["unknown", ""],
    ].forEach(function (values) {
      it(("get_ActionID__From__ActionNext('" + values[0] + "')").padEnd(49, " ") + ' should return ' + "'" + values[1] + "'", async function () {
        const result = sut.get_ActionID__From__ActionNext(read, String(values[0]));
        await doesNotReject(async () => JSON.stringify(result) === JSON.stringify(values[1]));
      });
    });
  });

  describe('get_ActionIndex__From__ActionId', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Italian.keylayout');
    const read = sut_r.read(inputFilename);

    [
      ["none", 0],
      ["a16", 8],
      ["a18", 10],
      ["a19", 11],
      ["0", 0],
      ["", 0],
      [" ", 0],
      [null, 0],
      [undefined, 0],
      ["unknown", 0],
    ].forEach(function (values) {
      it(("get_ActionIndex__From__ActionId('" + values[0] + "')").padEnd(50, " ") + ' should return ' + values[1], async function () {
        const result = sut.get_ActionIndex__From__ActionId(read, String(values[0]));
        await doesNotReject(async () => JSON.stringify(result) === JSON.stringify(values[1]));
      });
    });
  });

  describe('get_Output__From__ActionId_None', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Italian.keylayout');
    const read = sut_r.read(inputFilename);

    [["a14", "u"],
    ["", ""],
    [" ", ""],
    ["a18", undefined],
    ["unknown", ""],
    ].forEach(function (values) {
      it(
        ("get_Output__From__ActionId_None('" + values[0] + "')").padEnd(56, " ") + ' should return ' + "'" + values[1] + "'", async function () {
          const result = sut.get_Output__From__ActionId_None(read, String(values[0]));
          await doesNotReject(async () => JSON.stringify(result) === JSON.stringify(values[1]));
        });
    });

    [[null, ""],
    [undefined, ""],
    [99, ""],
    ].forEach(function (values) {
      it(("get_Output__From__ActionId_None('" + values[0] + "')").padEnd(56, " ") + ' should return ' + values[1], async function () {
        const result = sut.get_Output__From__ActionId_None(read, String(values[0]));
        await doesNotReject(async () => JSON.stringify(result) === JSON.stringify(values[1]));
      });
    });
  });

  describe('get_KeyMBehaviourModOutputArray__from__KeyActionBehaviourOutput_array', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Italian.keylayout');
    const read = sut_r.read(inputFilename);

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

    // for non-empty/undefined/null  and isCaps_used = true
    [[b1_keycode_arr, b1_modifierKey_arr],
    [[['49', 'K_SPACE', 'a0', '0', 'ˆ']], [['K_SPACE', 'a0', '0', 'NCAPS', 'ˆ']]],
    [[['49', 'K_SPACE', 'a0', '0', '']], [['K_SPACE', 'a0', '0', 'NCAPS', '']]],
    [[['49', 'K_SPACE', 'a0', '', 'ˆ']], [['K_SPACE', 'a0', '', 'NCAPS', 'ˆ']]],
    [[['49', 'K_SPACE', '', '0', 'ˆ']], [['K_SPACE', '', '0', 'NCAPS', 'ˆ']]],
    [[['49', '', 'a0', '0', 'ˆ']], [['', 'a0', '0', 'NCAPS', 'ˆ']]],
    [[['', 'K_SPACE', 'a0', '0', 'ˆ']], [['K_SPACE', 'a0', '0', 'NCAPS', 'ˆ']]],
    [[['', 'K_SPACE', 'a0', '0', 'ˆ']], [['K_SPACE', 'a0', '0', 'NCAPS', 'ˆ']]],
    [[['', '', '', '', '']], [['', '', '', 'NCAPS', '']]],
    ].forEach(function (values) {
      const isCaps_used = true;
      const string_in = "get_KeyMBehaviourModOutputArray__from__KeyActionBehaviourOutput_array(['" + "', '" + values[0][0][0] + "', '" + values[0][0][1] + "', '" + values[0][0][2] + "', '" + values[0][0][3] + "', '" + values[0][0][4] + "'])";
      const string_out = "['" + "', '" + values[1][0][0] + "', '" + values[1][0][1] + "', '" + values[1][0][2] + "', '" + values[1][0][3] + "', '" + values[1][0][4] + "']";

      it((JSON.stringify(values[1]).length > 60) ? 'an array of objects should return an array of objectss' :
        string_in.padEnd(74, " ") + ' should return ' + string_out, async function () {
          const result = sut.get_KeyMBehaviourModOutputArray__from__KeyActionBehaviourOutput_array(read, values[0], isCaps_used);
          await doesNotReject(async () => JSON.stringify(result) === JSON.stringify(values[1]));
        });
    });

    // for non-empty/undefined/null  and isCaps_used = false
    [[[['49', 'K_SPACE', 'a0', '0', 'ˆ']], [['K_SPACE', 'a0', '0', '', 'ˆ']]],
    [[['49', 'K_SPACE', 'a0', '0', '']], [['K_SPACE', 'a0', '0', '', '']]],
    [[['', 'K_SPACE', 'a0', '0', 'ˆ']], [['K_SPACE', 'a0', '0', '', 'ˆ']]],
    [[['', '', '', '', '']], [['', '', '', '', '']]],
    ].forEach(function (values) {
      const isCaps_used = false;
      const string_in = "get_KeyMBehaviourModOutputArray__from__KeyActionBehaviourOutput_array(['" + "', '" + values[0][0][0] + "', '" + values[0][0][1] + "', '" + values[0][0][2] + "', '" + values[0][0][3] + "', '" + values[0][0][4] + "'])";
      const string_out = "['" + "', '" + values[1][0][0] + "', '" + values[1][0][1] + "', '" + values[1][0][2] + "', '" + values[1][0][3] + "', '" + values[1][0][4] + "']";

      it(string_in.padEnd(74, " ") + ' should return ' + string_out, async function () {
        const result = sut.get_KeyMBehaviourModOutputArray__from__KeyActionBehaviourOutput_array(read, values[0], isCaps_used);
        await doesNotReject(async () => JSON.stringify(result) === JSON.stringify(values[1]));
      });
    });

    // for empty/undefined/null
    [[[], []],
    [undefined, []],
    [null, []],
    ].forEach(function (values) {
      const isCaps = true;
      it(("get_KeyMBehaviourModOutputArray__from__KeyActionBehaviourOutput_array([" + values[0] + "])").padEnd(74, " ") + ' should return ' + "[" + values[1] + "]", async function () {
        const result = sut.get_KeyMBehaviourModOutputArray__from__KeyActionBehaviourOutput_array(read, values[0], isCaps);
        await doesNotReject(async () => JSON.stringify(result) === JSON.stringify(values[1]));
      });
    });
  });

  describe('get_ActionStateOutput_array__From__ActionState', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Italian.keylayout');
    const read = sut_r.read(inputFilename);

    [["1", [
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
    ["2", [
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
    ["789", [],],
    ["", [],],
    [" ", [],],
    [123, [],],
    [null, [],],
    [undefined, [],],
    ].forEach(function (values) {
      it((JSON.stringify(values[1]).length > 30) ?
        ("get_ActionStateOutput_array__From__ActionState('" + values[0] + "')").padEnd(60, " ") + ' should return an array of objects' :
        ("get_ActionStateOutput_array__From__ActionState('" + values[0] + "')").padEnd(60, " ") + ' should return ' + "'" + JSON.stringify(values[1]) + "'", async function () {
          const result = sut.get_ActionStateOutput_array__From__ActionState(read, String(values[0]));
          await doesNotReject(async () => JSON.stringify(result) === JSON.stringify(values[1]));
        });
    });
  });

  describe('get_ActionOutputBehaviourKeyModi_From__ActionIDStateOutput', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Italian.keylayout');
    const read = sut_r.read(inputFilename);
    const converted = sut.convert(read);

    [
      ["a1", "A", true, [
        ['a1', 'A', 'a1', '1', 'K_A', 'NCAPS SHIFT'],
        ['a1', 'A', 'a1', '1', 'K_A', 'SHIFT CAPS'],
        ['a1', 'A', 'a1', '2', 'K_A', 'CAPS']]
      ],
      ["a1", "A", false, [
        ['a1', 'A', 'a1', '1', 'K_A', 'SHIFT'],
        ['a1', 'A', 'a1', '1', 'K_A', 'SHIFT CAPS'],
        ['a1', 'A', 'a1', '2', 'K_A', 'CAPS']]
      ],
      ["a9", "a", true, [['a9', 'a', 'a9', '0', 'K_A', 'NCAPS']]],
      ["a9", "a", false, [['a9', 'a', 'a9', '0', 'K_A', '']]],
      ["a9", "a", , [['a9', 'a', 'a9', '0', 'K_A', '']]],
      ["a9", "", true, [["a9", "", "a9", "0", "K_A", "NCAPS"]]],
      ["a9", "", false, [["a9", "", "a9", "0", "K_A", ""]]],
      ["", "a", true, []],
      ["", "a", false, []],
      ["", "", , []],

    ].forEach(function (values) {
      it((JSON.stringify(values[3]).length > 35) ?
        ("get_ActionOutputBehaviourKeyModi_From__ActionIDStateOutput('" + values[0] + "', '" + values[1] + "', " + values[2] + ")").padEnd(67, " ") + ' should return an array of objects' :
        ("get_ActionOutputBehaviourKeyModi_From__ActionIDStateOutput('" + values[0] + "', '" + values[1] + "', " + values[2] + ")").padEnd(67, " ") + ' should return ' + "'" + JSON.stringify(values[3]) + "'", async function () {
          const result = sut.get_ActionOutputBehaviourKeyModi_From__ActionIDStateOutput(read, converted.arrayOf_Modifiers, String(values[0]), String(values[1]), Boolean(values[2]));
          await doesNotReject(async () => JSON.stringify(result) === JSON.stringify(values[3]));
        });
    });
  });

  describe('get_KeyActionOutput_array__From__ActionStateOutput_array', function () {
    const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const sut_r = new KeylayoutFileReader(compilerTestCallbacks);
    const inputFilename = makePathToFixture('../data/Italian.keylayout');
    const read = sut_r.read(inputFilename);

    const b6_actionId_arr = [
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

    [[b6_actionId_arr, b1_keycode_arr],
    ].forEach(function (values) {
      it(("get_KeyActionOutput_array__From__ActionStateOutput_array([['" + values[0][0][0] + "', '" + values[0][0][1] + "', '" + values[0][0][2] + "'],..])").padEnd(73, " ") + ' should return an array of objects', async function () {
        const result = sut.get_KeyActionOutput_array__From__ActionStateOutput_array(read, values[0]);
        await doesNotReject(async () => JSON.stringify(result) === JSON.stringify(values[1]));
      });
    });

    //------------------------------------------------------------------------------------------------------
    const oneEntryResult = [
      ["49", "K_SPACE", "a0", "0", "ˆ"],
      ["49", "K_SPACE", "a0", "1", "ˆ"],
      ["49", "K_SPACE", "a0", "2", "ˆ"],
      ["6", "K_Z", "a0", "4", "ˆ"],
      ["25", "K_9", "a0", "4", "ˆ"],
      ["43", "K_COMMA", "a0", "4", "ˆ"],
      ["49", "K_SPACE", "a0", "7", "ˆ"]
    ];
    const oneEntryResultNoOutput = [
      ["49", "K_SPACE", "a0", "0", ""],
      ["49", "K_SPACE", "a0", "1", ""],
      ["49", "K_SPACE", "a0", "2", ""],
      ["6", "K_Z", "a0", "4", ""],
      ["25", "K_9", "a0", "4", ""],
      ["43", "K_COMMA", "a0", "4", ""],
      ["49", "K_SPACE", "a0", "7", ""]
    ];

    [[[['a0', '1', 'ˆ']], oneEntryResult],
    [[['a0', '1', '']], oneEntryResultNoOutput],
    [[['a0', '', 'ˆ']], oneEntryResult],
    ].forEach(function (values) {
      it(("get_KeyActionOutput_array__From__ActionStateOutput_array(['" + values[0][0][0] + "', '" + values[0][0][1] + "', '" + values[0][0][2] + "'])").padEnd(73, " ") + ' should return an array of objects', async function () {
        const result = sut.get_KeyActionOutput_array__From__ActionStateOutput_array(read, values[0]);
        await doesNotReject(async () => JSON.stringify(result) === JSON.stringify(values[1]));
      });
    });

    //------------------------------------------------------------------------------------------------------
    [[[['', '1', 'ˆ']], []],
    [[['', '', '']], []],
    [[[' ', ' ', '']], []],
    ].forEach(function (values) {
      it(("get_KeyActionOutput_array__From__ActionStateOutput_array(['" + values[0][0][0] + "', '" + values[0][0][1] + "', '" + values[0][0][2] + "'])").padEnd(73, " ") + ' should return ' + "'[" + values[1] + "]'", async function () {
        const result = sut.get_KeyActionOutput_array__From__ActionStateOutput_array(read, values[0]);
        await doesNotReject(async () => JSON.stringify(result) === JSON.stringify(values[1]));
      });
    });

    //------------------------------------------------------------------------------------------------------
    [[[], []],
    [undefined, []],
    [null, []],
    ].forEach(function (values) {
      it(("get_KeyActionOutput_array__From__ActionStateOutput_array(" + values[0] + ")").padEnd(73, " ") + ' should return ' + "'[" + values[1] + "]'", async function () {
        const result = sut.get_KeyActionOutput_array__From__ActionStateOutput_array(read, values[0]);
        await doesNotReject(async () => JSON.stringify(result) === JSON.stringify(values[1]));
      });
    });
  });
});
