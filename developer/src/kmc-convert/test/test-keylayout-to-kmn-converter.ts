/*
 * Keyman is copyright (C) SIL International. MIT License.
 */

/*run with 
cd developer/src/kmc-convert/
./build.sh test*/



import 'mocha';
import { assert } from 'chai';
import { compilerTestCallbacks, compilerTestOptions } from './helpers/index.js';
import { KeylayoutToKmnConverter } from '../src/keylayout-to-kmn/keylayout-to-kmn-converter.js';
import { makePathToFixture } from './helpers/index.js';

//-----------------------------------------------------------------------------------------------------------------------

describe('KeylayoutToKmnConverter', function () {

  before(function () {
    compilerTestCallbacks.clear();
  });

  // convert_object
  const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);

  // convert_object from usable file name
  const inputFilename = makePathToFixture('../data/Italian_copy.keylayout');
  const read = sut.read(inputFilename);
  const converted = sut.convert(read);

  // empty convert_object from unavailable file name
  const inputFilename_unavailable = makePathToFixture('X.keylayout');
  const read_unavailable = sut.read(inputFilename_unavailable);
  const converted_unavailable = sut.convert(read_unavailable);

  // empty convert_object from empty filename
  const inputFilename_empty = makePathToFixture('');
  const read_empty = sut.read(inputFilename_empty);
  const converted_empty = sut.convert(read_empty);

  describe("run() ", function () {

    it('run() should throw on null inputs', async function () {
      // note, could use 'chai as promised' library to make this more fluent:
      let threw = false;
      try {
        await sut.run(null, null);
      } catch {
        threw = true;
      }
      assert.isTrue(threw);
    });

    it('run() should throw on wrong output file', async function () {
      let threw = false;
      try {
        await sut.run(null, "X");
      } catch {
        threw = true;
      }
      assert.isTrue(threw);
    });

    it('run() should throw on wrong intput file', async function () {
      let threw = false;
      try {
        await sut.run("X", null);
      } catch {
        threw = true;
      }
      assert.isTrue(threw);
    });


    it('run() should return true if ran OK', async function () {

      //copy into C:\Projects\keyman\keyman\developer\src\kmc-convert\test\DATA then they run OK
      //const inputFilename = makePathToFixture('../data/US_complete.keylayout');               // OK
      //const inputFilename  = makePathToFixture('../data/German_complete.keylayout');          // OK
      const inputFilename = makePathToFixture('../data/Italian_copy.keylayout');                // OK
      //const inputFilename  = makePathToFixture('../data/German_Standard_copy.keylayout');     // OK
      //const inputFilename  = makePathToFixture('../data/German_Standard2.keylayout');         // OK
      //const inputFilename  = makePathToFixture('../data/German_StandardTweaked.keylayout');   // NO C3
      //const inputFilename  = makePathToFixture('../data/Spanish_copy.keylayout');             // OK
      //const inputFilename  = makePathToFixture('../data/Latin_American_copy.keylayout');      // OK
      //const inputFilename  = makePathToFixture('../data/My_dk_Keyboard.keylayout');           // No
      //const inputFilename  = makePathToFixture('../data/Swiss_French_copy.keylayout');        // OK
      //const inputFilename  = makePathToFixture('../data/Swiss_German_copy.keylayout');        // OK
      //const inputFilename  = makePathToFixture('../data/US_copy.keylayout');                  // OK

      // TODO use path also
      // const outputFilename = "data//" +inputFilename.substring(0, inputFilename.lastIndexOf(".")) + ".kmn"
      const outputFilename = makePathToFixture('../data/MyResult.kmn');

      let threw = false;
      try {
        await sut.run(inputFilename, outputFilename);
      } catch {
        threw = true;
      }
      assert.isTrue(!threw);
    });
  })

  describe("read() ", function () {
    it('read() should return filled array on correct input', async function () {
      const result = sut.read(inputFilename);
      assert.isNotEmpty(result);
    });

    it('read() should return empty array  on null input', async function () {
      const result = sut.read(null);
      assert.isEmpty(result);
    });

    it('read() should return empty array  on empty input', async function () {
      const result = sut.read("");
      assert.isEmpty(result);
    });

    it('read() should return empty array  on space as input', async function () {
      const result = sut.read(" ");
      assert.isEmpty(result);
    });

    it('read() should return empty array  on unavailable file name', async function () {
      const result = sut.read(inputFilename_unavailable);
      assert.isEmpty(result);
    });

    it('read() should return empty array  on typo in path', async function () {
      const result = sut.read('../data|Italian_copy.keylayout');
      assert.isEmpty(result);
    });
  })

  describe("write() ", function () {
    it('write() should return true (no error) if no inputfile', async function () {
      const result = sut.write(converted_unavailable);
      assert.isTrue(result);
    });

    it('write() should return true (no error) if written', async function () {
      const result = sut.write(converted);
      assert.isTrue(result);
    });
  })

  describe("convert() ", function () {

    it('should return converted array on correct input', async function () {
      // we use 'converted' from above
      assert.isTrue(converted.arrayOf_Rules.length !== 0);
    });

    it('should return empty on empty input', async function () {
      // we use 'converted_empty' from above
      assert.isTrue((converted_empty.keylayout_filename === ''
        && converted_empty.arrayOf_Modifiers.length === 0
        && converted_empty.arrayOf_Rules.length === 0));
    });

    it('should return empty on only name as input', async function () {
      // we use 'converted_unavailable' from above
      assert.isTrue((converted_unavailable.keylayout_filename === ''
        && converted_unavailable.arrayOf_Modifiers.length === 0
        && converted_unavailable.arrayOf_Rules.length === 0));
    });

    it('should return empty on only modifiers as input', async function () {
      const converted_mod = sut.convert({
        keylayout_filename: '',
        arrayOf_Modifiers: [['caps'], ['Shift'], ['command']],
        arrayOf_Rules: []
      });
      assert.isTrue((converted_mod.keylayout_filename === ''
        && converted_mod.arrayOf_Modifiers.length === 0
        && converted_mod.arrayOf_Rules.length === 0));
    });

    it('should return empty on only rules as input', async function () {
      const converted_rule = sut.convert({
        keylayout_filename: '',
        arrayOf_Modifiers: [],
        arrayOf_Rules: [["C0", "", "", 0, 0, "", "", 0, 0, "CAPS", "K_A", "A"]]
      });
      assert.isTrue((converted_rule.keylayout_filename === ''
        && converted_rule.arrayOf_Modifiers.length === 0
        && converted_rule.arrayOf_Rules.length === 0));
    });
  })

  describe('create_kmn_modifier ', function () {
    [
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
      ['leftoption', true, 'NCAPS RALT'],
      ['loption', true, 'NCAPS RALT'],
    ].forEach(function (values) {
      it(('should convert "' + values[0] + '"').padEnd(36, " ") + 'to "' + values[2] + '"', async function () {
        const result = sut.create_kmn_modifier(values[0] as string, values[1] as boolean);
        assert.equal(result, values[2]);
      });
    })
  })

  describe("isAcceptableKeymanModifier ", function () {
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
        const result = sut.isAcceptableKeymanModifier(values[0] as string);
        assert.equal(result, values[1]);
      });
    })
  })

  describe("map_UkeleleKC_To_VK ", function () {
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
        const result = sut.map_UkeleleKC_To_VK(values[0] as number);
        assert.equal(result, values[1]);
      });
    })
  })

  describe("checkIfCapsIsUsed ", function () {

    it(("checkIfCapsIsUsed([['caps', 'xxx'], ['yyy']])").padEnd(45, " ") + " should return true", async function () {
      const result = sut.checkIfCapsIsUsed([["caps", "xxx"], ["yyy"]]);
      assert.isTrue(result);
    });

    it(("checkIfCapsIsUsed([['zzz', 'xxx'], ['yyy']])").padEnd(45, " ") + " should return false", async function () {
      const result = sut.checkIfCapsIsUsed([["zzz", "xxx"], ["yyy"]]);
      assert.isFalse(result);
    });
    it(("checkIfCapsIsUsed([['', ''], ['']])").padEnd(45, " ") + " should return false", async function () {
      const result = sut.checkIfCapsIsUsed([["", ""], [""]]);
      assert.isFalse(result);
    });
    it(("checkIfCapsIsUsed([null])").padEnd(45, " ") + " should return false", async function () {
      const result = sut.checkIfCapsIsUsed([null]);
      assert.isFalse(result);
    });
  })

  describe("get_KeyMap_Modifier_array__From__behaviour_arr ", function () {
    [[[['0', 0]], [['', 'shift? caps? ']]],
    [[['0', 1]], [['anyShift caps?', 'shift? rightShift caps? ', 'shift? leftShift caps? ', 'shift leftShift caps ']]],
    [[['0', 999]], [null]],
    [[['999',]], [null]],
    [[['0', -999]], [null]],
    [[['0']], [null]],
    ].forEach(function (values) {
      it((values[1][0] !== null) ?
        ("get_KeyMap_Modifier_array__From__behaviour_arr(['" + values[0][0][0] + "', '" + values[0][0][1] + "'])").padEnd(68, " ") + " should return ['" + values[1][0][0] + "', '" + values[1][0][1] + "']" :
        ("get_KeyMap_Modifier_array__From__behaviour_arr(['" + values[0][0][0] + "', '" + values[0][0][1] + "'])").padEnd(68, " ") + " should return ['" + values[1][0] + "']", async function () {
          const result = sut.get_KeyMap_Modifier_array__From__behaviour_arr(converted.arrayOf_Modifiers, values[0])
          assert.deepStrictEqual(JSON.stringify(result), JSON.stringify(values[1]));
        });
    });

    [[[[null]], [null]],
    [[[undefined]], [null]],
    [[[]], [null]],
    ].forEach(function (values) {
      it(("get_KeyMap_Modifier_array__From__behaviour_arr([" + values[0][0] + "])").padEnd(68, " ") + " should return ['" + values[1][0] + "']", async function () {
        const result = sut.get_KeyMap_Modifier_array__From__behaviour_arr(converted.arrayOf_Modifiers, values[0])
        assert.deepStrictEqual(JSON.stringify(result), JSON.stringify(values[1]));
      });
    })
  })

  describe("get_KeyMap_Code_array__From__ActionID_Action ", function () {
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
        outstring = outstring + "[ '" + values[1][i][0] + "', " + values[1][i][1].toString() + "], "
      }

      it(("get_KeyMap_Code_array__From__ActionID_Action('" + values[0] + "')").padEnd(57, " ") + ' should return ' + outstring.substring(0, outstring.lastIndexOf(']') + 2) + " ]", async function () {
        const result = sut.get_KeyMap_Code_array__From__ActionID_Action(read, String(values[0]));
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    })
  })

  describe("get_ActionID_Id__From__ActionID_next ", function () {
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
      it(("get_ActionID_Id__From__ActionID_next('" + values[0] + "')").padEnd(49, " ") + ' should return ' + "'" + values[1] + "'", async function () {
        const result = sut.get_ActionID_Id__From__ActionID_next(read, String(values[0]));
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    })
  })

  describe("get_ActionID_Index__From__ActionID_Id ", function () {
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
      it(("get_ActionID_Index__From__ActionID_Id('" + values[0] + "')").padEnd(50, " ") + ' should return ' + values[1], async function () {
        const result = sut.get_ActionID_Index__From__ActionID_Id(read, String(values[0]));
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    })
  })

  describe("get_Action2ID_NoneOutput__From__ActionID_Id ", function () {
    [["a14", "u"],
    ["", ""],
    [" ", ""],
    ["a18", undefined],
    ["unknown", ""],
    ].forEach(function (values) {
      it(
        ("get_Action2ID_NoneOutput__From__ActionID_Id('" + values[0] + "')").padEnd(56, " ") + ' should return ' + "'" + values[1] + "'", async function () {
          const result = sut.get_Action2ID_NoneOutput__From__ActionID_Id(read, String(values[0]));
          assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
        });
    });

    [[null, ""],
    [undefined, ""],
    [99, ""],
    ].forEach(function (values) {
      it(("get_Action2ID_NoneOutput__From__ActionID_Id('" + values[0] + "')").padEnd(56, " ") + ' should return ' + values[1], async function () {
        const result = sut.get_Action2ID_NoneOutput__From__ActionID_Id(read, String(values[0]));
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    })
  })

  describe("get_KeyMapModiKeyArray__from__array ", function () {

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
      ['K_SPACE', 'a0', '1', 'SHIFT NCAPS', 'ˆ'],
      ['K_SPACE', 'a0', '1', 'SHIFT CAPS', 'ˆ'],
      ['K_SPACE', 'a0', '2', 'CAPS', 'ˆ'],
      ['K_Z', 'a0', '4', 'SHIFT NCAPS RALT', 'ˆ'],
      ['K_9', 'a0', '4', 'SHIFT NCAPS RALT', 'ˆ'],
      ['K_COMMA', 'a0', '4', 'SHIFT NCAPS RALT', 'ˆ'],
      ['K_SPACE', 'a0', '7', 'NCAPS CTRL', 'ˆ'],
      ['K_SPACE', 'a0', '7', 'NCAPS RALT CTRL', 'ˆ'],
      ['K_A', 'a1', '1', 'SHIFT NCAPS', 'Â'],
      ['K_A', 'a1', '1', 'SHIFT CAPS', 'Â'],
      ['K_A', 'a1', '2', 'CAPS', 'Â'],
      ['K_E', 'a10', '0', 'NCAPS', 'ê'],
      ['K_I', 'a11', '0', 'NCAPS', 'î'],
      ['K_O', 'a13', '0', 'NCAPS', 'ô'],
      ['K_U', 'a14', '0', 'NCAPS', 'û'],
      ['K_E', 'a2', '1', 'SHIFT NCAPS', 'Ê'],
      ['K_E', 'a2', '1', 'SHIFT CAPS', 'Ê'],
      ['K_E', 'a2', '2', 'CAPS', 'Ê'],
      ['K_I', 'a3', '1', 'SHIFT NCAPS', 'Î'],
      ['K_I', 'a3', '1', 'SHIFT CAPS', 'Î'],
      ['K_I', 'a3', '2', 'CAPS', 'Î'],
      ['K_O', 'a5', '1', 'SHIFT NCAPS', 'Ô'],
      ['K_O', 'a5', '1', 'SHIFT CAPS', 'Ô'],
      ['K_O', 'a5', '2', 'CAPS', 'Ô'],
      ['K_U', 'a6', '1', 'SHIFT NCAPS', 'Û'],
      ['K_U', 'a6', '1', 'SHIFT CAPS', 'Û'],
      ['K_U', 'a6', '2', 'CAPS', 'Û'],
      ['K_A', 'a9', '0', 'NCAPS', 'â']
    ];

    // isCaps_used === true; input defined
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

      const isCaps_used = true
      const string_in = "get_KeyMapModiKeyArray__from__array(['" + "', '" + values[0][0][0] + "', '" + values[0][0][1] + "', '" + values[0][0][2] + "', '" + values[0][0][3] + "', '" + values[0][0][4] + "'])"
      const string_out = "['" + "', '" + values[1][0][0] + "', '" + values[1][0][1] + "', '" + values[1][0][2] + "', '" + values[1][0][3] + "', '" + values[1][0][4] + "']"

      it((JSON.stringify(values[1]).length > 60) ? 'an array of objects should return an array of objectss' :
        string_in.padEnd(74, " ") + ' should return ' + string_out, async function () {
          const result = sut.get_KeyMapModiKeyArray__from__array(read, values[0], isCaps_used);
          assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
        });
    });

    // isCaps_used === false; input defined
    [[[['49', 'K_SPACE', 'a0', '0', 'ˆ']], [['K_SPACE', 'a0', '0', '', 'ˆ']]],
    [[['49', 'K_SPACE', 'a0', '0', '']], [['K_SPACE', 'a0', '0', '', '']]],
    [[['', 'K_SPACE', 'a0', '0', 'ˆ']], [['K_SPACE', 'a0', '0', '', 'ˆ']]],
    [[['', '', '', '', '']], [['', '', '', '', '']]],
    ].forEach(function (values) {

      const isCaps_used = false
      const string_in = "get_KeyMapModiKeyArray__from__array(['" + "', '" + values[0][0][0] + "', '" + values[0][0][1] + "', '" + values[0][0][2] + "', '" + values[0][0][3] + "', '" + values[0][0][4] + "'])"
      const string_out = "['" + "', '" + values[1][0][0] + "', '" + values[1][0][1] + "', '" + values[1][0][2] + "', '" + values[1][0][3] + "', '" + values[1][0][4] + "']"

      it(string_in.padEnd(74, " ") + ' should return ' + string_out, async function () {
        const result = sut.get_KeyMapModiKeyArray__from__array(read, values[0], isCaps_used);
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });

    // isCaps_used === true; input undefined/null/empty
    [[[], []],
    [undefined, []],
    [null, []],
    ].forEach(function (values) {
      const isCaps = true
      it(("get_KeyMapModiKeyArray__from__array([" + values[0] + "])").padEnd(74, " ") + ' should return ' + "[" + values[1] + "]", async function () {
        const result = sut.get_KeyMapModiKeyArray__from__array(read, values[0], isCaps);
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });
  })

  describe("writeData_Stores() ", function () {

    const out_expected_first: string =
      "c ......................................................................\n"
      + "c ......................................................................\n"
      + "c Keyman keyboard generated by kmn-convert\n"
      + "c from Ukelele file: "

    const out_expected_last: string =
      "\n"
      + "c ......................................................................\n"
      + "c ......................................................................\n"
      + "\n"
      + "store(&VERSION) '10.0'\n"
      + "store(&TARGETS) 'any'\n"
      + "store(&KEYBOARDVERSION) '1.0'\n"
      + "store(&COPYRIGHT) '© 2024 SIL International'\n"
      + "\n"
      + "begin Unicode > use(main)\n\n"
      + "group(main) using keys\n\n"
      + "\n"

    it(('writeData_Stores should return store text with filename ').padEnd(62, " ") + 'on correct input', async function () {
      const written_correctName = sut.writeData_Stores(converted)
      assert.equal(written_correctName, (out_expected_first + converted.keylayout_filename + out_expected_last));
    });

    it(('writeData_Stores should return store text without filename ').padEnd(62, " ") + 'on empty input', async function () {
      const written_emptyName = sut.writeData_Stores(converted_empty)
      assert.equal(written_emptyName, (out_expected_first + out_expected_last));
    });

    it(('writeData_Stores should return store text without filename ').padEnd(62, " ") + 'on only filename as input', async function () {
      const written_onlyName = sut.writeData_Stores(converted_unavailable)
      assert.equal(written_onlyName, (out_expected_first + converted_unavailable.keylayout_filename + out_expected_last));
    });
  })

  describe("get_ActionID_Output_array__From__ActionID_State ", function () {
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
        ("get_ActionID_Output_array__From__ActionID_State('" + values[0] + "')").padEnd(60, " ") + ' should return an array of objects' :
        ("get_ActionID_Output_array__From__ActionID_State('" + values[0] + "')").padEnd(60, " ") + ' should return ' + "'" + JSON.stringify(values[1]) + "'", async function () {
          const result = sut.get_ActionID_Output_array__From__ActionID_State(read, String(values[0]));
          assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
        });
    })
  })

  describe("get_Datat_array2D__From__ActionID_stateOutput ", function () {
    [
      ["a1", "A", true, [
        ['a1', 'A', 'a1', '1', 'K_A', 'SHIFT NCAPS'],
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
        ("get_Datat_array2D__From__ActionID_stateOutput('" + values[0] + "', '" + values[1] + "', " + values[2] + ")").padEnd(67, " ") + ' should return an array of objects' :
        ("get_Datat_array2D__From__ActionID_stateOutput('" + values[0] + "', '" + values[1] + "', " + values[2] + ")").padEnd(67, " ") + ' should return ' + "'" + JSON.stringify(values[3]) + "'", async function () {
          const result = sut.get_Datat_array2D__From__ActionID_stateOutput(read, converted.arrayOf_Modifiers, String(values[0]), String(values[1]), Boolean(values[2]))
          assert.equal(JSON.stringify(result), JSON.stringify(values[3]));
        });
    })
  })

  describe("get_KeyMap_Code_array__From__KeyMap_Action_array2D ", function () {

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
      it(("get_KeyMap_Code_array__From__KeyMap_Action_array2D([['" + values[0][0][0] + "', '" + values[0][0][1] + "', '" + values[0][0][2] + "'],..])").padEnd(73, " ") + ' should return an array of objects', async function () {
        const result = sut.get_KeyMap_Code_array__From__KeyMap_Action_array2D(read, values[0])
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });

    //-----------------
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
      it(("get_KeyMap_Code_array__From__KeyMap_Action_array2D(['" + values[0][0][0] + "', '" + values[0][0][1] + "', '" + values[0][0][2] + "'])").padEnd(73, " ") + ' should return an array of objects', async function () {
        const result = sut.get_KeyMap_Code_array__From__KeyMap_Action_array2D(read, values[0])
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });

    //-----------------
    [[[['', '1', 'ˆ']], []],
    [[['', '', '']], []],
    [[[' ', ' ', '']], []],
    ].forEach(function (values) {
      it(("get_KeyMap_Code_array__From__KeyMap_Action_array2D(['" + values[0][0][0] + "', '" + values[0][0][1] + "', '" + values[0][0][2] + "'])").padEnd(73, " ") + ' should return ' + "'[" + values[1] + "]'", async function () {
        const result = sut.get_KeyMap_Code_array__From__KeyMap_Action_array2D(read, values[0])
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });

    //-----------------
    [[[], []],
    [undefined, []],
    [null, []],
    ].forEach(function (values) {
      it(("get_KeyMap_Code_array__From__KeyMap_Action_array2D(" + values[0] + ")").padEnd(73, " ") + ' should return ' + "'[" + values[1] + "]'", async function () {
        const result = sut.get_KeyMap_Code_array__From__KeyMap_Action_array2D(read, values[0])
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    });
  })

});
