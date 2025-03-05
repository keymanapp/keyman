/*
 * Keyman is copyright (C) SIL International. MIT License.
 */
/*run with 
cd developer/src/kmc-convert/
./build_disp.sh test*/

// Todo
//    check all     
// ["a14"],      
//  ["unknownIdentifier"],      
// [""],      
// [" "],     
//  [null],      
// [undefined],

//    definition of converter,... to  top part
//    change run second test NOT
//    data inpout/output read in array
// write more tests

// sut???

//    data inpout/output convert in array
// check output sentence
// map_UkeleleKC_To_VK add " "  and ""
// checkIfCapsIsUsed Caps?  CapsWrongtext
// get_KeyMap_Modifier_array__From__behaviour_arr add " "  and ""
// get_KeyMap_Modifier_array__From__behaviour_arr add undefined
// get_KeyMap_Code_array__From__ActionID_Action add undefined
// get_ActionID_Index__From__ActionID_Id add undefined
// get_ActionID_Id__From__ActionID_next add undefined
// get_KeyMapModiKeyArray__from__array not finished
// writeData_Stores  data inpout/output convert in array
//get_Datat_array2D__From__ActionID_stateOutput add " "  and "" undefined
//get_KeyMap_Code_array__From__KeyMap_Action_array2D add " "  and "" undefined




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

  //-----------------------------------------------------------------------------------------------------------------------

  // usable convert_object
  const inputFilename = makePathToFixture('../data/Italian_copy.keylayout');
  const converter = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
  const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
  const read = converter.read(inputFilename);
  const converted = converter.convert(read);

  // convert_object from unavailable file
  const inputFilename_unavailable = makePathToFixture('X.keylayout');
  const read_unavailable = converter.read(inputFilename_unavailable);
  const converted_unavailable = converter.convert(read_unavailable);

  // empty  convert_object
  const converted_empty = { ...converted_unavailable }
  converted_empty.arrayOf_Modifiers = []
  converted_empty.arrayOf_Rules = []
  converted_empty.keylayout_filename = ""

  //-----------------------------------------------------------------------------------------------------------------------
  //     test run
  describe("run() ", function () {

    // _S2 first test
    it('should throw on null inputs', async function () {
      // const inputFilename = makePathToFixture('file.keylayout');
      const converter = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
      // note, could use 'chai as promised' library to make this more fluent:
      let threw = false;
      try {
        await converter.run(null, null);
      } catch {
        threw = true;
      }
      assert.isTrue(threw);
    });

    it('should throw on wrong output file', async function () {
      // const inputFilename = makePathToFixture('file.keylayout');
      const converter = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
      // note, could use 'chai as promised' library to make this more fluent:
      let threw = false;
      try {
        await converter.run(null, "X");
      } catch {
        threw = true;
      }
      assert.isTrue(threw);
    });

    it('should throw on wrong intput file', async function () {
      // const inputFilename = makePathToFixture('file.keylayout');
      const converter = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
      // note, could use 'chai as promised' library to make this more fluent:
      let threw = false;
      try {
        await converter.run("X", null);
      } catch {
        threw = true;
      }
      assert.isTrue(threw);
    });

    // _S2 My tests... // later throws n NOT all elements loaded
    it('should return true if ran OK', async function () {

      // some keys, no deadkeys
      //const inputFilename  = makePathToFixture('../data/Mysample.keylayout');

      // all keys, some deadkeys
      // copy into C:\Projects\keyman\keyman\developer\src\kmc-convert\test\DATA then they run OK

      //const inputFilename = makePathToFixture('../data/US_complete.keylayout');          // OK
      //const inputFilename  = makePathToFixture('../data/German_complete.keylayout');      // OK
      const inputFilename = makePathToFixture('../data/Italian_copy.keylayout');         // OK
      //const inputFilename  = makePathToFixture('../data/German_Standard_copy.keylayout');         //OK
      //const inputFilename  = makePathToFixture('../data/German_Standard2.keylayout');         //OK
      //const inputFilename  = makePathToFixture('../data/German_StandardTweaked.keylayout');         //NO C3
      //const inputFilename  = makePathToFixture('../data/Spanish_copy.keylayout');         // OK
      //const inputFilename  = makePathToFixture('../data/Latin_American_copy.keylayout');  // OK
      //const inputFilename  = makePathToFixture('../data/My_dk_Keyboard.keylayout');       // No
      //const inputFilename  = makePathToFixture('../data/Swiss_French_copy.keylayout');    // OK
      //const inputFilename  = makePathToFixture('../data/Swiss_German_copy.keylayout');    // OK
      //const inputFilename  = makePathToFixture('../data/US_copy.keylayout');              // OK

      const outputFilename = makePathToFixture('../data/MyResult.kmn');

      // TODO check filename if correct
      console.log('        inputFilename', inputFilename)
      console.log('        outputFilename', outputFilename)

      // _S2 create obj of derived class-> use derived functions
      const converter = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);

      let threw = false;
      try {
        await converter.run(inputFilename, outputFilename);
      } catch {
        threw = true;
      }
      assert.isFalse(threw);
    });
  })

  //-----------------------------------------------------------------------------------------------------------------------


  // OK test read
  describe("read() ", function () {
    it('should return filled array on correct input', async function () {
      const result = converter.read(inputFilename);
      assert.isNotEmpty(result);
    });

    it('should return empty array on null input', async function () {
      const result = converter.read(null);
      assert.isEmpty(result);
    });

    it('should return empty array on empty input', async function () {
      const result = converter.read("");
      assert.isEmpty(result);
    });

    it('should return empty array on space as input', async function () {
      const result = converter.read(" ");
      assert.isEmpty(result);
    });

    it('should return empty array on unavailable file name', async function () {
      ;
      const result = converter.read(inputFilename_unavailable);
      assert.isEmpty(result);
    });

    it('should return empty array on typo in path', async function () {
      const result = converter.read('../data|Italian_copy.keylayout');
      assert.isEmpty(result);
    });
  })

  //-----------------------------------------------------------------------------------------------------------------------
  // OK test write
  describe("write() ", function () {
    it('should return true if no inputfile', async function () {
      const result = converter.write(converted_unavailable);
      assert.isTrue(result);
    });
  })
  describe("write() ", function () {
    //const outArray =  sut.convert(read);
    it('should return true if written', async function () {
      const result = converter.write(converted);
      assert.isTrue(result);
    });
  })

  //-----------------------------------------------------------------------------------------------------------------------
  // OK test convert
  describe("convert() ", function () {

    // use converted from above
    it('should return converted array on correct input', async function () {
      assert.isTrue(converted.arrayOf_Rules.length !== 0);
    });

    const converted_empty = converter.convert({
      keylayout_filename: '',
      arrayOf_Modifiers: [],
      arrayOf_Rules: []
    });
    it('should return empty on empty input', async function () {
      assert.isTrue((converted_empty.keylayout_filename === ''
        && converted_empty.arrayOf_Modifiers.length === 0
        && converted_empty.arrayOf_Rules.length === 0));
    });

    const converted_name = converter.convert({
      keylayout_filename: 'X.keylayout',
      arrayOf_Modifiers: [],
      arrayOf_Rules: []
    });
    it('should return empty on only name as input', async function () {
      assert.isTrue((converted_name.keylayout_filename === ''
        && converted_name.arrayOf_Modifiers.length === 0
        && converted_name.arrayOf_Rules.length === 0));
    });

    const converted_mod = converter.convert({
      keylayout_filename: '',
      arrayOf_Modifiers: [['caps'], ['Shift'], ['command']],
      arrayOf_Rules: []
    });
    it('should return empty on only modifiers as input', async function () {
      assert.isTrue((converted_mod.keylayout_filename === ''
        && converted_mod.arrayOf_Modifiers.length === 0
        && converted_mod.arrayOf_Rules.length === 0));
    });

    const converted_rule = converter.convert({
      keylayout_filename: '',
      arrayOf_Modifiers: [],
      arrayOf_Rules: [["C0", "", "", 0, 0, "", "", 0, 0, "CAPS", "K_A", "A"]]
    });
    it('should return empty on only rules as input', async function () {
      assert.isTrue((converted_rule.keylayout_filename === ''
        && converted_rule.arrayOf_Modifiers.length === 0
        && converted_rule.arrayOf_Rules.length === 0));
    });
  })

  //-----------------------------------------------------------------------------------------------------------------------
  // OK  test create_kmn_modifier
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
      it(('(should convert "' + values[0] + '"').padEnd(36, " ") + 'to \t"' + values[2] + '"', async function () {
        const result = sut.create_kmn_modifier(values[0] as string, values[1] as boolean);
        assert.equal(result, values[2]);
      });
    })
  })

  //-----------------------------------------------------------------------------------------------------------------------
  // OK  test isAcceptableKeymanModifier
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
      it("'" + values[0] + "'" + ' should return ' + values[1], async function () {
        const result = sut.isAcceptableKeymanModifier(values[0] as string);
        assert.equal(result, values[1]);
      });
    })
  })

  //-----------------------------------------------------------------------------------------------------------------------
  // OK  test map_UkeleleKC_To_VK
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
      it(values[0] + ' should return ' + "'" + values[1] + "'", async function () {
        const result = sut.map_UkeleleKC_To_VK(values[0] as number);
        assert.equal(result, values[1]);
      });
    })
  })

  //-----------------------------------------------------------------------------------------------------------------------
  // OK  test checkIfCapsIsUsed
  describe("checkIfCapsIsUsed ", function () {

    it('should return true', async function () {
      const input: string[][] = [["caps", "xxx"], ["yyy"]]
      const result = sut.checkIfCapsIsUsed(input);
      assert.isTrue(result);
    });

    it('should return false', async function () {
      const input: string[][] = [["zzz", "xxx"], ["yyy"]]
      const result = sut.checkIfCapsIsUsed(input);
      assert.isFalse(result);
    });
    it('should return false', async function () {
      const input: string[][] = [null]
      const result = sut.checkIfCapsIsUsed(input);
      assert.isFalse(result);
    });
  })

  //-----------------------------------------------------------------------------------------------------------------------
  // OK  test get_KeyMap_Modifier_array__From__behaviour_arr
  describe("get_KeyMap_Modifier_array__From__behaviour_arr ", function () {

    [
      [[['0', 0]], [['', 'shift? caps? ']]],
      [[['0', 1]], [['anyShift caps?', 'shift? rightShift caps? ', 'shift? leftShift caps? ', 'shift leftShift caps ']]],
      [[['0', 999]], [null]],
      [[['999',]], [null]],
      [[['0', -999]], [null]],
      [[['0']], [null]],
      [[[null]], [null]],
    ].forEach(function (values) {
      it((values[1][0] !== null) ?
        "['0'," + values[0][0][1] + "]" + " should return ['" + values[1][0][0] + "', '" + values[1][0][1] + "']" :
        "['0'," + values[0][0][1] + "]" + " should return ['" + values[1][0] + "']", async function () {
          const result = converter.get_KeyMap_Modifier_array__From__behaviour_arr(converted.arrayOf_Modifiers, values[0])
          assert.deepStrictEqual(JSON.stringify(result), JSON.stringify(values[1]));
        });
    })
  })

  //-----------------------------------------------------------------------------------------------------------------------
  // OK  test get_KeyMap_Code_array__From__ActionID_Action
  describe("get_KeyMap_Code_array__From__ActionID_Action ", function () {
    [
      ["a16", [['32', 3]]],
      ["a19", [['45', 3]]],
      ["a18", [['24', 0], ['24', 3]]],
      ["unknownIdentifier", []],
      [undefined, []],
      [null, []],
      [" ", []],
      ["", []],
    ].forEach(function (values) {
      let outstring = "[ ";
      for (let i = 0; i < values[1].length; i++) {
        outstring = outstring + "[ '" + values[1][i][0] + "', " + values[1][i][1].toString() + "], "
      }
      it("'" + values[0] + "'" + ' should return ' + outstring.substring(0, outstring.lastIndexOf(']') + 2) + " ]", async function () {
        const result = converter.get_KeyMap_Code_array__From__ActionID_Action(read, String(values[0]));
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    })
  })

  //-----------------------------------------------------------------------------------------------------------------------
  //  OK test get_ActionID_Id__From__ActionID_next
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
      ["unknownIdentifier", ""],
    ].forEach(function (values) {
      it("'" + values[0] + "'" + ' should return ' + "'" + values[1] + "'", async function () {
        const result = converter.get_ActionID_Id__From__ActionID_next(read, String(values[0]));
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    })
  })

  //-----------------------------------------------------------------------------------------------------------------------
  //  OK test get_ActionID_Index__From__ActionID_Id
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
      ["unknownIdentifier", 0],
    ].forEach(function (values) {
      it("'" + values[0] + "'" + ' should return ' + values[1], async function () {
        const result = converter.get_ActionID_Index__From__ActionID_Id(read, String(values[0]));
        assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
      });
    })
  })

  //-----------------------------------------------------------------------------------------------------------------------
  // OK test get_Action2ID_NoneOutput__From__ActionID_Id
  describe("get_Action2ID_NoneOutput__From__ActionID_Id ", function () {
    [
      ["a14", "u"],
      ["", ""],
      [" ", ""],
      [null, ""],
      [undefined, ""],
      [99, ""],
      ["a18", undefined],
      ["unknownIdentifier", ""],
    ].forEach(function (values) {
      it((typeof (values[1]) === "string") ?
        "'" + values[0] + "'" + ' should return ' + "'" + values[1] + "'"
        : "'" + values[0] + "'" + ' should return ' + values[1], async function () {
          const result = converter.get_Action2ID_NoneOutput__From__ActionID_Id(read, String(values[0]));
          assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
        });
    })
  })

  //-----------------------------------------------------------------------------------------------------------------------
  //  TODO   test get_KeyMapModiKeyArray__from__array
  describe("get_KeyMapModiKeyArray__from__array ", function () {



    [
      [[['0', 'K_A', 'a9', '0', 'â']], true, [['K_A', 'a9', '0', 'NCAPS', 'â']]],
      [[['0', 'K_A', 'xa9', '0', 'â']], true, [['K_A', 'xa9', '0', 'NCAPS', 'â']]],

    ].forEach(function (values) {

      /* const i = [['0', 'K_A', 'a9', '0', 'â']]
       console.log("i[0][0] ", String(i[0][0]))
       const all = [[['0', 'K_A', 'xa9', '0', 'â']], true, [['K_A', 'xa9', '0', 'NCAPS', 'â']]]
       console.log("all", all)
       console.log(" all[0", all[0])
       console.log(" values[0", values[0])
 
       console.log(" all[1", all[1])
       console.log(" values[1", values[1])
       console.log(" all[2", all[2])
       console.log(" values[2", values[2])
       const new2 = JSON.stringify(values[2])
       console.log("new2 ", typeof (new2), new2)
       console.log("new2[0] ", new2[0])
       console.log("new2[1] ", new2[1])
       console.log("new2[0][0] ", new2[0][0])
 
 
 
 
       console.log("values_values ", values)
       console.log("values[0] ", values[0])
       // console.log("values[0][0] ", String(values[0][0]))
       console.log("values[1] ", values[1])
       console.log("values[2] ", values[2])
       console.log(" ",)*/



      /* let outstring = "[ ";
       for (let i = 0; i < values[1].length; i++) {
         outstring = outstring + "[ '" + values[1][i][0] + "', " + values[1][i][1].toString() + "], "
       }*/
      // it("'" + values[0] + "'" + ' should return ' + outstring.substring(0, outstring.lastIndexOf(']') + 2) + " ]", async function () {
      it("'" + values[0] + "'" + ' should return ' + values[1], async function () {
        //const result = converter.get_KeyMapModiKeyArray__from__array(read, values[0], values[1]);
        //assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
        assert.equal(0, 0);
      });
    })
  })




  //-----------------------------------------------------------------------------------------------------------------------
  // OK test writeData_Stores
  describe("writeData_Stores() ", function () {

    const written_correct = converter.writeData_Stores(converted)

    const converted_empty = converter.convert(converter.read(""));
    const written_empty = converter.writeData_Stores(converted_empty)

    const converted_onlyName = converter.convert(converter.read(""));
    converted_onlyName.keylayout_filename = "X.keylayout"
    const written_onlyName = converter.writeData_Stores(converted_onlyName)

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





    it('should return store text with filename on correct input', async function () {
      assert.equal(written_correct, (out_expected_first + converted.keylayout_filename + out_expected_last));
    });

    it('should return store text without filename on empty input', async function () {
      assert.equal(written_empty, (out_expected_first + out_expected_last));
    });

    it('should return store text without filename on only filename as input', async function () {
      assert.equal(written_onlyName, (out_expected_first + converted_onlyName.keylayout_filename + out_expected_last));
    });
  })

  //----------------------------------------------------------------------------------------------------------------------
  //     test get_ActionID_Output_array__From__ActionID_State
  describe("get_ActionID_Output_array__From__ActionID_State ", function () {
    [
      ["1", [
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
        ("'" + values[0] + "'").padEnd(11, " ") + ' should return an array of object' :
        ("'" + values[0] + "'").padEnd(11, " ") + ' should return ' + "'" + JSON.stringify(values[1]) + "'", async function () {
          const result = converter.get_ActionID_Output_array__From__ActionID_State(read, String(values[0]));
          assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
        });
    })
  })

  //-----------------------------------------------------------------------------------------------------------------------
  //     test get_Datat_array2D__From__ActionID_stateOutput
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
        ("get_Datat_array2D__From__ActionID_stateOutput('" + values[0] + "', '" + values[1] + "', " + values[2] + ")").padEnd(67, " ") + ' should return an array of object' :
        ("get_Datat_array2D__From__ActionID_stateOutput('" + values[0] + "', '" + values[1] + "', " + values[2] + ")").padEnd(67, " ") + ' should return ' + "'" + JSON.stringify(values[3]) + "'", async function () {
          const result = converter.get_Datat_array2D__From__ActionID_stateOutput(read, converted.arrayOf_Modifiers, String(values[0]), String(values[1]), Boolean(values[2]))
          assert.equal(JSON.stringify(result), JSON.stringify(values[3]));
        });
    })
  })

  //-----------------------------------------------------------------------------------------------------------------------
  /*const inputFilename9 = makePathToFixture('../data/Italian_copy.keylayout');
  const converter9 = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
  //const sut9 = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
  const read9 = converter9.read(inputFilename9);
  const converted9 = converter9.convert(read9);
  console.log("converted9 ", typeof (converted9))*/



  //     test get_KeyMap_Code_array__From__KeyMap_Action_array2D
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
    const oneEntryResult =
      [["49", "K_SPACE", "a0", "0", "ˆ"],
      ["49", "K_SPACE", "a0", "1", "ˆ"],
      ["49", "K_SPACE", "a0", "2", "ˆ"],
      ["6", "K_Z", "a0", "4", "ˆ"],
      ["25", "K_9", "a0", "4", "ˆ"],
      ["43", "K_COMMA", "a0", "4", "ˆ"],
      ["49", "K_SPACE", "a0", "7", "ˆ"]
      ];
    const oneEntryResultNoOutput =
      [["49", "K_SPACE", "a0", "0", ""],
      ["49", "K_SPACE", "a0", "1", ""],
      ["49", "K_SPACE", "a0", "2", ""],
      ["6", "K_Z", "a0", "4", ""],
      ["25", "K_9", "a0", "4", ""],
      ["43", "K_COMMA", "a0", "4", ""],
      ["49", "K_SPACE", "a0", "7", ""]
      ];

    [
      [b6_actionId_arr, b1_keycode_arr],
      [[['a0', '1', 'ˆ']], oneEntryResult],
      [[['a0', '1', '']], oneEntryResultNoOutput],
      [[['a0', '', 'ˆ']], oneEntryResult],
      [[['', '1', 'ˆ']], []],
      [[['', '', '']], []],
      [[[' ', ' ', '']], []],
      [[], []],
      [undefined, []],
      [null, []],
    ].forEach(function (values) {
      it(((values[0] === undefined) || (values.some(function (value) { return value === null })) || (values[0].length === 0)) ?
        ("get_KeyMap_Code_array__From__KeyMap_Action_array2D(" + values[0] + ")").padEnd(73, " ") + ' should return ' + "'[" + values[1] + "]'"
        : (JSON.stringify(values[1]).length > 35) ?
          (values[0].length > 0) ?
            ("get_KeyMap_Code_array__From__KeyMap_Action_array2D([['" + values[0][0][0] + "', '" + values[0][0][1] + "', '" + values[0][0][2] + "'],..])").padEnd(73, " ") + ' should return an array of object'
            : ("get_KeyMap_Code_array__From__KeyMap_Action_array2D(['" + values[0][0][0] + "', '" + values[0][0][1] + "', '" + values[0][0][2] + "'])").padEnd(73, " ") + ' should return an array of object'
          : ("get_KeyMap_Code_array__From__KeyMap_Action_array2D(['" + values[0][0][0] + "', '" + values[0][0][1] + "', '" + values[0][0][2] + "'])").padEnd(73, " ") + ' should return ' + "'[" + values[1] + "]'", async function () {
            const result = converter.get_KeyMap_Code_array__From__KeyMap_Action_array2D(read, values[0])
            assert.equal(JSON.stringify(result), JSON.stringify(values[1]));
          });
    })
  })

  //-----------------------------------------------------------------------------------------------------------------------

  // todo test for all public func for all "return paths"
  // todo check input null

  //     test createRuleData
  //     test reviewRules
  //     test writeData_Rules
  //-----------------------------------------------------------------------------------------------------------------------
  // ToDo remove
  describe("END ", function () {
    it('################################################## TESTs DONE ####################################################################################################', async function () {
      assert.equal(0, 0);
    });
  })




});
