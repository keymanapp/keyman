/*
 * Keyman is copyright (C) SIL International. MIT License.
 */
/*run with 
cd developer/src/kmc-convert/
./build_disp.sh test*/


import 'mocha';
import { assert } from 'chai';
import { compilerTestCallbacks, compilerTestOptions } from './helpers/index.js';
import { KeylayoutToKmnConverter } from '../src/keylayout-to-kmn/keylayout-to-kmn-converter.js';
import { makePathToFixture } from './helpers/index.js';       // _S2 my imports



describe('KeylayoutToKmnConverter', function () {

  before(function () {
    compilerTestCallbacks.clear();
  });

  // _S2 first test
  /*  it('should throw on null inputs', async function () {
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
    });*/


  // _S2 My tests... // later throws on NOT all elements loaded
  it('should throw on all elements loaded', async function () {

    // some keys, no deadkeys
    //const inputFilename  = makePathToFixture('../data/Mysample.keylayout');

    // all keys, some deadkeys
    // copy into C:\Projects\keyman\keyman\developer\src\kmc-convert\test\DATA then they run OK

    //const inputFilename  = makePathToFixture('../data/US_complete.keylayout');          // OK
    //const inputFilename  = makePathToFixture('../data/German_complete.keylayout');      // OK
    const inputFilename = makePathToFixture('../data/Italian_copy.keylayout');         // OK
    //const inputFilename  = makePathToFixture('../data/German_Standard_copy.keylayout');         //OK
    //const inputFilename  = makePathToFixture('../data/German_Standard2.keylayout');         //OK
    //const inputFilename = makePathToFixture('../data/German_StandardTweaked.keylayout');         //NO C3
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
    assert.isTrue(threw);
  });
  //------------------------

  it('should should return empty array on null input', async function () {
    const converter = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const result = converter.read(null);
    assert.isEmpty(result);
  });

  it('should should return empty array on empty input', async function () {
    const converter = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const result = converter.read("");
    assert.isEmpty(result);
  });

  it('should should return empty array on space as input', async function () {
    const converter = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const result = converter.read(" ");
    assert.isEmpty(result);
  });

  it('should should return filled array on correct input', async function () {
    const inputFilename = makePathToFixture('../data/Italian_copy.keylayout');
    const converter = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
    const result = converter.read(inputFilename);
    assert.isNotEmpty(result);
  });


  // todo test read - wrong name, empty string, ok name
  // todo test for all public func for all "return paths"

  //-----------------


  describe("create kmn modifier ", function () {
    [
      ["anycontrol", true, "NCAPS CTRL"],
      ["shift?", true, "NCAPS"],
      ["?", true, "NCAPS"],
      ["?", false, ""],
      ["caps", true, "CAPS"],
      ["", true, "NCAPS"],
      [" ", false, ""],
      ["wrongModifierName", false, "wrongModifierName"],
      ["shift", false, "SHIFT"],
      ["shift command", true, "NCAPS SHIFT command"],
      ["rshift", true, "NCAPS SHIFT"],
      ["rshift", false, "SHIFT"],
      ["rightshift", true, "NCAPS SHIFT"],
      ["riGhtsHift", true, "NCAPS SHIFT"],
      ["LEFTCONTROL", true, "NCAPS LCTRL"],
      ["RCONTROL", true, "NCAPS RCTRL"],
      ["leftoption", true, "NCAPS RALT"],
      ["loption", true, "NCAPS RALT"],
    ].forEach(function (values) {

      it('should convert "' + values[0] + '" to "' + values[2] + '"', async function () {
        const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
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
      ["", true],
      ["LCTRL CAPS", true],
      ["LCTRL X", false],
    ].forEach(function (values) {
      it( "'" + values[0] + "'" +' should return ' + values[1] , async function () {
        const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
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
    ].forEach(function (values) {
      it(  values[0] + ' should return ' + "'"+values[1] +"'", async function () {
        const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
        const result = sut.map_UkeleleKC_To_VK(values[0] as number);
        assert.equal(result, values[1]);
      });
    })
  })

  describe("checkIfCapsIsUsed ", function () {

    it('should return true', async function () {
      const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
      const input: string[][] = [["caps", "xxx"], ["yyy"]]
      const result = sut.checkIfCapsIsUsed(input);
      assert.isTrue(result);
    });

    it('should return false', async function () {
      const sut = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
      const input: string[][] = [["zzz", "xxx"], ["yyy"]]
      const result = sut.checkIfCapsIsUsed(input);
      assert.isFalse(result);
    });
  })

});
