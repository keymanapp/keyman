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
    const inputFilename  = makePathToFixture('../data/Italian_copy.keylayout');         // OK
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

});
