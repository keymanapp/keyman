/*
 * Keyman is copyright (C) SIL International. MIT License.
 */
import 'mocha';
import {assert} from 'chai';
import {compilerTestCallbacks, compilerTestOptions} from './helpers/index.js';
import {KeylayoutToKmnConverter} from '../src/keylayout-to-kmn/keylayout-to-kmn-converter.js';

import { makePathToFixture } from './helpers/index.js';       // _S2 my imports

describe('KeylayoutToKmnConverter', function() {

    before(function() {
      compilerTestCallbacks.clear();
    });

    // _S2 first test
    it('should throw on null inputs', async function () {
      // const inputFilename = makePathToFixture('file.keylayout');
      const converter = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);
      // note, could use 'chai as promised' library to make this more fluent:
      let threw = false;
      try {
        await converter.run(null, null, null);
      } catch {
        threw = true;
      }
      assert.isTrue(threw);
    });


    // _S2 My tests...
    it('should throw on all elements loaded', async function () {

      console.log('        ..................................................................................')
      const inputFilename  = makePathToFixture('../data/Mysample.keylayout');
      //const inputFilename  = makePathToFixture('../data/My_dk_Keyboard.keylayout');

      const outputFilename = makePathToFixture('../data/MyResult.kmn');
      const  data = new Uint8Array([1, 2, 3, 4]);       // _S2 just to have sth in an array

      console.log('        inputFilename', inputFilename)
      console.log('        outputFilename', outputFilename)
      console.log('        data', data)

      // _S2 create obj of derived class-> use derived functions
      const converter = new KeylayoutToKmnConverter(compilerTestCallbacks, compilerTestOptions);

      let threw = false;
      try {
        await converter.run(inputFilename, outputFilename, data);
      } catch {
        threw = true;
      }
      assert.isTrue(threw);
    });

  });
