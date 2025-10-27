/*
 * Keyman is copyright (C) SIL International. MIT License.
 */
import 'mocha';
import {assert} from 'chai';
import {compilerTestCallbacks, compilerTestOptions} from './helpers/index.js';
import {KeylayoutToKmnConverter} from '../src/keylayout-to-kmn/keylayout-to-kmn-converter.js';

describe('KeylayoutToKmnConverter', function() {

    before(function() {
      compilerTestCallbacks.clear();
    });

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

  });
