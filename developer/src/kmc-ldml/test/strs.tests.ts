import 'mocha';
import { assert } from 'chai';
import { compileKeyboard, compilerTestCallbacks, compilerTestOptions, makePathToFixture } from './helpers/index.js';
import { LdmlCompilerMessages } from '../src/compiler/ldml-compiler-messages.js';


/** strs tests */
describe('strs', function () {
    this.slow(500); // 0.5 sec -- json schema validation takes a while

    before(function () {
        compilerTestCallbacks.clear();
    });

    it('should fail on illegal chars - sections/strs/invalid-illegal.xml', async function () {
        const inputFilename = makePathToFixture('sections/strs/invalid-illegal.xml');
        const kmx = await compileKeyboard(inputFilename, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false },
            [
                // validation messages
                LdmlCompilerMessages.Error_IllegalCharacters({ count: 5, lowestCh: 0xFDD0 }),
                LdmlCompilerMessages.Hint_PUACharacters({ count: 2, lowestCh: 0xE010 }),
            ],
            true, // validation should fail
            [
                // compiler messages (not reached, we've already failed)
            ]);
        assert.isNull(kmx); // should fail post-validate
    });
    it('should hint on pua chars', async function () {
        const inputFilename = makePathToFixture('sections/strs/hint-pua.xml');
        // Compile the keyboard
        const kmx = await compileKeyboard(inputFilename, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false },
            [
                // validation messages
                LdmlCompilerMessages.Hint_PUACharacters({ count: 2, lowestCh: 0xE010 }),
            ],
            false, // validation should pass
            [
                // same messages
                LdmlCompilerMessages.Hint_PUACharacters({ count: 2, lowestCh: 0xE010 }),
            ]);
        assert.isNotNull(kmx);
    });
    it.skip('should warn on unassigned chars', async function () {
        // unassigned not implemented yet
        const inputFilename = makePathToFixture('sections/strs/warn-unassigned.xml');
        const kmx = await compileKeyboard(inputFilename, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false },
            [
                // validation messages
                LdmlCompilerMessages.Hint_PUACharacters({ count: 2, lowestCh: 0xE010 }),
                LdmlCompilerMessages.Warn_UnassignedCharacters({ count: 1, lowestCh: 0x0CFFFD }),
            ],
            false, // validation should pass
            [
                // same messages
                LdmlCompilerMessages.Hint_PUACharacters({ count: 2, lowestCh: 0xE010 }),
                LdmlCompilerMessages.Warn_UnassignedCharacters({ count: 1, lowestCh: 0x0CFFFD }),
            ]);
        assert.isNotNull(kmx);
    });

    it('should fail on extra escapes - sections/tran/fail-bad-tran-2.xml', async function () {
        const inputFilename = makePathToFixture('sections/tran/fail-bad-tran-2.xml');
        const kmx = await compileKeyboard(inputFilename, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false },
            [
                LdmlCompilerMessages.Error_InvalidQuadEscape({ cp: 295 }),
            ],
            true, // validation should fail
            [
                // compiler messages (not reached, we've already failed)
            ]);
        assert.isNull(kmx); // should fail post-validate
    });
    describe('should warn on denorm strings', async function () {
      for (const num of [1, 2, 3, 4]) {
        const path = `sections/strs/warn-denorm-${num}.xml`;
        it(path, async function () {
          const inputFilename = makePathToFixture(path);
          const s = 's\u0307\u0323'; // ṩ
          // Compile the keyboard
          const kmx = await compileKeyboard(inputFilename, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false },
            [
              // validation messages
              LdmlCompilerMessages.Warn_StringDenorm({s}),
            ],
            false, // validation should pass
            [
              // same messages
              LdmlCompilerMessages.Warn_StringDenorm({s}),
            ]);
          assert.isNotNull(kmx); // not failing
        });
      }
    });
});
