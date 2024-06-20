import 'mocha';
import {assert} from 'chai';
import {compileKeyboard, compilerTestCallbacks, compilerTestOptions, makePathToFixture} from './helpers/index.js';
import { CompilerMessages } from '../src/compiler/messages.js';

/** Overall tests that have to do with cross-section linting or hinting */
describe('linter-tests', function() {
    this.slow(500); // 0.5 sec -- json schema validation takes a while

    before(function() {
      compilerTestCallbacks.clear();
    });

    it('should warn on mark keys with no display', async function () {
      // unassigned not implemented yet
      const inputFilename = makePathToFixture('sections/keys/warn-no-keycap.xml');
      const msgs = [
        CompilerMessages.Hint_NoDisplayForMarker({ id: 'mark-dotbelow' }),
        CompilerMessages.Hint_NoDisplayForMarker({ id: 'mark-dotbelowabove' }),
        CompilerMessages.Hint_NoDisplayForSwitch({ id: 'rebase' }),
      ];
      const kmx = await compileKeyboard(inputFilename, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false },
        msgs,
        false, // validation should pass
        msgs,
      );
      assert.isNotNull(kmx);
    });

  });
