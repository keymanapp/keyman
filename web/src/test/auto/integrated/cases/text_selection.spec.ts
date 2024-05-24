import { assert } from 'chai';

import {
  DEVICE_DETECT_FAILURE,
  loadKeyboardFromJSON,
  setupKMW,
  teardownKMW
} from "../test_utils.js";
import * as KMWRecorder from '#recorder';
import { timedPromise } from '@keymanapp/web-utils';
import { type KeymanEngine } from 'keyman/app/browser';

const baseTimeout = 5000;
const attachmentTimeout = 500;
const host = document.createElement('div');
document.body.appendChild(host);

describe('Text Selection', function() {
  this.timeout(baseTimeout);

  /* Utility functions */

  function setupElement(ele) {
    const keyman: KeymanEngine = window['keyman'];
    keyman.setActiveElement(ele);
    return ele;
  }

  function assertInputSteps(ele, count, setup) {
    return new Promise<void>((resolve, reject) => {
      var i = 0;
      var listener = function() {
        i++;
        if(i == count) {
          ele.removeEventListener("input", listener);
          resolve();
        }
      };
      ele.addEventListener("input", listener);
      setup();
    });
  }

  function instantiateBrowserDriver(ele) {
    return new KMWRecorder.BrowserDriver(setupElement(ele));
  }

  /* Define key event specs */

  let keys: {[id: string]: KMWRecorder.PhysicalInputEventSpec} = {};
  for (var i = 0; i < 26; i++) {
    var simple = {"type":"key","key":String.fromCharCode(i+97),"code":"Key"+String.fromCharCode(i+65),"keyCode":i+65,"modifierSet":0,"location":0};
    var key = new KMWRecorder.PhysicalInputEventSpec(simple);
    keys[String.fromCharCode(i+65)] = key;
  }
  keys.Backspace = new KMWRecorder.PhysicalInputEventSpec({"type":"key","key":"Backspace","code":"Backspace","keyCode":8,"modifierSet":0,"location":0});

  /* Test Setup */

  // TODO: Add automated tests for editable DIV, designMode iframe
  for(var inputType of ['Input', 'TextArea']) {
    describe('Text Selection in '+inputType, function() {
      before(function() {
        // These tests require use of KMW's device-detection functionality.
        assert.isFalse(DEVICE_DETECT_FAILURE, "Cannot run due to device detection failure.");

        return setupKMW(null, baseTimeout).then(() => {
          return loadKeyboardFromJSON("resources/json/keyboards/web_context_tests.json", baseTimeout).then(() => {
            const keyman: KeymanEngine = window['keyman'];
            return keyman.setActiveKeyboard("web_context_tests");
          });
        });
      });

      beforeEach(async function() {
        const ele = document.createElement(inputType);
        ele.id = 'singleton';
        host.appendChild(ele);

        await timedPromise(attachmentTimeout);
      });

      after(function() {
        const keyman: KeymanEngine = window['keyman'];
        keyman?.removeKeyboards('web_context_tests');
        teardownKMW();
      });

      afterEach(async function() {
        host.innerHTML = '';

        Promise.resolve();
      });

      /*
        --- Tests ---

        A-E: BMP tests
        F-J: SMP tests
        K-O: Mixed BMP+SMP tests (O = SMP)
        Q-Z: Unused
      */

      /**
       * TEST_CONTEXT_BASE
       *
       * Using the web_context_tests keyboard, type abcd. The output after the final key should be '!'.
       */
      it('Should do a basic transform without selection involved', async () => {
        var ele = document.getElementById("singleton") as HTMLInputElement | HTMLTextAreaElement;
        var eventDriver = instantiateBrowserDriver(ele);

        await assertInputSteps(ele, 5, () => {
          eventDriver.simulateEvent(keys.E);
          eventDriver.simulateEvent(keys.A);
          eventDriver.simulateEvent(keys.B);
          eventDriver.simulateEvent(keys.C);
          eventDriver.simulateEvent(keys.D);
        });

        assert.strictEqual(ele.value, 'e!', "Expected keys eabcd to transform to e!");
        assert.deepEqual([ele.selectionStart, ele.selectionEnd], [2,2], "Expected caret to be at end of text");
      });

      it('Should do a basic transform without selection involved - SMP', async () => {
        var ele = document.getElementById("singleton") as HTMLInputElement | HTMLTextAreaElement;
        var eventDriver = instantiateBrowserDriver(ele);

        await assertInputSteps(ele, 5, () => {
          eventDriver.simulateEvent(keys.J);
          eventDriver.simulateEvent(keys.F);
          eventDriver.simulateEvent(keys.G);
          eventDriver.simulateEvent(keys.H);
          eventDriver.simulateEvent(keys.I);
        });

        assert.strictEqual(ele.value, '𐌈𐌙', "Expected keys eabcd to transform to 𐌈𐌙");
        assert.deepEqual([ele.selectionStart, ele.selectionEnd], [4,4], "Expected caret to be at end of text");
      });

      // `as const` allows TS to lock in on the specific strings, rather than just saying 'string' as the type.
      for(let direction of ['forward', 'backward'] as const) {

        /**
         * TEST_SELECTION
         *
         * Using the web_context_tests keyboard, type abcx. Select bc and type q.
         * The output after the final key should be aqx.
         */
        it('Should do a basic selection replacement, in '+direction+' direction', async () => {
          var ele = document.getElementById("singleton") as HTMLInputElement | HTMLTextAreaElement;
          var eventDriver = instantiateBrowserDriver(ele);

          // Step 1: 'abcx'

          await assertInputSteps(ele, 4, () => {
            eventDriver.simulateEvent(keys.A);
            eventDriver.simulateEvent(keys.B);
            eventDriver.simulateEvent(keys.C);
            eventDriver.simulateEvent(keys.X);
          });

          assert.strictEqual(ele.value, 'abcx', "Sanity check: expected text to be 'abcx'");

          // Step 2, select 'bc', replace with 'q'

          ele.selectionStart = 1;
          ele.selectionEnd = 3;
          ele.selectionDirection = direction;
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [1, 3], "Sanity check: expected selection to be 'bc'");

          await assertInputSteps(ele, 1, () => {
            eventDriver.simulateEvent(keys.Q);
          });

          assert.strictEqual(ele.value, 'aqx', "Expected selection to have been replaced with 'q'");
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [2, 2], "Expected caret to be after 'q'");
        });

        it('Should do a basic selection replacement, with a matching rule, in '+direction+' direction', async () => {
          var ele = document.getElementById("singleton") as HTMLInputElement | HTMLTextAreaElement;
          var eventDriver = instantiateBrowserDriver(ele);

          // Step 1: 'abcx'

          await assertInputSteps(ele, 4, () => {
            eventDriver.simulateEvent(keys.A);
            eventDriver.simulateEvent(keys.B);
            eventDriver.simulateEvent(keys.C);
            eventDriver.simulateEvent(keys.X);
          });

          assert.strictEqual(ele.value, 'abcx', "Sanity check: expected text to be 'abcx'");

          // Step 2, select 'bc', replace with [p] => 'q'

          ele.selectionStart = 1;
          ele.selectionEnd = 3;
          ele.selectionDirection = direction;
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [1, 3], "Sanity check: expected selection to be 'bc'");

          await assertInputSteps(ele, 1, () => {
            eventDriver.simulateEvent(keys.P);
          });

          assert.strictEqual(ele.value, 'aqx', "Expected selection to have been replaced with 'q'");
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [2, 2], "Expected caret to be after 'q'");
        });

        it('Should do a basic selection replacement, in '+direction+' direction - SMP', async () => {
          var ele = document.getElementById("singleton") as HTMLInputElement | HTMLTextAreaElement;
          var eventDriver = instantiateBrowserDriver(ele);

          // Step 1: 'fghj'

          await assertInputSteps(ele, 4, () => {
            eventDriver.simulateEvent(keys.F);
            eventDriver.simulateEvent(keys.G);
            eventDriver.simulateEvent(keys.H);
            eventDriver.simulateEvent(keys.J);
          });

          assert.strictEqual(ele.value, '𐌀𐌁𐌂𐌈', "Sanity check: expected text to be '𐌀𐌁𐌂𐌈'");

          // Step 2, select '𐌁𐌂', replace with '𐍈'

          ele.selectionStart = 2;
          ele.selectionEnd = 6;
          ele.selectionDirection = direction;
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [2, 6], "Sanity check: expected selection to be '𐌁𐌂'");

          await assertInputSteps(ele, 1, () => {
            eventDriver.simulateEvent(keys.O);
          });

          assert.strictEqual(ele.value, '𐌀𐍈𐌈', "Expected selection to have been replaced with '𐍈'");
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [4, 4], "Expected caret to be after '𐍈'");
        });

        /**
         * TEST_CONTEXT_SELECTION
         *
         * Using the attached web_context_tests keyboard, type abcx. Select the x character, and type d.
         * The output after the final key should be 'abcd'.
         */
        it('Should ignore context when a selection, in '+direction+' direction, is made', async () => {
          var ele = document.getElementById("singleton") as HTMLInputElement | HTMLTextAreaElement;
          var eventDriver = instantiateBrowserDriver(ele);

          // Step 1: 'abcx'

          await assertInputSteps(ele, 4, () => {
            eventDriver.simulateEvent(keys.A);
            eventDriver.simulateEvent(keys.B);
            eventDriver.simulateEvent(keys.C);
            eventDriver.simulateEvent(keys.X);
          });

          assert.strictEqual(ele.value, 'abcx', "Sanity check: expected text to be 'abcx'");

          // Step 2, select 'x', replace with 'd'

          ele.selectionStart = 3;
          ele.selectionEnd = 4;
          ele.selectionDirection = direction;
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [3, 4], "Sanity check: expected selection to be 'x'");

          await assertInputSteps(ele, 1, () => {
            eventDriver.simulateEvent(keys.D);
          });

          assert.strictEqual(ele.value, 'abcd', "Expected selection to have been replaced with 'd'");
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [4, 4], "Expected caret to be after 'd'");
        });

        it('Should ignore context when a selection, in '+direction+' direction, is made - SMP', async () => {
          var ele = document.getElementById("singleton") as HTMLInputElement | HTMLTextAreaElement;
          var eventDriver = instantiateBrowserDriver(ele);

          // Step 1: 'fghj'

          await assertInputSteps(ele, 4, () => {
            eventDriver.simulateEvent(keys.F);
            eventDriver.simulateEvent(keys.G);
            eventDriver.simulateEvent(keys.H);
            eventDriver.simulateEvent(keys.J);
          });

          assert.strictEqual(ele.value, '𐌀𐌁𐌂𐌈', "Sanity check: expected text to be '𐌀𐌁𐌂𐌈'");

          // Step 2, select '𐌈', replace with [i]

          ele.selectionStart = 6;
          ele.selectionEnd = 8;
          ele.selectionDirection = direction;
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [6, 8], "Sanity check: expected selection to be '𐌈'");

          await assertInputSteps(ele, 1, () => {
            eventDriver.simulateEvent(keys.I);
          });

          assert.strictEqual(ele.value, '𐌀𐌁𐌂i', "Expected selection to have been replaced with 'i'");
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [7, 7], "Expected caret to be after 'i'");
        });

        /**
         * TEST_CONTEXT_SELECTION_2
         *
         * Using the attached web_context_tests keyboard, type abcx. Select the x character, and
         * delete it with Backspace. Type d. The output after the final key should be !.
         */
        it('Should correctly delete selection, in '+direction+' direction, with backspace and not lose sync', async () => {
          var ele = document.getElementById("singleton") as HTMLInputElement | HTMLTextAreaElement;
          var eventDriver = instantiateBrowserDriver(ele);

          // Step 1: 'abcx'

          await assertInputSteps(ele, 4, () => {
            eventDriver.simulateEvent(keys.A);
            eventDriver.simulateEvent(keys.B);
            eventDriver.simulateEvent(keys.C);
            eventDriver.simulateEvent(keys.X);
          });

          assert.strictEqual(ele.value, 'abcx', "Sanity check: expected text to be 'abcx'");

          // Step 2, select 'x', press Backspace

          ele.selectionStart = 3;
          ele.selectionEnd = 4;
          ele.selectionDirection = direction;
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [3, 4], "Sanity check: expected selection to be 'x'");

          await assertInputSteps(ele, 1, () => {
            eventDriver.simulateEvent(keys.Backspace);
          });


          assert.strictEqual(ele.value, 'abc', "Expected selection to have been deleted");
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [3, 3], "Expected caret to be after 'c'");

          // Step 3, type [d]

          await assertInputSteps(ele, 1, () => {
            eventDriver.simulateEvent(keys.D);
          });

          assert.strictEqual(ele.value, '!', "Expected text to have been transformed to '!'");
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [1, 1], "Expected caret to be after '!'");
        });

        it('Should correctly delete selection, in '+direction+' direction, with backspace and not lose sync - SMP', async () => {
          var ele = document.getElementById("singleton") as HTMLInputElement | HTMLTextAreaElement;
          var eventDriver = instantiateBrowserDriver(ele);

          // Step 1: 'abcx'

          await assertInputSteps(ele, 4, () => {
            eventDriver.simulateEvent(keys.F);
            eventDriver.simulateEvent(keys.G);
            eventDriver.simulateEvent(keys.H);
            eventDriver.simulateEvent(keys.J);
          });

          assert.strictEqual(ele.value, '𐌀𐌁𐌂𐌈', "Sanity check: expected text to be '𐌀𐌁𐌂𐌈'");

          // Step 2, select '𐌈', press Backspace

          ele.selectionStart = 6;
          ele.selectionEnd = 8;
          ele.selectionDirection = direction;
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [6, 8], "Sanity check: expected selection to be '𐌈'");

          await assertInputSteps(ele, 1, () => {
            eventDriver.simulateEvent(keys.Backspace);
          });


          assert.strictEqual(ele.value, '𐌀𐌁𐌂', "Expected selection to have been deleted");
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [6, 6], "Expected caret to be after '𐌂'");

          // Step 3, type [i]

          await assertInputSteps(ele, 1, () => {
            eventDriver.simulateEvent(keys.I);
          });

          assert.strictEqual(ele.value, '𐌙', "Expected text to have been transformed to '𐌙'");
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [2, 2], "Expected caret to be after '𐌙'");
        });

        /**
         * TEST_CONTEXT_SELECTION_3
         *
         * Using the attached web_context_tests keyboard, type abcx. Select the x character, and type y.
         * Press Backspace, and type d. The output after the final key should be !.
         */
        it('Should correctly replace selection, in '+direction+' direction, and not lose sync', async () => {
          var ele = document.getElementById("singleton") as HTMLInputElement | HTMLTextAreaElement;
          var eventDriver = instantiateBrowserDriver(ele);

          // Step 1: 'abcx'

          await assertInputSteps(ele, 4, () => {
            eventDriver.simulateEvent(keys.A);
            eventDriver.simulateEvent(keys.B);
            eventDriver.simulateEvent(keys.C);
            eventDriver.simulateEvent(keys.X);
          });

          assert.strictEqual(ele.value, 'abcx', "Sanity check: expected text to be 'abcx'");

          // Step 2, select 'x', press y

          ele.selectionStart = 3;
          ele.selectionEnd = 4;
          ele.selectionDirection = direction;
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [3, 4], "Sanity check: expected selection to be 'x'");

          await assertInputSteps(ele, 1, () => {
            eventDriver.simulateEvent(keys.Y);
          });

          assert.strictEqual(ele.value, 'abcy', "Expected selection to have been replaced with y");
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [4, 4], "Expected caret to be after 'y'");

          // Step 3, press Backspace

          await assertInputSteps(ele, 1, () => {
            eventDriver.simulateEvent(keys.Backspace);
          });

          assert.strictEqual(ele.value, 'abc', "Expected y to have been deleted");
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [3, 3], "Expected caret to be after 'c'");

          // Step 4, type 'd'

          await assertInputSteps(ele, 1, () => {
            eventDriver.simulateEvent(keys.D);
          });

          assert.strictEqual(ele.value, '!', "Expected text to have been transformed to '!'");
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [1, 1], "Expected caret to be after '!'");
        });

        it('Should correctly replace selection, in '+direction+' direction, and not lose sync - SMP', async () => {
          var ele = document.getElementById("singleton") as HTMLInputElement | HTMLTextAreaElement;
          var eventDriver = instantiateBrowserDriver(ele);

          // Step 1: 'abcx'

          await assertInputSteps(ele, 4, () => {
            eventDriver.simulateEvent(keys.F);
            eventDriver.simulateEvent(keys.G);
            eventDriver.simulateEvent(keys.H);
            eventDriver.simulateEvent(keys.J);
          });

          assert.strictEqual(ele.value, '𐌀𐌁𐌂𐌈', "Sanity check: expected text to be '𐌀𐌁𐌂𐌈'");

          // Step 2, select '𐌈', press [k]

          ele.selectionStart = 6;
          ele.selectionEnd = 8;
          ele.selectionDirection = direction;
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [6, 8], "Sanity check: expected selection to be '𐌈'");

          await assertInputSteps(ele, 1, () => {
            eventDriver.simulateEvent(keys.K);
          });

          assert.strictEqual(ele.value, '𐌀𐌁𐌂𐌰', "Expected selection to have been replaced with 𐌰");
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [8, 8], "Expected caret to be after '𐌰'");

          // Step 3, press Backspace

          await assertInputSteps(ele, 1, () => {
            eventDriver.simulateEvent(keys.Backspace);
          });

          assert.strictEqual(ele.value, '𐌀𐌁𐌂', "Expected 𐌰 to have been deleted");
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [6, 6], "Expected caret to be after '𐌂'");

          // Step 4, type [i]

          await assertInputSteps(ele, 1, () => {
            eventDriver.simulateEvent(keys.I);
          });

          assert.strictEqual(ele.value, '𐌙', "Expected text to have been transformed to '𐌙'");
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [2, 2], "Expected caret to be after '𐌙'");
        });

        /**
         * TEST_CONTEXT_SELECTION_4
         *
         * Using the attached web_context_tests keyboard, type xabcx.
         * Select the abc characters, and type d. The output after the final key should be xdx.
         */
        it('Should not treat the selection, in '+direction+' direction, as context', async () => {
          var ele = document.getElementById("singleton") as HTMLInputElement | HTMLTextAreaElement;
          var eventDriver = instantiateBrowserDriver(ele);

          // Step 1: 'xabcx'

          await assertInputSteps(ele, 5, () => {
            eventDriver.simulateEvent(keys.X);
            eventDriver.simulateEvent(keys.A);
            eventDriver.simulateEvent(keys.B);
            eventDriver.simulateEvent(keys.C);
            eventDriver.simulateEvent(keys.X);
          });

          assert.strictEqual(ele.value, 'xabcx', "Sanity check: expected text to be 'xabcx'");

          // Step 2, select 'abc', press 'd'

          ele.selectionStart = 1;
          ele.selectionEnd = 4;
          ele.selectionDirection = direction;
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [1, 4], "Sanity check: expected selection to be 'abc'");

          await assertInputSteps(ele, 1, () => {
            eventDriver.simulateEvent(keys.D);
          });

          assert.strictEqual(ele.value, 'xdx', "Expected selection to have been replaced with d");
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [2, 2], "Expected caret to be after 'd'");
        });

        it('Should not treat the selection, in '+direction+' direction, as context - SMP', async () => {
          var ele = document.getElementById("singleton") as HTMLInputElement | HTMLTextAreaElement;
          var eventDriver = instantiateBrowserDriver(ele);

          // Step 1: [k][f][g][h][k]

          await assertInputSteps(ele, 5, () => {
            eventDriver.simulateEvent(keys.K);
            eventDriver.simulateEvent(keys.F);
            eventDriver.simulateEvent(keys.G);
            eventDriver.simulateEvent(keys.H);
            eventDriver.simulateEvent(keys.K);
          });

          assert.strictEqual(ele.value, '𐌰𐌀𐌁𐌂𐌰', "Sanity check: expected text to be '𐌰𐌀𐌁𐌂𐌰'");

          // Step 2, select '𐌀𐌁𐌂', press [i]

          ele.selectionStart = 2;
          ele.selectionEnd = 8;
          ele.selectionDirection = direction;
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [2, 8], "Sanity check: expected selection to be '𐌀𐌁𐌂'");

          await assertInputSteps(ele, 1, () => {
            eventDriver.simulateEvent(keys.I);
          });

          assert.strictEqual(ele.value, '𐌰i𐌰', "Expected selection to have been replaced with i");
          assert.deepEqual([ele.selectionStart, ele.selectionEnd], [3, 3], "Expected caret to be after 'i'");
        });
      }
    });
  }
});
