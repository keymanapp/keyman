import { assert } from 'chai';

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

import { MinimalKeymanGlobal, Mock } from 'keyman/engine/keyboard';
import { KeyboardInterface } from 'keyman/engine/js-processor';
import { NodeKeyboardLoader } from 'keyman/engine/keyboard/node-keyboard-loader';

describe('Headless keyboard loading', function () {
  const laoPath = require.resolve('@keymanapp/common-test-resources/keyboards/lao_2008_basic.js');
  const khmerPath = require.resolve('@keymanapp/common-test-resources/keyboards/khmer_angkor.js');
  const nonKeyboardPath = require.resolve('@keymanapp/common-test-resources/index.mjs');
  const ipaPath = require.resolve('@keymanapp/common-test-resources/keyboards/sil_ipa.js');
  // Common test suite setup.

  let device = {
    formFactor: 'desktop',
    OS: 'windows',
    browser: 'native'
  }

  describe('Full harness loading', () => {
    it('successfully loads', async function () {
      // -- START: Standard Recorder-based unit test loading boilerplate --
      let harness = new KeyboardInterface({}, MinimalKeymanGlobal);
      let keyboardLoader = new NodeKeyboardLoader(harness);
      let keyboard = await keyboardLoader.loadKeyboardFromPath(laoPath);
      harness.activeKeyboard = keyboard;
      // --  END:  Standard Recorder-based unit test loading boilerplate --

      // This part provides assurance that the keyboard properly loaded.
      assert.equal(keyboard.id, "Keyboard_lao_2008_basic");
    });

    it('can evaluate rules', async function () {
      // -- START: Standard Recorder-based unit test loading boilerplate --
      let harness = new KeyboardInterface({}, MinimalKeymanGlobal);
      let keyboardLoader = new NodeKeyboardLoader(harness);
      let keyboard = await keyboardLoader.loadKeyboardFromPath(laoPath);
      harness.activeKeyboard = keyboard;
      // --  END:  Standard Recorder-based unit test loading boilerplate --

      // Runs a blank KeyEvent through the keyboard's rule processing.
      harness.processKeystroke(new Mock(), keyboard.constructNullKeyEvent(device));
    });

    it('does not change the active kehboard', async function () {
      let harness = new KeyboardInterface({}, MinimalKeymanGlobal);
      let keyboardLoader = new NodeKeyboardLoader(harness);
      const lao_keyboard = await keyboardLoader.loadKeyboardFromPath(laoPath);
      assert.isNotOk(harness.activeKeyboard);
      assert.isOk(lao_keyboard);

      // This part provides assurance that the keyboard properly loaded.
      assert.equal(lao_keyboard.id, "Keyboard_lao_2008_basic");

      harness.activeKeyboard = lao_keyboard;

      const khmer_keyboard = await keyboardLoader.loadKeyboardFromPath(khmerPath);
      assert.strictEqual(lao_keyboard, harness.activeKeyboard);

      assert.equal(khmer_keyboard.id, "Keyboard_khmer_angkor");
    });

    it('throws distinct errors', async function () {
      const invalidPath = 'totally_invalid_path.js';

      let harness = new KeyboardInterface({}, MinimalKeymanGlobal);
      let keyboardLoader = new NodeKeyboardLoader(harness);
      let missingError;
      try {
        await keyboardLoader.loadKeyboardFromPath(invalidPath);
      } catch (err) {
        missingError = err;
      }
      assert.isOk(missingError);

      let scriptLoadError;
      try {
        await keyboardLoader.loadKeyboardFromPath(nonKeyboardPath);
      } catch (err) {
        scriptLoadError = err;
      }
      assert.isOk(scriptLoadError);

      // The main test:  do the errors match?
      assert.notEqual(scriptLoadError.message, missingError.message);
    });
  })
});
