import { assert } from 'chai';

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

import { KeyboardHarness, MinimalKeymanGlobal, Mock } from 'keyman/engine/keyboard';
import { KeyboardInterface } from 'keyman/engine/js-processor';
import { NodeKeyboardLoader } from 'keyman/engine/keyboard/node-keyboard-loader';

describe('Headless keyboard loading', function() {
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

  describe('Minimal harness loading', () => {
    it('successfully loads', async function() {
      // -- START: Standard Recorder-based unit test loading boilerplate --
      let harness = new KeyboardHarness({}, MinimalKeymanGlobal);
      let keyboardLoader = new NodeKeyboardLoader(harness);
      let keyboard = await keyboardLoader.loadKeyboardFromPath(laoPath);
      // --  END:  Standard Recorder-based unit test loading boilerplate --

      // Asserts that the harness's loading field is cleared once the load is complete.
      assert.isNotOk(harness.loadedKeyboard);

      // Asserts that the `activeKeyboard` field was not set by the operation.
      assert.isNotOk(harness.activeKeyboard);

      // This part provides assurance that the keyboard properly loaded.
      assert.equal(keyboard.id, "Keyboard_lao_2008_basic");
    });

    it('successfully loads (has variable stores)', async () => {
      // -- START: Standard Recorder-based unit test loading boilerplate --
      let harness = new KeyboardHarness({}, MinimalKeymanGlobal);
      let keyboardLoader = new NodeKeyboardLoader(harness);
      let keyboard = await keyboardLoader.loadKeyboardFromPath(ipaPath);
      // --  END:  Standard Recorder-based unit test loading boilerplate --

      // This part provides extra assurance that the keyboard properly loaded.
      assert.equal(keyboard.id, "Keyboard_sil_ipa");
    });

    it('cannot evaluate rules', async function() {
      // -- START: Standard Recorder-based unit test loading boilerplate --
      let harness = new KeyboardHarness({}, MinimalKeymanGlobal);
      let keyboardLoader = new NodeKeyboardLoader(harness);
      let keyboard = await keyboardLoader.loadKeyboardFromPath(laoPath);
      // --  END:  Standard Recorder-based unit test loading boilerplate --

      // Runs a blank KeyEvent through the keyboard's rule processing...
      // but via separate harness configured with a different captured global.
      let ruleHarness = new KeyboardInterface({}, MinimalKeymanGlobal);
      ruleHarness.activeKeyboard = keyboard;
      try {
        ruleHarness.processKeystroke(new Mock(), keyboard.constructNullKeyEvent(device));
        assert.fail();
      } catch (err) {
        // Drives home an important detail: the 'global' object is effectively
        // closure-captured.  (Similar constraints may occur when experimenting with
        // 'sandboxed' keyboard loading in the DOM!)
        assert.equal(err.message, 'k.KKM is not a function');
      }
    });

    it('accurately determines supported gesture types', async () => {
      // -- START: Standard Recorder-based unit test loading boilerplate --
      let harness = new KeyboardHarness({}, MinimalKeymanGlobal);
      let keyboardLoader = new NodeKeyboardLoader(harness);
      let km_keyboard = await keyboardLoader.loadKeyboardFromPath(khmerPath);
      // --  END:  Standard Recorder-based unit test loading boilerplate --

      // `khmer_angkor` - supports longpresses, but not flicks or multitaps.

      const desktopLayout = km_keyboard.layout('desktop');
      assert.isFalse(desktopLayout.hasFlicks);
      assert.isFalse(desktopLayout.hasLongpresses);
      assert.isFalse(desktopLayout.hasMultitaps);

      const mobileLayout = km_keyboard.layout('phone');
      assert.isFalse(mobileLayout.hasFlicks);
      assert.isTrue(mobileLayout.hasLongpresses);
      assert.isFalse(mobileLayout.hasMultitaps);
    });
  });
});
