import { assert } from 'chai';

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

import { KeyboardHarness, MinimalKeymanGlobal, KeyboardDownloadError, DeviceSpec, InvalidKeyboardError } from 'keyman/engine/keyboard';
import { KeyboardInterface, Mock } from 'keyman/engine/js-processor';
import { NodeKeyboardLoader } from 'keyman/engine/keyboard/node-keyboard-loader';
import { assertThrowsAsync, assertThrows } from 'keyman/tools/testing/test-utils';

describe('Headless keyboard loading', function() {
  const laoPath = require.resolve('@keymanapp/common-test-resources/keyboards/lao_2008_basic.js');
  const nonKeyboardPath = require.resolve('@keymanapp/common-test-resources/index.mjs');
  const ipaPath = require.resolve('@keymanapp/common-test-resources/keyboards/sil_ipa.js');
  const nonExisting = '/does/not/exist.js';
  // Common test suite setup.

  const device = {
    formFactor: DeviceSpec.FormFactor.Desktop,
    OS: DeviceSpec.OperatingSystem.Windows,
    browser: DeviceSpec.Browser.Native,
    touchable: false
  }

  describe('Minimal harness loading', () => {
    it('successfully loads a single keyboard from filesystem', async ()  => {
      // -- START: Standard Recorder-based unit test loading boilerplate --
      const harness = new KeyboardInterface({}, MinimalKeymanGlobal);
      const keyboardLoader = new NodeKeyboardLoader(harness);
      const keyboard = await keyboardLoader.loadKeyboardFromPath(laoPath);
      // --  END:  Standard Recorder-based unit test loading boilerplate --

      // Asserts that the harness's loading field is cleared once the load is complete.
      assert.isNotOk(harness.loadedKeyboard);

      // Asserts that the `activeKeyboard` field was not set by the operation.
      assert.isNotOk(harness.activeKeyboard);

      // This part provides assurance that the keyboard properly loaded.
      assert.equal(keyboard.id, "Keyboard_lao_2008_basic");
    });

    it('throws error when keyboard does not exist', async () => {
      const harness = new KeyboardInterface({}, MinimalKeymanGlobal);
      const keyboardLoader = new NodeKeyboardLoader(harness);

      await assertThrowsAsync(async () => await keyboardLoader.loadKeyboardFromPath(nonExisting),
        KeyboardDownloadError, `Unable to download keyboard at ${nonExisting}`);
    });

    it('throws error when keyboard is invalid', async () => {
      const harness = new KeyboardInterface({}, MinimalKeymanGlobal);
      const keyboardLoader = new NodeKeyboardLoader(harness);

      await assertThrowsAsync(async () => await keyboardLoader.loadKeyboardFromPath(nonKeyboardPath),
        InvalidKeyboardError, `${nonKeyboardPath} is not a valid keyboard file`);
    });

    it('successfully loads (has variable stores)', async () => {
      // -- START: Standard Recorder-based unit test loading boilerplate --
      const harness = new KeyboardInterface({}, MinimalKeymanGlobal);
      const keyboardLoader = new NodeKeyboardLoader(harness);
      const keyboard = await keyboardLoader.loadKeyboardFromPath(ipaPath);
      // --  END:  Standard Recorder-based unit test loading boilerplate --

      // This part provides extra assurance that the keyboard properly loaded.
      assert.equal(keyboard.id, "Keyboard_sil_ipa");
    });

    it('cannot evaluate rules', async function() {
      // -- START: Standard Recorder-based unit test loading boilerplate --
      const harness = new KeyboardHarness({}, MinimalKeymanGlobal);
      const keyboardLoader = new NodeKeyboardLoader(harness);
      const keyboard = await keyboardLoader.loadKeyboardFromPath(laoPath);
      // --  END:  Standard Recorder-based unit test loading boilerplate --

      // Runs a blank KeyEvent through the keyboard's rule processing...
      // but via separate harness configured with a different captured global.
      // This shows an important detail: the 'global' object is effectively
      // closure-captured. (Similar constraints may occur when experimenting with
      // 'sandboxed' keyboard loading in the DOM!)
      const ruleHarness = new KeyboardInterface({}, MinimalKeymanGlobal);
      ruleHarness.activeKeyboard = keyboard;
      assertThrows(() => ruleHarness.processKeystroke(new Mock(), keyboard.constructNullKeyEvent(device)), 'k.KKM is not a function');
    });
  });
});
