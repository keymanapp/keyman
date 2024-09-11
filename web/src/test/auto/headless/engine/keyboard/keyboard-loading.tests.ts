import { assert } from 'chai';

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

import { KeyboardHarness, MinimalKeymanGlobal, KeyboardDownloadError, DeviceSpec, KeyboardMissingError } from 'keyman/engine/keyboard';
import { KeyboardInterface, Mock } from 'keyman/engine/js-processor';
import { NodeKeyboardLoader } from 'keyman/engine/keyboard/node-keyboard-loader';

// async function assertThrowsAsync(fn: () => Promise<any>, message?: string): Promise<void>;
async function assertThrowsAsync(fn: () => Promise<any>, type?: any, message?: string): Promise<void> {
  if (typeof (type) === 'string') {
    message = type;
    type = undefined;
  }
  try {
    await fn();
    assert.fail('Expected function to throw an error, but it did not.');
  } catch (err) {
    if (type) {
      assert.isTrue(err instanceof type, `Expected error to be of type ${type.name}, but got ${err.constructor.name}`);
    }
    if (message) {
      assert.equal((err as Error).message, message);
    }
  }
}

function assertThrows(fn: () => any, message?: string): void;
function assertThrows(fn: () => any, type?: any, message?: string): void {
  if (typeof (type) === 'string') {
    message = type;
    type = undefined;
  }
  assert.throws(fn, type, message);
}

describe('Headless keyboard loading', function() {
  const laoPath = require.resolve('@keymanapp/common-test-resources/keyboards/lao_2008_basic.js');
  const khmerPath = require.resolve('@keymanapp/common-test-resources/keyboards/khmer_angkor.js');
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
    it('successfully loads', async function() {
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

    it('throws error when keyboard does not exist', async function () {
      const harness = new KeyboardInterface({}, MinimalKeymanGlobal);
      const keyboardLoader = new NodeKeyboardLoader(harness);

      await assertThrowsAsync(() => keyboardLoader.loadKeyboardFromPath(nonExisting),
        KeyboardDownloadError, `Unable to read keyboard file at ${nonExisting}`);
    });

    it('throws error when keyboard is invalid', async function () {
      const harness = new KeyboardInterface({}, MinimalKeymanGlobal);
      const keyboardLoader = new NodeKeyboardLoader(harness);

      await assertThrowsAsync(() => keyboardLoader.loadKeyboardFromPath(nonKeyboardPath),
        KeyboardMissingError, `Cannot find the keyboard at ${nonKeyboardPath}.`);
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
      const ruleHarness = new KeyboardInterface({}, MinimalKeymanGlobal);
      ruleHarness.activeKeyboard = keyboard;
      assertThrows(() => ruleHarness.processKeystroke(new Mock(), keyboard.constructNullKeyEvent(device)), 'k.KKM is not a function');
    });

    it('accurately determines supported gesture types', async () => {
      // -- START: Standard Recorder-based unit test loading boilerplate --
      const harness = new KeyboardHarness({}, MinimalKeymanGlobal);
      const keyboardLoader = new NodeKeyboardLoader(harness);
      const km_keyboard = await keyboardLoader.loadKeyboardFromPath(khmerPath);
      // --  END:  Standard Recorder-based unit test loading boilerplate --

      // `khmer_angkor` - supports longpresses, but not flicks or multitaps.

      const desktopLayout = km_keyboard.layout(DeviceSpec.FormFactor.Desktop);
      assert.isFalse(desktopLayout.hasFlicks);
      assert.isFalse(desktopLayout.hasLongpresses);
      assert.isFalse(desktopLayout.hasMultitaps);

      const mobileLayout = km_keyboard.layout(DeviceSpec.FormFactor.Phone);
      assert.isFalse(mobileLayout.hasFlicks);
      assert.isTrue(mobileLayout.hasLongpresses);
      assert.isFalse(mobileLayout.hasMultitaps);
    });
  });
});
