import { assert } from 'chai';

import { DOMKeyboardLoader } from 'keyman/engine/keyboard/dom-keyboard-loader';
import { extendString, KeyboardHarness, JSKeyboard, MinimalKeymanGlobal, DeviceSpec, KeyboardKeymanGlobal, KeyboardDownloadError, KeyboardScriptError, Keyboard } from 'keyman/engine/keyboard';
import { JSKeyboardInterface, Mock } from 'keyman/engine/js-processor';
import { assertThrowsAsync } from 'keyman/tools/testing/test-utils';

declare let window: typeof globalThis;
// KeymanEngine from the web/ folder... when available.
// At this level, though... we just mock it.
declare let keyman: KeyboardKeymanGlobal;
declare let KeymanWeb: JSKeyboardInterface;

// Note:  rule processing tests will fail if string extensions are not established beforehand.
extendString();

const device: DeviceSpec = {
  touchable: false,
  formFactor: DeviceSpec.FormFactor.Phone,
  OS: DeviceSpec.OperatingSystem.iOS,
  browser: DeviceSpec.Browser.Safari
}

describe('Keyboard loading in DOM', function() {
  afterEach(() => {
    if (KeymanWeb) {
      KeymanWeb.uninstall();
    }
  })

  it('throws error when keyboard does not exist', async () => {
    const harness = new JSKeyboardInterface(window, MinimalKeymanGlobal);
    const keyboardLoader = new DOMKeyboardLoader(harness);
    const nonExisting = '/does/not/exist.js';

    await assertThrowsAsync(async () => await keyboardLoader.loadKeyboardFromPath(nonExisting),
      KeyboardDownloadError, `Unable to download keyboard at ${nonExisting}`);
  });

  it('throws error when keyboard is invalid', async () => {
    const harness = new JSKeyboardInterface(window, MinimalKeymanGlobal);
    const keyboardLoader = new DOMKeyboardLoader(harness);
    const nonKeyboardPath = '/common/test/resources/index.mjs';

    await assertThrowsAsync(async () => await keyboardLoader.loadKeyboardFromPath(nonKeyboardPath),
      KeyboardScriptError, `Error registering the keyboard script at ${nonKeyboardPath}; it may contain an error.`);
  });

  it('`window`, disabled rule processing', async () => {
    const harness = new KeyboardHarness(window, MinimalKeymanGlobal);
    let keyboardLoader = new DOMKeyboardLoader(harness);
    let keyboard: Keyboard = await keyboardLoader.loadKeyboardFromPath('/common/test/resources/keyboards/khmer_angkor.js');

    assert.isOk(keyboard);
    assert.instanceOf(keyboard, JSKeyboard);
    const jsKeyboard = keyboard as JSKeyboard;
    assert.equal(jsKeyboard.id, 'Keyboard_khmer_angkor');
    assert.isTrue(jsKeyboard.isChiral);
    assert.isFalse(jsKeyboard.isCJK);
    assert.isOk(KeymanWeb);
    assert.isOk(keyman);
    assert.isOk(keyman.osk);
    assert.isOk(keyman.osk.keyCodes);

    // Should be cleared post-keyboard-load.
    assert.isNotOk(harness.loadedKeyboard);
  });

  it('`window`, enabled rule processing', async () => {
    const jsHarness = new JSKeyboardInterface(window, MinimalKeymanGlobal);
    const keyboardLoader = new DOMKeyboardLoader(jsHarness);
    const keyboard: Keyboard = await keyboardLoader.loadKeyboardFromPath('/common/test/resources/keyboards/khmer_angkor.js');
    const jsKeyboard = keyboard as JSKeyboard;
    jsHarness.activeKeyboard = jsKeyboard;

    assert.isOk(keyboard);
    assert.instanceOf(keyboard, JSKeyboard);
    assert.equal(jsKeyboard.id, 'Keyboard_khmer_angkor');
    assert.isTrue(jsKeyboard.isChiral);
    assert.isFalse(jsKeyboard.isCJK);
    assert.isOk(KeymanWeb);
    assert.isOk(keyman);
    assert.isOk(keyman.osk);
    assert.isOk(keyman.osk.keyCodes);

    // TODO:  verify actual rule processing.
    const nullKeyEvent = jsKeyboard.constructNullKeyEvent(device);
    const mock = new Mock();
    const result = jsHarness.processKeystroke(mock, nullKeyEvent);

    assert.isOk(result);
    assert.isOk(KeymanWeb);
    assert.isOk(keyman);
    assert.isOk(keyman.osk);
    assert.isOk(keyman.osk.keyCodes);

    // Should be cleared post-keyboard-load.
    assert.isNotOk(jsHarness.loadedKeyboard);
  });

  it('load keyboards successfully in parallel without side effects', async () => {
    let jsHarness = new JSKeyboardInterface(window, MinimalKeymanGlobal);
    let keyboardLoader = new DOMKeyboardLoader(jsHarness);

    // Preload a keyboard and make it active.
    const test_kbd: Keyboard = await keyboardLoader.loadKeyboardFromPath('/common/test/resources/keyboards/test_917.js');
    assert.instanceOf(test_kbd, JSKeyboard);
    const test_jskbd = test_kbd as JSKeyboard;
    jsHarness.activeKeyboard = test_jskbd;
    assert.isNotOk(jsHarness.loadedKeyboard);

    // With an active keyboard, load three keyboards but activate none of them.
    const lao_keyboard_promise = keyboardLoader.loadKeyboardFromPath('/common/test/resources/keyboards/lao_2008_basic.js');
    const khmer_keyboard_promise = keyboardLoader.loadKeyboardFromPath('/common/test/resources/keyboards/khmer_angkor.js');
    const chiral_keyboard_promise = keyboardLoader.loadKeyboardFromPath('/common/test/resources/keyboards/test_chirality.js');

    // Sure, why not `await` out of order?
    const chiral_keyboard = await chiral_keyboard_promise;
    const lao_keyboard = await lao_keyboard_promise;
    const khmer_keyboard = await khmer_keyboard_promise;

    assert.strictEqual(test_kbd, jsHarness.activeKeyboard);
    assert.isNotOk(jsHarness.loadedKeyboard);

    assert.isOk(lao_keyboard);
    assert.isOk(chiral_keyboard);
    assert.isOk(khmer_keyboard);

    // This part provides assurance that the keyboard properly loaded.
    assert.equal(lao_keyboard.id, "Keyboard_lao_2008_basic");
    assert.equal(khmer_keyboard.id, "Keyboard_khmer_angkor");
    assert.equal(chiral_keyboard.id, "Keyboard_test_chirality");

    jsHarness.activeKeyboard = lao_keyboard as JSKeyboard;
  });
});
