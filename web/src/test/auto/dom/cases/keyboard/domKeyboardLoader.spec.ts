import { assert } from 'chai';

import { DOMKeyboardLoader } from 'keyman/engine/keyboard/dom-keyboard-loader';
import { extendString, KeyboardHarness, Keyboard, MinimalKeymanGlobal, DeviceSpec, KeyboardKeymanGlobal } from 'keyman/engine/keyboard';
import { KeyboardInterface, Mock } from 'keyman/engine/js-processor';

declare let window: typeof globalThis;
// KeymanEngine from the web/ folder... when available.
// At this level, though... we just mock it.
declare let keyman: KeyboardKeymanGlobal;
declare let KeymanWeb: KeyboardInterface;

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

  it('`window`, disabled rule processing', async () => {
    const harness = new KeyboardHarness(window, MinimalKeymanGlobal);
    let keyboardLoader = new DOMKeyboardLoader(harness);
    let keyboard: Keyboard = await keyboardLoader.loadKeyboardFromPath('/resources/keyboards/khmer_angkor.js');

    assert.isOk(keyboard);
    assert.equal(keyboard.id, 'Keyboard_khmer_angkor');
    assert.isTrue(keyboard.isChiral);
    assert.isFalse(keyboard.isCJK);
    assert.isOk(KeymanWeb);
    assert.isOk(keyman);
    assert.isOk(keyman.osk);
    assert.isOk(keyman.osk.keyCodes);

    // Should be cleared post-keyboard-load.
    assert.isNotOk(harness.loadedKeyboard);
  });

  it('`window`, enabled rule processing', async () => {
    const harness = new KeyboardInterface(window, MinimalKeymanGlobal);
    const keyboardLoader = new DOMKeyboardLoader(harness);
    const keyboard: Keyboard = await keyboardLoader.loadKeyboardFromPath('/resources/keyboards/khmer_angkor.js');
    harness.activeKeyboard = keyboard;

    assert.isOk(keyboard);
    assert.equal(keyboard.id, 'Keyboard_khmer_angkor');
    assert.isTrue(keyboard.isChiral);
    assert.isFalse(keyboard.isCJK);
    assert.isOk(KeymanWeb);
    assert.isOk(keyman);
    assert.isOk(keyman.osk);
    assert.isOk(keyman.osk.keyCodes);

    // TODO:  verify actual rule processing.
    const nullKeyEvent = keyboard.constructNullKeyEvent(device);
    const mock = new Mock();
    const result = harness.processKeystroke(mock, nullKeyEvent);

    assert.isOk(result);
    assert.isOk(KeymanWeb);
    assert.isOk(keyman);
    assert.isOk(keyman.osk);
    assert.isOk(keyman.osk.keyCodes);

    // Should be cleared post-keyboard-load.
    assert.isNotOk(harness.loadedKeyboard);
  });

  it('load keyboards successfully in parallel without side effects', async () => {
    let harness = new KeyboardInterface(window, MinimalKeymanGlobal);
    let keyboardLoader = new DOMKeyboardLoader(harness);

    // Preload a keyboard and make it active.
    const test_kbd: Keyboard = await keyboardLoader.loadKeyboardFromPath('/resources/keyboards/test_917.js');
    harness.activeKeyboard = test_kbd;
    assert.isNotOk(harness.loadedKeyboard);

    // With an active keyboard, load three keyboards but activate none of them.
    const lao_keyboard_promise = keyboardLoader.loadKeyboardFromPath('/resources/keyboards/lao_2008_basic.js');
    const khmer_keyboard_promise = keyboardLoader.loadKeyboardFromPath('/resources/keyboards/khmer_angkor.js');
    const chiral_keyboard_promise = keyboardLoader.loadKeyboardFromPath('/resources/keyboards/test_chirality.js');

    // Sure, why not `await` out of order?
    const chiral_keyboard = await chiral_keyboard_promise;
    const lao_keyboard = await lao_keyboard_promise;
    const khmer_keyboard = await khmer_keyboard_promise;

    assert.strictEqual(test_kbd, harness.activeKeyboard);
    assert.isNotOk(harness.loadedKeyboard);

    assert.isOk(lao_keyboard);
    assert.isOk(chiral_keyboard);
    assert.isOk(khmer_keyboard);

    // This part provides assurance that the keyboard properly loaded.
    assert.equal(lao_keyboard.id, "Keyboard_lao_2008_basic");
    assert.equal(khmer_keyboard.id, "Keyboard_khmer_angkor");
    assert.equal(chiral_keyboard.id, "Keyboard_test_chirality");

    harness.activeKeyboard = lao_keyboard;
  });
});
