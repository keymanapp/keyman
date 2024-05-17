import { assert } from '../../../../../../node_modules/chai/chai.js';

import { DOMKeyboardLoader } from '../../../build/lib/dom-keyboard-loader.mjs';
import { extendString, KeyboardHarness, KeyboardInterface, MinimalKeymanGlobal, Mock } from '../../../build/lib/index.mjs';

// Note:  rule processing tests will fail if string extensions are not established beforehand.
extendString();

const device = {
  touchable: false,
  formFactor: 'phone',
  OS: 'ios',
  browser: 'safari'
}

describe('Keyboard loading in DOM', function() {
  afterEach(() => {
    if(window.KeymanWeb) {
      window.KeymanWeb.uninstall();
    }
  })

  it('`window`, disabled rule processing', async () => {
    const harness = new KeyboardHarness(window, MinimalKeymanGlobal);
    let keyboardLoader = new DOMKeyboardLoader(harness);
    let keyboard = await keyboardLoader.loadKeyboardFromPath('/resources/keyboards/khmer_angkor.js');

    assert.isOk(keyboard);
    assert.equal(keyboard.id, 'Keyboard_khmer_angkor');
    assert.isTrue(keyboard.isChiral);
    assert.isFalse(keyboard.isCJK);
    assert.isOk(window.KeymanWeb);
    assert.isOk(window.keyman);

    // Should be not be modified by the keyboard load; it is not activated by default.
    assert.isNotOk(harness.activeKeyboard);

    // Should be cleared post-keyboard-load.
    assert.isNotOk(harness.loadedKeyboard);
  });

  it('`window`, enabled rule processing', async () => {
    const harness = new KeyboardInterface(window, MinimalKeymanGlobal);
    const keyboardLoader = new DOMKeyboardLoader(harness);
    const keyboard = await keyboardLoader.loadKeyboardFromPath('/resources/keyboards/khmer_angkor.js');
    harness.activeKeyboard = keyboard;

    assert.isOk(keyboard);
    assert.equal(keyboard.id, 'Keyboard_khmer_angkor');
    assert.isTrue(keyboard.isChiral);
    assert.isFalse(keyboard.isCJK);
    assert.isOk(window.KeymanWeb);
    assert.isOk(window.keyman);

    // TODO:  verify actual rule processing.
    const nullKeyEvent = keyboard.constructNullKeyEvent(device);
    const mock = new Mock();
    const result = harness.processKeystroke(mock, nullKeyEvent);

    assert.isOk(result);
    assert.isOk(window.KeymanWeb);
    assert.isOk(window.keyman);

    // Should be cleared post-keyboard-load.
    assert.isNotOk(harness.loadedKeyboard);
  });

  it('load keyboards successfully in parallel without side effects', async () => {
    let harness = new KeyboardInterface(window, MinimalKeymanGlobal);
    let keyboardLoader = new DOMKeyboardLoader(harness);

    // Preload a keyboard and make it active.
    const test_kbd = await keyboardLoader.loadKeyboardFromPath('/resources/keyboards/test_917.js');
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