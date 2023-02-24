let assert = chai.assert;

import { DOMKeyboardLoader, DOMKeyboardSandbox } from '../../../build/lib/keyboards/loaders/domKeyboardLoader.mjs';
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
    let keyboardLoader = new DOMKeyboardLoader(new KeyboardHarness(window, MinimalKeymanGlobal));
    let keyboard = await keyboardLoader.loadKeyboardFromPath('/resources/keyboards/khmer_angkor.js');

    assert.isOk(keyboard);
    assert.equal(keyboard.id, 'Keyboard_khmer_angkor');
    assert.isTrue(keyboard.isChiral);
    assert.isFalse(keyboard.isCJK);
    assert.isOk(window.KeymanWeb);
    assert.isOk(window.keyman);
  });

  it('`window`, enabled rule processing', async () => {
    const harness = new KeyboardInterface(window, MinimalKeymanGlobal);
    const keyboardLoader = new DOMKeyboardLoader(harness);
    const keyboard = await keyboardLoader.loadKeyboardFromPath('/resources/keyboards/khmer_angkor.js');

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
  });
});