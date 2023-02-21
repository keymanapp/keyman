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

  // Note:  tests already pass, but we don't wish to "turn the feature on" and maintain it at this time.
  it.skip('`<iframe>` sandbox, disabled rule processing', async () => {
    let sandboxedGlobal = await DOMKeyboardSandbox.buildSandbox();
    let keyboardLoader = new DOMKeyboardLoader(new KeyboardHarness(sandboxedGlobal.sandbox, MinimalKeymanGlobal), sandboxedGlobal);
    let keyboard = await keyboardLoader.loadKeyboardFromPath('/resources/keyboards/khmer_angkor.js');

    assert.isOk(keyboard);
    assert.equal(keyboard.id, 'Keyboard_khmer_angkor');
    assert.isTrue(keyboard.isChiral);
    assert.isFalse(keyboard.isCJK);
    assert.isNotOk(window.KeymanWeb);
    assert.isNotOk(window.keyman);
  });

  // Note:  tests already pass, but we don't wish to "turn the feature on" and maintain it at this time.
  it.skip('`<iframe>` sandbox, enabled rule processing', async () => {
    const sandboxedGlobal = await DOMKeyboardSandbox.buildSandbox();
    const harness = new KeyboardInterface(sandboxedGlobal.sandbox, MinimalKeymanGlobal);
    const keyboardLoader = new DOMKeyboardLoader(harness, sandboxedGlobal);
    const keyboard = await keyboardLoader.loadKeyboardFromPath('/resources/keyboards/khmer_angkor.js');
    assert.isNotOk(window.KeymanWeb);

    assert.isOk(keyboard);
    assert.equal(keyboard.id, 'Keyboard_khmer_angkor');
    assert.isTrue(keyboard.isChiral);
    assert.isFalse(keyboard.isCJK);
    assert.isNotOk(window.KeymanWeb);
    assert.isNotOk(window.keyman);

    // TODO:  verify actual rule processing.
    const nullKeyEvent = keyboard.constructNullKeyEvent(device);
    const mock = new Mock();
    const result = harness.processKeystroke(mock, nullKeyEvent);

    assert.isOk(result);
    assert.isNotOk(window.KeymanWeb);
    assert.isNotOk(window.keyman);
  });

  // Obviously, not yet implemented; this aspect is not adequately tested and is a part of
  // why the potential feature's on pause.
  it.skip('`<iframe>` sandbox, loading from remote URL', () => {});
});