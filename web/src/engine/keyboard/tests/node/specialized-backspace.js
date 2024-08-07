import { assert } from 'chai';
import fs from 'fs';

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

import { Codes, KeyEvent, MinimalKeymanGlobal, Mock } from 'keyman/engine/keyboard';
import { KeyboardInterface, KeyboardProcessor } from 'keyman/engine/js-processor';
import { NodeKeyboardLoader } from 'keyman/engine/keyboard/node-keyboard-loader';
import { ModifierKeyConstants } from '@keymanapp/common-types';


const TEST_DEVICE = {
  formFactor: 'desktop',
  OS: 'windows',
  browser: 'native',
  touchable: false
}

// Basic scaffolding necessary to use special, locally-defined test keyboards.
const COMMON_KBD_SCRIPT_PROPS = new (function (){
  this.KMINVER="10.0";
  // this.KV left empty - we aren't doing layout stuff for this test, so it's "fine".
  // - also this.KV.KLS
  // - also this.KV.BK
  this.KH='';
  this.KM=0;
  this.KBVER="0.0.1";
  this.KMBM=ModifierKeyConstants.K_SHIFTFLAG /* 0x0010 */;
  this.gs=function(t,e) {
    return this.g_main(t,e);
  };
})();

const DUMMIED_KEYS_KEYBOARD_SCRIPT = function keyboard_core () {
  Object.assign(this, COMMON_KBD_SCRIPT_PROPS);

  this.KI="Keyboard_dummied_keys";
  this.KN="Dummied Keys";
  this.g_main=function(t,e) {
    // All keys match but do nothing.
    return 1;
  }
}

const DOUBLED_BKSP_KEYBOARD_SCRIPT = function keyboard_core () {
  Object.assign(this, COMMON_KBD_SCRIPT_PROPS);

  this.KI="Keyboard_doubled_backspace";
  this.KN="Doubled Backspace";
  this.g_main=function(t,e) {
    var k=KeymanWeb;
    // Standard compiled keyboards do not directly use any methods on `t`.
    if(e.Lcode == Codes.keyCodes.K_BKSP && t.getTextBeforeCaret().kmwLength() >= 2){
      // If the context has at least two characters, delete two.
      k.KO(2, t, '')
      // Our rule matched, so signal that.
      return 1;
    } else {
      // All other keystrokes should result in default behavior.
      return 0;
    }
  }
}

describe('Engine - specialized backspace handling', function() {
  const ipaPath = require.resolve('@keymanapp/common-test-resources/keyboards/sil_ipa.js');
  const angkorPath = require.resolve('@keymanapp/common-test-resources/keyboards/khmer_angkor.js');

  let device = {
    formFactor: 'desktop',
    OS: 'windows',
    browser: 'native'
  }

  let ipaWithHarness;
  let angkorWithHarness;
  let dummiedWithHarness;
  let bksp2xWithHarness;

  before(async () => {
    // -- START: Standard keyboard unit test loading boilerplate --
    let harness = new KeyboardInterface({}, MinimalKeymanGlobal);
    let keyboardLoader = new NodeKeyboardLoader(harness);
    let keyboard = await keyboardLoader.loadKeyboardFromPath(ipaPath);
    // --  END:  Standard keyboard unit test loading boilerplate --

    // This part provides extra assurance that the keyboard properly loaded.
    assert.equal(keyboard.id, "Keyboard_sil_ipa");

    harness.activeKeyboard = keyboard;
    ipaWithHarness = harness;

    // --------------

    // -- START: Standard keyboard unit test loading boilerplate --
    harness = new KeyboardInterface({}, MinimalKeymanGlobal);
    keyboardLoader = new NodeKeyboardLoader(harness);
    keyboard = await keyboardLoader.loadKeyboardFromPath(angkorPath);
    // --  END:  Standard keyboard unit test loading boilerplate --

    // This part provides extra assurance that the keyboard properly loaded.
    assert.equal(keyboard.id, "Keyboard_khmer_angkor");

    harness.activeKeyboard = keyboard;
    angkorWithHarness = harness;

    // --------------

    harness = new KeyboardInterface(globalThis, MinimalKeymanGlobal);
    harness.install();
    // Sets the keyboard as the harness's "loaded" keyboard, but not "active".
    harness.KR(new DUMMIED_KEYS_KEYBOARD_SCRIPT());
    harness.activeKeyboard = harness.loadedKeyboard;
    assert.isOk(harness.activeKeyboard);
    dummiedWithHarness = harness;

    // --------------

    harness = new KeyboardInterface(globalThis, MinimalKeymanGlobal);
    harness.install();
    // Sets the keyboard as the harness's "loaded" keyboard, but not "active".
    harness.KR(new DOUBLED_BKSP_KEYBOARD_SCRIPT());
    harness.activeKeyboard = harness.loadedKeyboard;
    assert.isOk(harness.activeKeyboard);
    bksp2xWithHarness = harness;
  });

  it('empty context, positional keyboard', () => {
    let contextSource = new Mock('');
    let event = new KeyEvent({
      Lcode: Codes.keyCodes.K_BKSP,
      Lmodifiers: 0,
      Lstates: ModifierKeyConstants.NOTCAPITALFLAG | ModifierKeyConstants.NOTNUMLOCKFLAG | ModifierKeyConstants.NOTSCROLLFLAG,
      LisVirtualKey: true,
      kName: '',
      vkCode: Codes.keyCodes.K_BKSP,
      device: device
    });

    const processor = new KeyboardProcessor(TEST_DEVICE, {
      keyboardInterface: angkorWithHarness
    });
    const result = processor.processKeystroke(event, contextSource);

    assert.isTrue(result.triggerKeyDefault);
    assert.equal(contextSource.getTextBeforeCaret(), '');
    assert.equal(contextSource.getTextAfterCaret(), '');
    assert.isOk(result?.transcription?.transform);

    const transform = result.transcription.transform;
    assert.equal(transform.insert, '');
    assert.equal(transform.deleteLeft, 1);
    assert.isNotOk(transform.deleteRight);
  });

  it("empty context, positional keyboard, outputless-key that's not BKSP", () => {
    let contextSource = new Mock('');
    let event = new KeyEvent({
      Lcode: Codes.keyCodes.K_A,
      Lmodifiers: 0,
      Lstates: ModifierKeyConstants.NOTCAPITALFLAG | ModifierKeyConstants.NOTNUMLOCKFLAG | ModifierKeyConstants.NOTSCROLLFLAG,
      LisVirtualKey: true,
      kName: '',
      vkCode: Codes.keyCodes.K_A,
      device: device
    });

    // A specialized test keyboard that handles keys without emitting any content.
    // We want to ensure error cases without output, on null context, don't act
    // like backspaces.
    const processor = new KeyboardProcessor(TEST_DEVICE, {
      keyboardInterface: dummiedWithHarness
    });
    const result = processor.processKeystroke(event, contextSource);

    // Did match a keyboard rule.
    assert.isFalse(result.triggerKeyDefault);
    assert.equal(contextSource.getTextBeforeCaret(), '');
    assert.equal(contextSource.getTextAfterCaret(), '');
    assert.isOk(result?.transcription?.transform);

    const transform = result.transcription.transform;
    assert.equal(transform.insert, '');
    // Does not trigger backspace-like behavior.
    assert.equal(transform.deleteLeft, 0);
    assert.isNotOk(transform.deleteRight);
  });

  it('empty context, positional keyboard, but text is selected', () => {
    let contextSource = new Mock('selected text', 0);
    contextSource.setSelection(0, contextSource.getText().kmwLength());

    let event = new KeyEvent({
      Lcode: Codes.keyCodes.K_BKSP,
      Lmodifiers: 0,
      Lstates: ModifierKeyConstants.NOTCAPITALFLAG | ModifierKeyConstants.NOTNUMLOCKFLAG | ModifierKeyConstants.NOTSCROLLFLAG,
      LisVirtualKey: true,
      kName: '',
      vkCode: Codes.keyCodes.K_BKSP,
      device: device
    });

    const processor = new KeyboardProcessor(TEST_DEVICE, {
      keyboardInterface: angkorWithHarness
    });
    const result = processor.processKeystroke(event, contextSource);

    assert.isFalse(result.triggerKeyDefault);
    assert.equal(contextSource.getTextBeforeCaret(), '');
    assert.equal(contextSource.getTextAfterCaret(), '');
    assert.isOk(result?.transcription?.transform);

    const transform = result.transcription.transform;
    assert.equal(transform.insert, '');
    // This BKSP was to delete selected text, NOT to delete text before the caret.
    assert.equal(transform.deleteLeft, 0);

    // Is currently not what I'd expect - it reports the length of the selected string instead!
    // Fixing that may have knock-on effects, though... and it seems to pass through
    // the apps just fine as-is.
    //
    // I do have concerns for certain scripts on iOS, though... see #4538.
    // assert.isNotOk(transform.deleteRight);
  });

  it('empty left-context, positional keyboard', () => {
    let contextSource = new Mock('post-caret text', 0);
    let event = new KeyEvent({
      Lcode: Codes.keyCodes.K_BKSP,
      Lmodifiers: 0,
      Lstates: ModifierKeyConstants.NOTCAPITALFLAG | ModifierKeyConstants.NOTNUMLOCKFLAG | ModifierKeyConstants.NOTSCROLLFLAG,
      LisVirtualKey: true,
      kName: '',
      vkCode: Codes.keyCodes.K_BKSP,
      device: device
    });

    const processor = new KeyboardProcessor(TEST_DEVICE, {
      keyboardInterface: angkorWithHarness
    });
    const result = processor.processKeystroke(event, contextSource);

    assert.isTrue(result.triggerKeyDefault);
    assert.equal(contextSource.getTextBeforeCaret(), '');
    assert.equal(contextSource.getTextAfterCaret(), 'post-caret text');
    assert.isOk(result?.transcription?.transform);

    const transform = result.transcription.transform;
    assert.equal(transform.insert, '');
    assert.equal(transform.deleteLeft, 1);
    assert.isNotOk(transform.deleteRight);
  });

  it('empty context, mnemonic keyboard', () => {
    let contextSource = new Mock('');
    let event = new KeyEvent({
      Lcode: Codes.keyCodes.K_BKSP,
      Lmodifiers: 0,
      Lstates: ModifierKeyConstants.NOTCAPITALFLAG | ModifierKeyConstants.NOTNUMLOCKFLAG | ModifierKeyConstants.NOTSCROLLFLAG,
      LisVirtualKey: true,
      kName: '',
      vkCode: Codes.keyCodes.K_BKSP,
      device: device
    });

    const processor = new KeyboardProcessor(TEST_DEVICE, {
      keyboardInterface: ipaWithHarness
    });
    const result = processor.processKeystroke(event, contextSource);

    assert.isTrue(result.triggerKeyDefault);
    assert.equal(contextSource.getTextBeforeCaret(), '');
    assert.equal(contextSource.getTextAfterCaret(), '');
    assert.isOk(result?.transcription?.transform);

    const transform = result.transcription.transform;
    assert.equal(transform.insert, '');
    assert.equal(transform.deleteLeft, 1);
    assert.isNotOk(transform.deleteRight);
  });

  it('final empty context, positional keyboard, rule-handled BKSP', () => {
    let contextSource = new Mock('abc', 2);
    let event = new KeyEvent({
      Lcode: Codes.keyCodes.K_BKSP,
      Lmodifiers: 0,
      Lstates: ModifierKeyConstants.NOTCAPITALFLAG | ModifierKeyConstants.NOTNUMLOCKFLAG | ModifierKeyConstants.NOTSCROLLFLAG,
      LisVirtualKey: true,
      kName: '',
      vkCode: Codes.keyCodes.K_BKSP,
      device: device
    });

    // A specialized test keyboard that duplicates backspaces when sufficient
    // context exists.

    const processor = new KeyboardProcessor(TEST_DEVICE, {
      keyboardInterface: bksp2xWithHarness
    });
    const result = processor.processKeystroke(event, contextSource);

    // Did match a keyboard rule.
    assert.isFalse(result.triggerKeyDefault);
    assert.equal(contextSource.getTextBeforeCaret(), '');
    assert.equal(contextSource.getTextAfterCaret(), 'c');
    assert.isOk(result?.transcription?.transform);

    const transform = result.transcription.transform;
    assert.equal(transform.insert, '');
    // Triggered keyboard's backspace behavior, not special started-as-null behavior.
    // (Ensure that _ending_ with empty context doesn't overwrite a rule that gets us there.)
    assert.equal(transform.deleteLeft, 2);
    assert.isNotOk(transform.deleteRight);
  });

  // Special case:  BKSP rule-matches with empty left-context.
  it("empty context, positional keyboard, outputless BKSP rule", () => {
    let contextSource = new Mock('');
    let event = new KeyEvent({
      Lcode: Codes.keyCodes.K_BKSP,
      Lmodifiers: 0,
      Lstates: ModifierKeyConstants.NOTCAPITALFLAG | ModifierKeyConstants.NOTNUMLOCKFLAG | ModifierKeyConstants.NOTSCROLLFLAG,
      LisVirtualKey: true,
      kName: '',
      vkCode: Codes.keyCodes.K_BKSP,
      device: device
    });

    // A specialized test keyboard that handles keys without emitting any content.
    // We want to ensure error cases without output, on null context, don't act
    // like backspaces.
    const processor = new KeyboardProcessor(TEST_DEVICE, {
      keyboardInterface: dummiedWithHarness
    });
    const result = processor.processKeystroke(event, contextSource);

    assert.isFalse(result.triggerKeyDefault);
    assert.equal(contextSource.getTextBeforeCaret(), '');
    assert.equal(contextSource.getTextAfterCaret(), '');
    assert.isOk(result?.transcription?.transform);

    const transform = result.transcription.transform;
    assert.equal(transform.insert, '');
    // Does not trigger backspace-like behavior because of the keyboard's rule.
    assert.equal(transform.deleteLeft, 0);
    assert.isNotOk(transform.deleteRight);
  });
});
