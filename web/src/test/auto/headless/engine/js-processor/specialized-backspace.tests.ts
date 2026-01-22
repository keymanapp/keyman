import { assert } from 'chai';

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

import { DeviceSpec, KMWString } from 'keyman/common/web-utils';
import { Codes, DefaultOutputRules, JSKeyboard, KeyEvent, MinimalKeymanGlobal, SyntheticTextStore } from 'keyman/engine/keyboard';
import { JSKeyboardInterface, JSKeyboardProcessor } from 'keyman/engine/js-processor';
import { NodeKeyboardLoader } from 'keyman/test/resources';
import { ModifierKeyConstants } from '@keymanapp/common-types';


const TEST_DEVICE = {
  formFactor: 'desktop',
  OS: 'windows',
  browser: 'native',
  touchable: false
} as DeviceSpec;

// Basic scaffolding necessary to use special, locally-defined test keyboards.
class CommonKbdScriptProps {
  KMINVER = "10.0";
  // this.KV left empty - we aren't doing layout stuff for this test, so it's "fine".
  // - also this.KV.KLS
  // - also this.KV.BK
  KH = '';
  KM = 0;
  KBVER = "0.0.1";
  KMBM = ModifierKeyConstants.K_SHIFTFLAG /* 0x0010 */;
  gs(t: any, e: any) {
    return (this as any).g_main(t, e);
  }
}

const COMMON_KBD_SCRIPT_PROPS = new CommonKbdScriptProps();

const DUMMIED_KEYS_KEYBOARD_SCRIPT = function keyboard_core (this: any) {
  Object.assign(this, COMMON_KBD_SCRIPT_PROPS);

  this.KI="Keyboard_dummied_keys";
  this.KN="Dummied Keys";
  this.g_main=function(t:any, e:any) {
    // All keys match but do nothing.
    return 1;
  }
  this.gs=function(t:any, e:any) {
    return this.g_main(t, e);
  }
}

const DOUBLED_BKSP_KEYBOARD_SCRIPT = function keyboard_core (this: any) {
  Object.assign(this, COMMON_KBD_SCRIPT_PROPS);

  this.KI="Keyboard_doubled_backspace";
  this.KN="Doubled Backspace";
  this.KO=function(count: number, t: any, output: string) {
    // Delete 'count' characters from the text store and output the replacement
    t.deleteCharsBeforeCaret(count);
    if(output) {
      t.insertTextBeforeCaret(output);
    }
  };
  this.g_main=function(t: { getTextBeforeCaret: () => string; }, e: { Lcode: number; }) {
    // Standard compiled keyboards do not directly use any methods on `t`.
    if(e.Lcode == Codes.keyCodes.K_BKSP && KMWString.length(t.getTextBeforeCaret()) >= 2){
      // If the context has at least two characters, delete two.
      this.KO(2, t, '')
      // Our rule matched, so signal that.
      return 1;
    } else {
      // All other keystrokes should result in default behavior.
      return 0;
    }
  }
  this.gs=function(t:any, e:any) {
    return this.g_main(t, e);
  }
}

describe('Engine - specialized backspace handling', function() {
  const ipaPath = require.resolve('@keymanapp/common-test-resources/keyboards/sil_ipa.js');
  const angkorPath = require.resolve('@keymanapp/common-test-resources/keyboards/khmer_angkor.js');

  const device = {
    formFactor: 'desktop',
    OS: 'windows',
    browser: 'native',
    touchable: false
  } as DeviceSpec;

  let ipaWithHarness: JSKeyboardInterface;
  let angkorWithHarness: JSKeyboardInterface;
  let dummiedWithHarness: JSKeyboardInterface;
  let bksp2xWithHarness: JSKeyboardInterface;

  before(async () => {
    // -- START: Standard keyboard unit test loading boilerplate --
    let harness = new JSKeyboardInterface({}, MinimalKeymanGlobal);
    let keyboardLoader = new NodeKeyboardLoader(harness);
    let keyboard = await keyboardLoader.loadKeyboardFromPath(ipaPath);
    // --  END:  Standard keyboard unit test loading boilerplate --

    // This part provides extra assurance that the keyboard properly loaded.
    assert.equal(keyboard.id, "Keyboard_sil_ipa");

    harness.activeKeyboard = keyboard as JSKeyboard;
    ipaWithHarness = harness;

    // --------------

    // -- START: Standard keyboard unit test loading boilerplate --
    harness = new JSKeyboardInterface({}, MinimalKeymanGlobal);
    keyboardLoader = new NodeKeyboardLoader(harness);
    keyboard = await keyboardLoader.loadKeyboardFromPath(angkorPath);
    // --  END:  Standard keyboard unit test loading boilerplate --

    // This part provides extra assurance that the keyboard properly loaded.
    assert.equal(keyboard.id, "Keyboard_khmer_angkor");

    harness.activeKeyboard = keyboard as JSKeyboard;
    angkorWithHarness = harness;

    // --------------

    harness = new JSKeyboardInterface(globalThis, MinimalKeymanGlobal);
    harness.install();
    // Sets the keyboard as the harness's "loaded" keyboard, but not "active".
    harness.KR(new (DUMMIED_KEYS_KEYBOARD_SCRIPT as any)());
    harness.activeKeyboard = harness.loadedKeyboard;
    assert.isOk(harness.activeKeyboard);
    dummiedWithHarness = harness;

    // --------------

    harness = new JSKeyboardInterface(globalThis, MinimalKeymanGlobal);
    harness.install();
    // Sets the keyboard as the harness's "loaded" keyboard, but not "active".
    harness.KR(new (DOUBLED_BKSP_KEYBOARD_SCRIPT as any)());
    harness.activeKeyboard = harness.loadedKeyboard;
    assert.isOk(harness.activeKeyboard);
    bksp2xWithHarness = harness;
  });

  it('empty context, positional keyboard', () => {
    const contextSource = new SyntheticTextStore('');
    const event = new KeyEvent({
      Lcode: Codes.keyCodes.K_BKSP,
      Lmodifiers: 0,
      Lstates: ModifierKeyConstants.NOTCAPITALFLAG | ModifierKeyConstants.NOTNUMLOCKFLAG | ModifierKeyConstants.NOTSCROLLFLAG,
      LisVirtualKey: true,
      kName: '',
      vkCode: Codes.keyCodes.K_BKSP,
      device
    });

    const processor = new JSKeyboardProcessor(TEST_DEVICE, {
      baseLayout: 'us',
      keyboardInterface: angkorWithHarness,
      defaultOutputRules: new DefaultOutputRules()
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
    const contextSource = new SyntheticTextStore('');
    const event = new KeyEvent({
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
    const processor = new JSKeyboardProcessor(TEST_DEVICE, {
      baseLayout: 'us',
      keyboardInterface: dummiedWithHarness,
      defaultOutputRules: new DefaultOutputRules()
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
    const contextSource = new SyntheticTextStore('selected text', 0);
    contextSource.setSelection(0, KMWString.length(contextSource.getText()));

    const event = new KeyEvent({
      Lcode: Codes.keyCodes.K_BKSP,
      Lmodifiers: 0,
      Lstates: ModifierKeyConstants.NOTCAPITALFLAG | ModifierKeyConstants.NOTNUMLOCKFLAG | ModifierKeyConstants.NOTSCROLLFLAG,
      LisVirtualKey: true,
      kName: '',
      vkCode: Codes.keyCodes.K_BKSP,
      device: device
    });

    const processor = new JSKeyboardProcessor(TEST_DEVICE, {
      baseLayout: 'us',
      keyboardInterface: angkorWithHarness,
      defaultOutputRules: new DefaultOutputRules()
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
    const contextSource = new SyntheticTextStore('post-caret text', 0);
    const event = new KeyEvent({
      Lcode: Codes.keyCodes.K_BKSP,
      Lmodifiers: 0,
      Lstates: ModifierKeyConstants.NOTCAPITALFLAG | ModifierKeyConstants.NOTNUMLOCKFLAG | ModifierKeyConstants.NOTSCROLLFLAG,
      LisVirtualKey: true,
      kName: '',
      vkCode: Codes.keyCodes.K_BKSP,
      device: device
    });

    const processor = new JSKeyboardProcessor(TEST_DEVICE, {
      baseLayout: 'us',
      keyboardInterface: angkorWithHarness,
      defaultOutputRules: new DefaultOutputRules()
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
    const contextSource = new SyntheticTextStore('');
    const event = new KeyEvent({
      Lcode: Codes.keyCodes.K_BKSP,
      Lmodifiers: 0,
      Lstates: ModifierKeyConstants.NOTCAPITALFLAG | ModifierKeyConstants.NOTNUMLOCKFLAG | ModifierKeyConstants.NOTSCROLLFLAG,
      LisVirtualKey: true,
      kName: '',
      vkCode: Codes.keyCodes.K_BKSP,
      device: device
    });

    const processor = new JSKeyboardProcessor(TEST_DEVICE, {
      baseLayout: 'us',
      keyboardInterface: ipaWithHarness,
      defaultOutputRules: new DefaultOutputRules()
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
    const contextSource = new SyntheticTextStore('abc', 2);
    const event = new KeyEvent({
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

    const processor = new JSKeyboardProcessor(TEST_DEVICE, {
      baseLayout: 'us',
      keyboardInterface: bksp2xWithHarness,
      defaultOutputRules: new DefaultOutputRules()
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
    const contextSource = new SyntheticTextStore('');
    const event = new KeyEvent({
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
    const processor = new JSKeyboardProcessor(TEST_DEVICE, {
      baseLayout: 'us',
      keyboardInterface: dummiedWithHarness,
      defaultOutputRules: new DefaultOutputRules()
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
