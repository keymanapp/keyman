import { assert } from 'chai';
import { ModifierKeyConstants } from '@keymanapp/common-types';

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

import { Codes, KeyEvent, MinimalKeymanGlobal, SyntheticTextStore } from 'keyman/engine/keyboard';
import { JSKeyboardInterface } from 'keyman/engine/js-processor';
import { NodeKeyboardLoader } from '../../../resources/loader/nodeKeyboardLoader.js';

// Compare and contrast the unit tests here with those for app/browser key-event unit testing
// in the hardware-event-processing set; the output objects there should have the same format
// as the event object inputs used here.

describe('Engine - rule processing', function() {
  const ipaPath = require.resolve('@keymanapp/common-test-resources/keyboards/sil_ipa.js');
  const armenianPath = require.resolve('@keymanapp/common-test-resources/keyboards/armenian.js');

  let device = {
    formFactor: 'desktop',
    OS: 'windows',
    browser: 'native'
  }

  describe('for mnemonic keyboards (via `sil_ipa`)', function () {
    let keyboardWithHarness;

    before(async () => {
      // -- START: Standard keyboard unit test loading boilerplate --
      let harness = new JSKeyboardInterface({}, MinimalKeymanGlobal);
      let keyboardLoader = new NodeKeyboardLoader(harness);
      let keyboard = await keyboardLoader.loadKeyboardFromPath(ipaPath);
      // --  END:  Standard keyboard unit test loading boilerplate --

      // This part provides extra assurance that the keyboard properly loaded.
      assert.equal(keyboard.id, "Keyboard_sil_ipa");

      harness.activeKeyboard = keyboard;
      keyboardWithHarness = harness;
    });

    it('matches rules with mnemonic-specced KeyEvents', () => {
      // Note:  plain 'n' is produced from default key outputs for sil_ipa, not a keyboard rule.
      let mnemonicTextStore = new SyntheticTextStore('n');
      let mnemonicEvent = new KeyEvent({
        // sil_ipa is a mnenomic keyboard:  it expects codes based on the key's standard character output.
        Lcode: '>'.charCodeAt(0), // 62
        Lmodifiers: ModifierKeyConstants.K_SHIFTFLAG, // '>' is shift-layer.
        Lstates: ModifierKeyConstants.NOTCAPITALFLAG | ModifierKeyConstants.NOTNUMLOCKFLAG | ModifierKeyConstants.NOTSCROLLFLAG,
        LisVirtualKey: true,
        kName: '',
        vkCode: Codes.keyCodes.K_N,
        device: device
      });

      const mnemonicResult = keyboardWithHarness.processKeystroke(mnemonicTextStore, mnemonicEvent);

      assert.isOk(mnemonicResult);
      assert.isFalse(mnemonicResult.triggerKeyDefault);
      assert.equal(mnemonicTextStore.getText(), 'ŋ');
    });

    it('requires correct modifiers', () => {
      // Note:  plain 'n' is produced from default key outputs for sil_ipa, not a keyboard rule.
      let mnemonicTextStore = new SyntheticTextStore('n');
      let mnemonicEvent = new KeyEvent({
        // sil_ipa is a mnenomic keyboard:  it expects codes based on the key's standard character output.
        Lcode: '>'.charCodeAt(0), // 62
        Lmodifiers: 0, // '>' is shift-layer, not default - and this matters for context matching.
        Lstates: ModifierKeyConstants.NOTCAPITALFLAG | ModifierKeyConstants.NOTNUMLOCKFLAG | ModifierKeyConstants.NOTSCROLLFLAG,
        LisVirtualKey: true,
        kName: '',
        vkCode: Codes.keyCodes.K_N,
        device: device
      });

      const mnemonicResult = keyboardWithHarness.processKeystroke(mnemonicTextStore, mnemonicEvent);

      assert.isOk(mnemonicResult);
      assert.isTrue(mnemonicResult.triggerKeyDefault);
    });

    it('does not match rules with positional-specced KeyEvents', () => {
      let positionalTextStore = new SyntheticTextStore('n');
      let positionalEvent = new KeyEvent({
        // If it were positional, we'd use this instead:
        Lcode: Codes.keyCodes.K_COMMA, // 188
        Lmodifiers: ModifierKeyConstants.K_SHIFTFLAG, // '>' is shift-layer.
        Lstates: ModifierKeyConstants.NOTCAPITALFLAG | ModifierKeyConstants.NOTNUMLOCKFLAG | ModifierKeyConstants.NOTSCROLLFLAG,
        LisVirtualKey: true,
        kName: '',
        vkCode: Codes.keyCodes.K_N,
        device: device
      });

      const positionalResult = keyboardWithHarness.processKeystroke(positionalTextStore, positionalEvent);

      assert.isOk(positionalResult);
      assert.isTrue(positionalResult.triggerKeyDefault);
      assert.notEqual(positionalTextStore.getText(), 'ŋ');
    });
  });

  describe('for KMW 1.0 legacy keyboards (via `armenian`)', function () {
    let keyboardWithHarness;

    before(async () => {
      // -- START: Standard keyboard unit test loading boilerplate --
      let harness = new JSKeyboardInterface({}, MinimalKeymanGlobal);
      let keyboardLoader = new NodeKeyboardLoader(harness);
      let keyboard = await keyboardLoader.loadKeyboardFromPath(armenianPath);
      // --  END:  Standard keyboard unit test loading boilerplate --

      // This part provides extra assurance that the keyboard properly loaded.
      assert.equal(keyboard.id, "Keyboard_armenian");

      harness.activeKeyboard = keyboard;
      keyboardWithHarness = harness;
    });

    it('matches rules with legacy-specced KeyEvents', () => {
      let legacyTextStore = new SyntheticTextStore('');
      let legacyEvent = new KeyEvent({
        // armenian is a KMW 1.0 keyboard:  it expects codes based on the key's standard character output.
        Lcode: 'a'.charCodeAt(0),
        Lmodifiers: 0,
        Lstates: ModifierKeyConstants.NO_CAPSFLAG | ModifierKeyConstants.NO_NUM_LOCKFLAG | ModifierKeyConstants.NO_SCROLL_LOCKFLAG,
        LisVirtualKey: false,
        kName: '',
        vkCode: Codes.keyCodes.K_N,
        device: device
      });

      const legacyResult = keyboardWithHarness.processKeystroke(legacyTextStore, legacyEvent);

      assert.isOk(legacyResult);
      assert.isFalse(legacyResult.triggerKeyDefault);
      assert.equal(legacyTextStore.getText(), 'ա');
    });

    it('ignores current modifiers and states', () => {
      let legacyTextStore = new SyntheticTextStore('');
      let legacyEvent = new KeyEvent({
        // armenian is a KMW 1.0 keyboard:  it expects codes based on the key's standard character output.
        Lcode: 'a'.charCodeAt(0),
        Lmodifiers: 27,
        Lstates: ModifierKeyConstants.CAPITALFLAG | ModifierKeyConstants.NOTNUMLOCKFLAG | ModifierKeyConstants.SCROLLFLAG | 0x5C00,
        LisVirtualKey: false,
        kName: '',
        vkCode: Codes.keyCodes.K_N,
        device: device
      });

      const legacyResult = keyboardWithHarness.processKeystroke(legacyTextStore, legacyEvent);

      assert.isOk(legacyResult);
      assert.isFalse(legacyResult.triggerKeyDefault);
      assert.equal(legacyTextStore.getText(), 'ա');
    });

    it('does not match rules with mnemonic-specced KeyEvents', () => {
      let mnemonicTextStore = new SyntheticTextStore('');
      let mnemonicEvent = new KeyEvent({
        // armenian is a KMW 1.0 keyboard:  it expects codes based on the key's standard character output.
        Lcode: 'a'.charCodeAt(0),
        Lmodifiers: 0,
        Lstates: ModifierKeyConstants.NOTCAPITALFLAG | ModifierKeyConstants.NOTNUMLOCKFLAG | ModifierKeyConstants.NOTSCROLLFLAG,
        LisVirtualKey: true,
        kName: '',
        vkCode: Codes.keyCodes.K_N,
        device: device
      });

      const mnemonicResult = keyboardWithHarness.processKeystroke(mnemonicTextStore, mnemonicEvent);

      assert.isOk(mnemonicResult);
      assert.isTrue(mnemonicResult.triggerKeyDefault);
    });

    it('does not match rules with positional-specced KeyEvents', () => {
      let positionalTextStore = new SyntheticTextStore('');
      let positionalEvent = new KeyEvent({
        // If it were positional, we'd use this instead:
        Lcode: Codes.keyCodes.K_A,
        Lmodifiers: 0,
        Lstates: ModifierKeyConstants.NOTCAPITALFLAG | ModifierKeyConstants.NOTNUMLOCKFLAG | ModifierKeyConstants.NOTSCROLLFLAG,
        LisVirtualKey: true,
        kName: '',
        vkCode: Codes.keyCodes.K_N,
        device: device
      });

      const positionalResult = keyboardWithHarness.processKeystroke(positionalTextStore, positionalEvent);

      assert.isOk(positionalResult);
      assert.isTrue(positionalResult.triggerKeyDefault);
    });
  });
});
