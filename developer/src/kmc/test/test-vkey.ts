import 'mocha';
import { assert } from 'chai';
import { VkeyCompiler } from '../src/keyman/compiler/vkey';
import { CompilerCallbacks, loadSectionFixture } from './helpers';
import { Vkey } from '../src/keyman/kmx/kmx-plus';
import { CompilerMessages } from '../src/keyman/compiler/messages';
import { USVirtualKeyCodes } from '../src/keyman/ldml-keyboard/virtual-key-constants';

describe('vkey compiler', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should compile minimal vkey data', function() {
    const callbacks = new CompilerCallbacks();
    let vkey = loadSectionFixture(VkeyCompiler, 'sections/vkey/minimal.xml', callbacks) as Vkey;
    assert.equal(callbacks.messages.length, 0);

    assert.equal(vkey.vkeys.length, 4);
    // Note, final order is sorted by `vkey` member
    assert.deepEqual(vkey.vkeys[0], {vkey: USVirtualKeyCodes.K_A, target: USVirtualKeyCodes.K_Q});
    assert.deepEqual(vkey.vkeys[1], {vkey: USVirtualKeyCodes.K_Q, target: USVirtualKeyCodes.K_A});
    assert.deepEqual(vkey.vkeys[2], {vkey: USVirtualKeyCodes.K_W, target: USVirtualKeyCodes.K_Z});
    assert.deepEqual(vkey.vkeys[3], {vkey: USVirtualKeyCodes.K_Z, target: USVirtualKeyCodes.K_W});
  });

  it('should hint on redundant data', function() {
    const callbacks = new CompilerCallbacks();
    let vkey = loadSectionFixture(VkeyCompiler, 'sections/vkey/redundant.xml', callbacks) as Vkey;
    assert.isNotNull(vkey);
    assert.equal(callbacks.messages.length, 1);
    assert.deepEqual(callbacks.messages[0], CompilerMessages.Hint_VkeyMapIsRedundant('A'));
  });

  it('should report an info message if same target found', function() {
    const callbacks = new CompilerCallbacks();
    let vkey = loadSectionFixture(VkeyCompiler, 'sections/vkey/same-target.xml', callbacks) as Vkey;
    assert.isNotNull(vkey);
    assert.equal(callbacks.messages.length, 1);
    assert.deepEqual(callbacks.messages[0], CompilerMessages.Info_MultipleVkeyMapsHaveSameTarget('Q'));
  });

  it('should error on invalid "from" vkey', function() {
    const callbacks = new CompilerCallbacks();
    let vkey = loadSectionFixture(VkeyCompiler, 'sections/vkey/invalid-from-vkey.xml', callbacks) as Vkey;
    assert.isNull(vkey);
    assert.equal(callbacks.messages.length, 2);
    assert.deepEqual(callbacks.messages[0], CompilerMessages.Error_VkeyIsNotValid('q'));
    assert.deepEqual(callbacks.messages[1], CompilerMessages.Error_VkeyIsNotValid('HYFEN'));
  });

  it('should error on invalid "to" vkey', function() {
    const callbacks = new CompilerCallbacks();
    let vkey = loadSectionFixture(VkeyCompiler, 'sections/vkey/invalid-to-vkey.xml', callbacks) as Vkey;
    assert.isNull(vkey);
    assert.equal(callbacks.messages.length, 1);
    assert.deepEqual(callbacks.messages[0], CompilerMessages.Error_VkeyIsNotValid('A-ACUTE'));
  });

  it('should error on repeated vkeys', function() {
    const callbacks = new CompilerCallbacks();
    let vkey = loadSectionFixture(VkeyCompiler, 'sections/vkey/invalid-repeated-vkey.xml', callbacks) as Vkey;
    assert.isNull(vkey);
    assert.equal(callbacks.messages.length, 1);
    assert.deepEqual(callbacks.messages[0], CompilerMessages.Error_VkeyMapIsRepeated('A'));
  });
});

