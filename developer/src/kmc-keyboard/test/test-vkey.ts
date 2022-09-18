import 'mocha';
import { assert } from 'chai';
import { VkeyCompiler } from '../src/compiler/vkey.js';
import { CompilerCallbacks, loadSectionFixture } from './helpers/index.js';
import { Vkey } from '../src/kmx/kmx-plus.js';
import { CompilerMessages } from '../src/compiler/messages.js';
import { USVirtualKeyCodes } from '../src/ldml-keyboard/virtual-key-constants.js';

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
    assert.deepEqual(callbacks.messages[0], CompilerMessages.Hint_VkeyMapIsRedundant({vkey: 'A'}));
  });

  it('should report an info message if same target found', function() {
    const callbacks = new CompilerCallbacks();
    let vkey = loadSectionFixture(VkeyCompiler, 'sections/vkey/same-target.xml', callbacks) as Vkey;
    assert.isNotNull(vkey);
    assert.equal(callbacks.messages.length, 1);
    assert.deepEqual(callbacks.messages[0], CompilerMessages.Info_MultipleVkeyMapsHaveSameTarget({vkey: 'Q'}));
  });

  it('should error on invalid "from" vkey', function() {
    const callbacks = new CompilerCallbacks();
    let vkey = loadSectionFixture(VkeyCompiler, 'sections/vkey/invalid-from-vkey.xml', callbacks) as Vkey;
    assert.isNull(vkey);
    assert.equal(callbacks.messages.length, 2);
    assert.deepEqual(callbacks.messages[0], CompilerMessages.Error_VkeyIsNotValid({vkey: 'q'}));
    assert.deepEqual(callbacks.messages[1], CompilerMessages.Error_VkeyIsNotValid({vkey: 'HYFEN'}));
  });

  it('should error on invalid "to" vkey', function() {
    const callbacks = new CompilerCallbacks();
    let vkey = loadSectionFixture(VkeyCompiler, 'sections/vkey/invalid-to-vkey.xml', callbacks) as Vkey;
    assert.isNull(vkey);
    assert.equal(callbacks.messages.length, 1);
    assert.deepEqual(callbacks.messages[0], CompilerMessages.Error_VkeyIsNotValid({vkey: 'A-ACUTE'}));
  });

  it('should error on repeated vkeys', function() {
    const callbacks = new CompilerCallbacks();
    let vkey = loadSectionFixture(VkeyCompiler, 'sections/vkey/invalid-repeated-vkey.xml', callbacks) as Vkey;
    assert.isNull(vkey);
    assert.equal(callbacks.messages.length, 1);
    assert.deepEqual(callbacks.messages[0], CompilerMessages.Error_VkeyMapIsRepeated({vkey: 'A'}));
  });
});

