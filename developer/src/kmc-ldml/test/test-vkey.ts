import 'mocha';
import { assert } from 'chai';
import { VkeyCompiler } from '../src/compiler/vkey.js';
import { compilerTestCallbacks, loadSectionFixture } from './helpers/index.js';
import { KMXPlus, Constants } from '@keymanapp/common-types';
import { CompilerMessages } from '../src/compiler/messages.js';

import Vkey = KMXPlus.Vkey;
import USVirtualKeyCodes = Constants.USVirtualKeyCodes;

describe('vkey compiler', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should compile minimal vkey data', async function() {
    let vkey = await loadSectionFixture(VkeyCompiler, 'sections/vkey/minimal.xml', compilerTestCallbacks) as Vkey;
    assert.equal(compilerTestCallbacks.messages.length, 0);

    assert.equal(vkey.vkeys.length, 4);
    // Note, final order is sorted by `vkey` member
    assert.deepEqual(vkey.vkeys[0], {vkey: USVirtualKeyCodes.K_A, target: USVirtualKeyCodes.K_Q});
    assert.deepEqual(vkey.vkeys[1], {vkey: USVirtualKeyCodes.K_Q, target: USVirtualKeyCodes.K_A});
    assert.deepEqual(vkey.vkeys[2], {vkey: USVirtualKeyCodes.K_W, target: USVirtualKeyCodes.K_Z});
    assert.deepEqual(vkey.vkeys[3], {vkey: USVirtualKeyCodes.K_Z, target: USVirtualKeyCodes.K_W});
  });

  it('should hint on redundant data', async function() {
    let vkey = await loadSectionFixture(VkeyCompiler, 'sections/vkey/redundant.xml', compilerTestCallbacks) as Vkey;
    assert.isNotNull(vkey);
    assert.equal(compilerTestCallbacks.messages.length, 1);
    assert.deepEqual(compilerTestCallbacks.messages[0], CompilerMessages.Hint_VkeyIsRedundant({vkey: 'A'}));
  });

  it('should report an info message if same target found', async function() {
    let vkey = await loadSectionFixture(VkeyCompiler, 'sections/vkey/same-target.xml', compilerTestCallbacks) as Vkey;
    assert.isNotNull(vkey);
    assert.equal(compilerTestCallbacks.messages.length, 1);
    assert.deepEqual(compilerTestCallbacks.messages[0], CompilerMessages.Info_MultipleVkeysHaveSameTarget({vkey: 'Q'}));
  });

  it('should hint on invalid "from" vkey', async function() {
    let vkey = await loadSectionFixture(VkeyCompiler, 'sections/vkey/invalid-from-vkey.xml', compilerTestCallbacks) as Vkey;
    assert.isNotNull(vkey);
    assert.equal(compilerTestCallbacks.messages.length, 2);
    assert.deepEqual(compilerTestCallbacks.messages[0], CompilerMessages.Hint_VkeyIsNotValid({vkey: 'q'}));
    assert.deepEqual(compilerTestCallbacks.messages[1], CompilerMessages.Hint_VkeyIsNotValid({vkey: 'HYFEN'}));
  });

  it('should hint on invalid "to" vkey', async function() {
    let vkey = await loadSectionFixture(VkeyCompiler, 'sections/vkey/invalid-to-vkey.xml', compilerTestCallbacks) as Vkey;
    assert.isNotNull(vkey);
    assert.equal(compilerTestCallbacks.messages.length, 1);
    assert.deepEqual(compilerTestCallbacks.messages[0], CompilerMessages.Hint_VkeyIsNotValid({vkey: 'A-ACUTE'}));
  });

  it('should error on repeated vkeys', async function() {
    let vkey = await loadSectionFixture(VkeyCompiler, 'sections/vkey/invalid-repeated-vkey.xml', compilerTestCallbacks) as Vkey;
    assert.isNull(vkey);
    assert.equal(compilerTestCallbacks.messages.length, 1);
    assert.deepEqual(compilerTestCallbacks.messages[0], CompilerMessages.Error_VkeyIsRepeated({vkey: 'A'}));
  });
});

