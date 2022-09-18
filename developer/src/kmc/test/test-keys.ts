import 'mocha';
import { assert } from 'chai';
import { KeysCompiler } from '../src/keyman/compiler/keys';
import { CompilerCallbacks, loadSectionFixture } from './helpers';
import { Keys } from '../src/keyman/kmx/kmx-plus';
import { CompilerMessages } from '../src/keyman/compiler/messages';

describe('keys', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should compile minimal keys data', function() {
    const callbacks = new CompilerCallbacks();
    let keys = loadSectionFixture(KeysCompiler, 'sections/keys/minimal.xml', callbacks) as Keys;
    assert.isNotNull(keys);
    assert.equal(callbacks.messages.length, 0);
    assert.equal(keys.keys.length, 1);
  });

  it('should compile a hardware layer', function() {
    const callbacks = new CompilerCallbacks();
    let keys = loadSectionFixture(KeysCompiler, 'sections/keys/hardware.xml', callbacks) as Keys;
    assert.isNotNull(keys);
    assert.equal(callbacks.messages.length, 0);
    assert.equal(keys.keys.length, 2);
  });

  it('should reject structurally invalid layers', function() {
    const callbacks = new CompilerCallbacks();
    let keys = loadSectionFixture(KeysCompiler, 'sections/keys/invalid-missing-layer.xml', callbacks) as Keys;
    assert.isNull(keys);
    assert.equal(callbacks.messages.length, 1);

    assert.deepEqual(callbacks.messages[0], CompilerMessages.Error_MustBeAtLeastOneLayerElement());
  });

  it('should reject layouts with too many hardware rows', function() {
    const callbacks = new CompilerCallbacks();
    let keys = loadSectionFixture(KeysCompiler, 'sections/keys/invalid-hardware-too-many-rows.xml', callbacks) as Keys;
    assert.isNull(keys);
    assert.equal(callbacks.messages.length, 1);

    assert.deepEqual(callbacks.messages[0], CompilerMessages.Error_HardwareLayerHasTooManyRows());
  });

  it('should reject layouts with too many hardware keys', function() {
    const callbacks = new CompilerCallbacks();
    let keys = loadSectionFixture(KeysCompiler, 'sections/keys/invalid-hardware-too-many-keys.xml', callbacks) as Keys;
    assert.isNull(keys);
    assert.equal(callbacks.messages.length, 1);

    assert.deepEqual(callbacks.messages[0], CompilerMessages.Error_RowOnHardwareLayerHasTooManyKeys({row: 1}));
  });

  it('should reject layouts with undefined keys', function() {
    const callbacks = new CompilerCallbacks();
    let keys = loadSectionFixture(KeysCompiler, 'sections/keys/invalid-undefined-key.xml', callbacks) as Keys;
    assert.isNull(keys);
    assert.equal(callbacks.messages.length, 1);

    assert.deepEqual(callbacks.messages[0], CompilerMessages.Error_KeyNotFoundInKeyBag({col: 1, form: 'hardware', keyId: 'foo', layer: 'base', row: 1}));
  });
});
