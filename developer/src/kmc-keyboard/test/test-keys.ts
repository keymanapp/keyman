import 'mocha';
import { assert } from 'chai';
import { KeysCompiler } from '../src/compiler/keys.js';
import { compilerTestCallbacks, loadSectionFixture } from './helpers/index.js';
import { Keys } from '../src/kmx/kmx-plus.js';
import { CompilerMessages } from '../src/compiler/messages.js';

describe('keys', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should compile minimal keys data', function() {
    let keys = loadSectionFixture(KeysCompiler, 'sections/keys/minimal.xml', compilerTestCallbacks) as Keys;
    assert.isNotNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 0);
    assert.equal(keys.keys.length, 1);
  });

  it('should compile a hardware layer', function() {
    let keys = loadSectionFixture(KeysCompiler, 'sections/keys/hardware.xml', compilerTestCallbacks) as Keys;
    assert.isNotNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 0);
    assert.equal(keys.keys.length, 2);
  });

  it('should reject structurally invalid layers', function() {
    let keys = loadSectionFixture(KeysCompiler, 'sections/keys/invalid-missing-layer.xml', compilerTestCallbacks) as Keys;
    assert.isNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 1);

    assert.deepEqual(compilerTestCallbacks.messages[0], CompilerMessages.Error_MustBeAtLeastOneLayerElement());
  });

  it('should reject layouts with too many hardware rows', function() {
    let keys = loadSectionFixture(KeysCompiler, 'sections/keys/invalid-hardware-too-many-rows.xml', compilerTestCallbacks) as Keys;
    assert.isNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 1);

    assert.deepEqual(compilerTestCallbacks.messages[0], CompilerMessages.Error_HardwareLayerHasTooManyRows());
  });

  it('should reject layouts with too many hardware keys', function() {
    let keys = loadSectionFixture(KeysCompiler, 'sections/keys/invalid-hardware-too-many-keys.xml', compilerTestCallbacks) as Keys;
    assert.isNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 1);

    assert.deepEqual(compilerTestCallbacks.messages[0], CompilerMessages.Error_RowOnHardwareLayerHasTooManyKeys({row: 1}));
  });

  it('should reject layouts with undefined keys', function() {
    let keys = loadSectionFixture(KeysCompiler, 'sections/keys/invalid-undefined-key.xml', compilerTestCallbacks) as Keys;
    assert.isNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 1);

    assert.deepEqual(compilerTestCallbacks.messages[0], CompilerMessages.Error_KeyNotFoundInKeyBag({col: 1, form: 'hardware', keyId: 'foo', layer: 'base', row: 1}));
  });
});
