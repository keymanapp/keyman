import 'mocha';
import { assert } from 'chai';
import { KeysCompiler } from '../src/compiler/keys.js';
import { compilerTestCallbacks, loadSectionFixture } from './helpers/index.js';
import { KMXPlus } from '@keymanapp/common-types';
import { CompilerMessages } from '../src/compiler/messages.js';

import Keys = KMXPlus.Keys;
import { constants } from '@keymanapp/ldml-keyboard-constants';

describe('keys', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should compile minimal keys data', function() {
    let keys = loadSectionFixture(KeysCompiler, 'sections/keys/minimal.xml', compilerTestCallbacks) as Keys;
    assert.isNotNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 0);
    assert.equal(keys.keys.length, 1);
  });

  it('should compile escaped keys data', function() {
    let keys = loadSectionFixture(KeysCompiler, 'sections/keys/escaped.xml', compilerTestCallbacks) as Keys;
    assert.isNotNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 0);
    assert.equal(keys.keys.length, 1);
    assert.equal(keys.keys[0].to.value, String.fromCodePoint(0x1faa6));
  });

  it('should compile a hardware layer', function() {
    let keys = loadSectionFixture(KeysCompiler, 'sections/keys/hardware.xml', compilerTestCallbacks) as Keys;
    assert.isNotNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 0);
    assert.equal(keys.keys.length, 4);
    assert.sameDeepMembers(keys.keys.map(({vkey, to, mod}) => ({vkey, to: to.value, mod})), [
      {
        vkey: 192,
        to: 'qqq',
        mod: constants.keys_mod_none,
      },
      {
        vkey: '1'.charCodeAt(0),
        to: 'www',
        mod: constants.keys_mod_none,
      },
      {
        vkey: 192,
        to: 'QQQ',
        mod: constants.keys_mod_shift,
      },
      {
        vkey: '1'.charCodeAt(0),
        to: 'WWW',
        mod: constants.keys_mod_shift,
      },
    ]);
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
  it('should reject layouts with invalid keys', function() {
    let keys = loadSectionFixture(KeysCompiler, 'sections/keys/invalid-key-missing-attrs.xml', compilerTestCallbacks) as Keys;
    assert.isNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 1);
    assert.deepEqual(compilerTestCallbacks.messages[0], CompilerMessages.Error_KeyMissingToGapOrSwitch({keyId: 'Q'}));
  });
  it('should accept layouts with gap/switch keys', function() {
    let keys = loadSectionFixture(KeysCompiler, 'sections/keys/gap-switch.xml', compilerTestCallbacks) as Keys;
    assert.isNotNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 0);
    assert.equal(keys.keys.length, 4);
  });
  // TODO-LDML:  <modifier="altR-shift" should throw a message
});
