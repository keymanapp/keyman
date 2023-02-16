import 'mocha';
import { assert } from 'chai';
import { Key2Compiler } from '../src/compiler/key2.js';
import { compilerTestCallbacks, loadSectionFixture, testCompilationCases } from './helpers/index.js';
import { KMXPlus, Constants } from '@keymanapp/common-types';
import { CompilerMessages } from '../src/compiler/messages.js';

const K = Constants.USVirtualKeyCodes;

import Key2 = KMXPlus.Key2;
import { constants } from '@keymanapp/ldml-keyboard-constants';

// Note: keeping the test cases, but they now apply to the subtable
describe('key2.kmap', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should compile minimal kmap data', function() {
    let key2 = loadSectionFixture(Key2Compiler, 'sections/keys/minimal.xml', compilerTestCallbacks) as Key2;
    assert.isNotNull(key2);
    assert.equal(compilerTestCallbacks.messages.length, 0);
    assert.equal(key2.kmap.length, 1);
  });

  testCompilationCases(Key2Compiler, [
    {
      subpath: 'sections/keys/hardware.xml',
      callback: (sect, subpath, callbacks) => {
        const keys = sect as Key2;
        assert.isNotNull(keys);
        assert.equal(compilerTestCallbacks.messages.length, 0);
        assert.equal(keys.keys.length, 4);
        assert.sameDeepMembers(keys.kmap, [
          {
            vkey: K.K_BKQUOTE,
            key: 'qqq',
            mod: constants.keys_mod_none,
          },
          {
            vkey: K.K_1,
            key: 'www',
            mod: constants.keys_mod_none,
          },
          {
            vkey: K.K_BKQUOTE,
            key: 'QQQ',
            mod: constants.keys_mod_shift,
          },
          {
            vkey: K.K_1,
            key: 'WWW',
            mod: constants.keys_mod_shift,
          },
        ]);
      },
    },
    {
      subpath: 'sections/keys/hardware_us.xml',
      callback: (sect, subpath, callbacks) => {
        const keys = sect as Key2;
        assert.isNotNull(keys);
        assert.includeDeepMembers(keys.kmap, [
          {
            vkey: K.K_BKSLASH,
            key: 'backslash',
            mod: constants.keys_mod_none,
          },
          {
            vkey: K.K_Z,
            key: 'z',
            mod: constants.keys_mod_none,
          },
          {
            vkey: K.K_BKQUOTE,
            key: 'grave',
            mod: constants.keys_mod_none,
          },
        ]);
      },
    },
    {
      subpath: 'sections/keys/hardware_iso.xml',
      callback: (sect, subpath, callbacks) => {
        const keys = sect as Key2;
        assert.isNotNull(keys);
        assert.includeDeepMembers(keys.kmap, [
          {
            vkey: K.K_oE2,
            key: 'backslash',
            mod: constants.keys_mod_none,
          },
          {
            vkey: 'Z'.charCodeAt(0),
            key: 'z',
            mod: constants.keys_mod_none,
          },
          {
            vkey: 192,
            key: 'grave',
            mod: constants.keys_mod_none,
          },
        ]);
      },
    },
    {
      subpath: 'sections/keys/invalid-bad-modifier.xml',
      errors: [
        CompilerMessages.Error_InvalidModifier({layer:'base',modifier:'altR-shift'}),
      ]
    },
  ]);

  it('should reject structurally invalid layers', function() {
    let keys = loadSectionFixture(Key2Compiler, 'sections/keys/invalid-missing-layer.xml', compilerTestCallbacks) as Key2;
    assert.isNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 1);

    assert.deepEqual(compilerTestCallbacks.messages[0], CompilerMessages.Error_MustBeAtLeastOneLayerElement());
  });

  it('should reject layouts with too many hardware rows', function() {
    let keys = loadSectionFixture(Key2Compiler, 'sections/keys/invalid-hardware-too-many-rows.xml', compilerTestCallbacks) as Key2;
    assert.isNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 1);

    assert.deepEqual(compilerTestCallbacks.messages[0], CompilerMessages.Error_HardwareLayerHasTooManyRows());
  });

  it('should reject layouts with too many hardware keys', function() {
    let keys = loadSectionFixture(Key2Compiler, 'sections/keys/invalid-hardware-too-many-keys.xml', compilerTestCallbacks) as Key2;
    assert.isNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 1);

    assert.deepEqual(compilerTestCallbacks.messages[0], CompilerMessages.Error_RowOnHardwareLayerHasTooManyKeys({row: 1, hardware: 'us'}));
  });

  it('should reject layouts with undefined keys', function() {
    let keys = loadSectionFixture(Key2Compiler, 'sections/keys/invalid-undefined-key.xml', compilerTestCallbacks) as Key2;
    assert.isNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 1);

    assert.deepEqual(compilerTestCallbacks.messages[0], CompilerMessages.Error_KeyNotFoundInKeyBag({col: 1, form: 'hardware', keyId: 'foo', layer: 'base', row: 1}));
  });
  it('should reject layouts with invalid keys', function() {
    let keys = loadSectionFixture(Key2Compiler, 'sections/keys/invalid-key-missing-attrs.xml', compilerTestCallbacks) as Key2;
    assert.isNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 1);
    assert.deepEqual(compilerTestCallbacks.messages[0], CompilerMessages.Error_KeyMissingToGapOrSwitch({keyId: 'Q'}));
  });
  it('should accept layouts with gap/switch keys', function() {
    let keys = loadSectionFixture(Key2Compiler, 'sections/keys/gap-switch.xml', compilerTestCallbacks) as Key2;
    assert.isNotNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 0);
    assert.equal(keys.keys.length, 4);
  });
});
