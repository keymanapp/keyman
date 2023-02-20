import 'mocha';
import { assert } from 'chai';
import { KeysCompiler } from '../src/compiler/keys.js';
import { compilerTestCallbacks, loadSectionFixture, testCompilationCases } from './helpers/index.js';
import { KMXPlus, Constants } from '@keymanapp/common-types';
import { CompilerMessages } from '../src/compiler/messages.js';
import { constants } from '@keymanapp/ldml-keyboard-constants';
import Keys = KMXPlus.Keys;
const K = Constants.USVirtualKeyCodes;

describe('keys', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should compile minimal keys data', function () {
    let keys = loadSectionFixture(KeysCompiler, 'sections/keys/minimal.xml', compilerTestCallbacks) as Keys;
    assert.ok(keys);
    assert.equal(compilerTestCallbacks.messages.length, 0);
    assert.equal(keys.keys.length, 1);
    assert.equal(keys.flicks.length, 1); // there's always a 'null' flick
    assert.equal(keys.keys[0].to.value, '🪦');
    assert.equal(keys.keys[0].id.value, 'grave');
  });

  it('should compile maximal keys data', function () {
    let keys = loadSectionFixture(KeysCompiler, 'sections/keys/maximal.xml', compilerTestCallbacks) as Keys;
    assert.ok(keys);
    assert.equal(compilerTestCallbacks.messages.length, 0);
    assert.equal(keys.keys.length, 4);

    const [q] = keys.keys.filter(({ id }) => id.value === 'q');
    assert.ok(q);
    assert.isFalse(!!(q.flags & constants.keys_key_flags_gap));
    assert.equal(q.width, 32, 'q\'s width'); // ceil(3.14159 * 10.0)
    assert.equal(q.flicks, 'flick0'); // note this is a string, not a StrsItem
    assert.equal(q.longPress.toString(), 'á é í');
    assert.equal(q.longPressDefault.value, 'é');
    assert.equal(q.multiTap.toString(), 'ä ë ï');

    const [flick0] = keys.flicks.filter(({ id }) => id.value === 'flick0');
    assert.ok(flick0);
    assert.equal(flick0.flicks.length, 2);

    const [flick0_nw_se] = flick0.flicks.filter(({ directions }) => directions && directions.isEqual('nw se'.split(' ')));
    assert.ok(flick0_nw_se);
    assert.equal(flick0_nw_se.to?.value, 'ç');

    const [flick0_ne_sw] = flick0.flicks.filter(({ directions }) => directions && directions.isEqual('ne sw'.split(' ')));
    assert.ok(flick0_ne_sw);
    assert.equal(flick0_ne_sw.to?.value, 'ê');
  });

  it('should compile escaped keys data', function () {
    let keys = loadSectionFixture(KeysCompiler, 'sections/keys/escaped.xml', compilerTestCallbacks) as Keys;
    assert.ok(keys);
    assert.equal(compilerTestCallbacks.messages.length, 0);
    assert.equal(keys.keys.length, 4);

    const [q] = keys.keys.filter(({ id }) => id.value === 'q');
    assert.ok(q);
    assert.isFalse(!!(q.flags & constants.keys_key_flags_gap));
    assert.equal(q.width, 32); // ceil(3.1 * 10)
    assert.equal(q.flicks, 'flick0'); // note this is a string, not a StrsItem
    assert.equal(q.longPress.toString(), 'á é í');
    assert.equal(q.longPressDefault.value, 'é');
    assert.equal(q.multiTap.toString(), 'ä ë ï');

    const [flick0] = keys.flicks.filter(({ id }) => id.value === 'flick0');
    assert.ok(flick0);
    assert.equal(flick0.flicks.length, 2);

    const [flick0_nw_se] = flick0.flicks.filter(({ directions }) => directions && directions.isEqual('nw se'.split(' ')));
    assert.ok(flick0_nw_se);
    assert.equal(flick0_nw_se.to?.value, 'ç');

    const [flick0_ne_sw] = flick0.flicks.filter(({ directions }) => directions && directions.isEqual('ne sw'.split(' ')));
    assert.ok(flick0_ne_sw);
    assert.equal(flick0_ne_sw.to?.value, 'ế');
  });


  it('should accept layouts with gap/switch keys', function () {
    let keys = loadSectionFixture(KeysCompiler, 'sections/keys/gap-switch.xml', compilerTestCallbacks) as Keys;
    assert.ok(keys);
    assert.equal(compilerTestCallbacks.messages.length, 0);
    assert.equal(keys.keys.length, 4);

    const [Qgap] = keys.keys.filter(({ id }) => id.value === 'Q');
    assert.ok(Qgap);
    assert.isTrue(!!(Qgap.flags & constants.keys_key_flags_gap), 'Q’s gap=');

    const [Wshift] = keys.keys.filter(({ id }) => id.value === 'W');
    assert.isNotNull(Wshift);
    assert.isFalse(!!(Wshift.flags & constants.keys_key_flags_gap));
    assert.equal(Wshift.switch.value, 'shift');

  });

  testCompilationCases(KeysCompiler, [
    {
      subpath: 'sections/keys/escaped2.xml',
      callback: (keys, subpath, callbacks) => {
        assert.isNotNull(keys);
        assert.equal((<Keys>keys).keys.length, 1);
        const [q] = (<Keys>keys).keys.filter(({ id }) => id.value === 'grave');
        assert.equal(q.to.value, String.fromCodePoint(0x1faa6));
      },
    },
  ]);
});

describe('keys.kmap', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should compile minimal kmap data', function() {
    let keys = loadSectionFixture(KeysCompiler, 'sections/keys/minimal.xml', compilerTestCallbacks) as Keys;
    assert.isNotNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 0);
    assert.equal(keys.kmap.length, 1);
  });

  testCompilationCases(KeysCompiler, [
    {
      subpath: 'sections/keys/hardware.xml',
      callback: (sect, subpath, callbacks) => {
        const keys = sect as Keys;
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
        const keys = sect as Keys;
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
        const keys = sect as Keys;
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

    assert.deepEqual(compilerTestCallbacks.messages[0], CompilerMessages.Error_RowOnHardwareLayerHasTooManyKeys({row: 1, hardware: 'us'}));
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
});
