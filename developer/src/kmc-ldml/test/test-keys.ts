import 'mocha';
import { assert } from 'chai';
import { KeysCompiler } from '../src/compiler/keys.js';
import { compilerTestCallbacks, loadSectionFixture, testCompilationCases } from './helpers/index.js';
import { KMXPlus, Constants, MarkerParser } from '@keymanapp/common-types';
import { CompilerMessages } from '../src/compiler/messages.js';
import { constants } from '@keymanapp/ldml-keyboard-constants';
import Keys = KMXPlus.Keys;
const K = Constants.USVirtualKeyCodes;

describe('keys', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while
  testCompilationCases(KeysCompiler, [
    {
      // should compile minimal keys data
      subpath: 'sections/keys/minimal.xml',
      callback(sect) {
        const keys = <Keys> sect;
        assert.ok(keys);
        assert.equal(compilerTestCallbacks.messages.length, 0);
        assert.equal(keys.keys.length, 2);
        assert.equal(keys.flicks.length, 1); // there's always a 'null' flick
        assert.equal(keys.keys[0].to.value, 'oops');
        assert.isFalse(keys.keys[0].to.isOneChar);
        assert.equal(keys.keys[0].flags, constants.keys_key_flags_extend);
        assert.equal(keys.keys[0].id.value, 'mistake');
        assert.isTrue(keys.keys[1].to.isOneChar);
        assert.equal(keys.keys[1].to.value, String.fromCodePoint(0x1FAA6));
        assert.equal(keys.keys[1].flags, 0);
        assert.equal(keys.keys[1].id.value, 'grave');
      },
    },
    {
      subpath: 'sections/keys/maximal.xml',
      callback(sect) {
        const keys = <Keys> sect;
        assert.ok(keys);
        assert.equal(compilerTestCallbacks.messages.length, 0);
        assert.equal(keys.keys.length, 4);

        const [w] = keys.keys.filter(({ id }) => id.value === 'w');
        assert.ok(w);
        assert.equal(w.to.value, 'w', 'substituted key value');

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
        assert.equal(flick0_nw_se.keyId?.value, 'ç');

        const [flick0_ne_sw] = flick0.flicks.filter(({ directions }) => directions && directions.isEqual('ne sw'.split(' ')));
        assert.ok(flick0_ne_sw);
        assert.equal(flick0_ne_sw.keyId?.value, 'ê'); // via variable
      },
    },
    {
      subpath: 'sections/keys/escaped.xml',
      callback(sect) {
        const keys = <Keys> sect;
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
        assert.equal(flick0_nw_se.keyId?.value, 'ç');

        const [flick0_ne_sw] = flick0.flicks.filter(({ directions }) => directions && directions.isEqual('ne sw'.split(' ')));
        assert.ok(flick0_ne_sw);
        assert.equal(flick0_ne_sw.keyId?.value, 'ế');
      },
    },
    {
      subpath: 'sections/keys/gap-switch.xml',
      callback(sect) {
        const keys = <Keys> sect;
        assert.equal(compilerTestCallbacks.messages.length, 0);
        assert.equal(keys.keys.length, 4);

        const [Qgap] = keys.keys.filter(({ id }) => id.value === 'Q');
        assert.ok(Qgap);
        assert.isTrue(!!(Qgap.flags & constants.keys_key_flags_gap), 'Q’s gap=');

        const [Wshift] = keys.keys.filter(({ id }) => id.value === 'W');
        assert.isNotNull(Wshift);
        assert.isFalse(!!(Wshift.flags & constants.keys_key_flags_gap));
        assert.equal(Wshift.switch.value, 'shift');
      },
    },
    {
      subpath: 'sections/keys/escaped2.xml',
      callback: (keys, subpath, callbacks) => {
        assert.isNotNull(keys);
        assert.equal((<Keys>keys).keys.length, 1);
        const [q] = (<Keys>keys).keys.filter(({ id }) => id.value === 'grave');
        assert.equal(q.to.value, String.fromCodePoint(0x1faa6));
      },
    },
    {
      subpath: 'sections/keys/markers.xml',
      callback(sect) {
        const keys = <Keys> sect;
        assert.ok(keys);
        assert.equal(compilerTestCallbacks.messages.length, 0);
        assert.equal(keys.keys.length, 1);

        const [ww] = keys.keys.filter(({ id }) => id.value === 'ww');
        assert.ok(ww);
        const MARKER_1 = MarkerParser.markerOutput(1);
        assert.equal(ww.to.value, MARKER_1);
        assert.equal(ww.longPressDefault.value, MARKER_1);
        assert.equal(ww.longPress[0].value.value, MARKER_1);
        assert.equal(ww.multiTap[0].value.value, MARKER_1);
        const [flickw] = keys.flicks?.filter(({id}) => id.value === 'flickw');
        assert.ok(flickw);
        assert.equal(flickw.flicks[0].keyId.value, MARKER_1);
      },
    },
  ]);
});

describe('keys.kmap', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should compile minimal kmap data', async function() {
    let keys = await loadSectionFixture(KeysCompiler, 'sections/keys/minimal.xml', compilerTestCallbacks) as Keys;
    assert.isNotNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 0);
    assert.equal(keys.kmap.length, 2);
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
        CompilerMessages.Error_InvalidModifier({layer:'base',modifiers:'altR-shift'}),
      ]
    },
    {
      subpath: 'sections/keys/invalid-missing-flick.xml',
      errors: [
        CompilerMessages.Error_MissingFlicks({flickId:'an-undefined-flick-id',id:'Q'}),
      ]
    },
    {
      subpath: 'sections/layr/invalid-invalid-form.xml',
      errors: [CompilerMessages.Error_InvalidHardware({
        form: 'holographic',
      }),],
    },
    {
      // warning on custom form
      subpath: 'sections/layr/warn-custom-us-form.xml',
      warnings: [
        CompilerMessages.Warn_CustomForm({id: "us"}),
      ],
      callback: (sect, subpath, callbacks) => {
        const keys = sect as Keys;
        assert.isNotNull(keys);
        assert.equal(compilerTestCallbacks.messages.length, 0);
        assert.equal(keys.keys.length, 3);
        assert.sameDeepMembers(keys.kmap, [
          {
            vkey: K.K_K,
            key: 'one',
            mod: constants.keys_mod_none,
          },
          {
            vkey: K.K_E,
            key: 'two',
            mod: constants.keys_mod_none,
          },
          {
            vkey: K.K_Y,
            key: 'three',
            mod: constants.keys_mod_none,
          },
        ]);
      },
    },
    {
      // warning on a custom unknown form - but no error!
      subpath: 'sections/layr/warn-custom-zzz-form.xml',
      warnings: [
        CompilerMessages.Warn_CustomForm({id: "zzz"}),
      ],
      callback: (sect, subpath, callbacks) => {
        const keys = sect as Keys;
        assert.isNotNull(keys);
        assert.equal(compilerTestCallbacks.messages.length, 0);
        assert.equal(keys.keys.length, 3);
        assert.sameDeepMembers(keys.kmap, [
          {
            vkey: K.K_K,
            key: 'one',
            mod: constants.keys_mod_none,
          },
          {
            vkey: K.K_E,
            key: 'two',
            mod: constants.keys_mod_none,
          },
          {
            vkey: K.K_Y,
            key: 'three',
            mod: constants.keys_mod_none,
          },
        ]);
      },
    },
    {
      subpath: 'sections/layr/error-custom-us-form.xml',
      warnings: [
        CompilerMessages.Warn_CustomForm({id: "us"}),
      ],
      errors: [
        CompilerMessages.Error_InvalidScanCode({ form: "us", codes: ['ff'] }),
      ],
    },
    {
      subpath: 'sections/layr/error-custom-zzz-form.xml',
      warnings: [
        CompilerMessages.Warn_CustomForm({id: "zzz"}),
      ],
      errors: [
        CompilerMessages.Error_InvalidScanCode({ form: "zzz", codes: ['ff'] }),
      ],
    },
  ]);

  it('should reject layouts with too many hardware rows', async function() {
    let keys = await loadSectionFixture(KeysCompiler, 'sections/keys/invalid-hardware-too-many-rows.xml', compilerTestCallbacks) as Keys;
    assert.isNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 1);

    assert.deepEqual(compilerTestCallbacks.messages[0], CompilerMessages.Error_HardwareLayerHasTooManyRows());
  });

  it('should reject layouts with too many hardware keys', async function() {
    let keys = await loadSectionFixture(KeysCompiler, 'sections/keys/invalid-hardware-too-many-keys.xml', compilerTestCallbacks) as Keys;
    assert.isNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 1);

    assert.deepEqual(compilerTestCallbacks.messages[0], CompilerMessages.Error_RowOnHardwareLayerHasTooManyKeys({row: 1, hardware: 'us', modifiers: 'none'}));
  });

  it('should reject layouts with undefined keys', async function() {
    let keys = await loadSectionFixture(KeysCompiler, 'sections/keys/invalid-undefined-key.xml', compilerTestCallbacks) as Keys;
    assert.isNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 1);

    assert.deepEqual(compilerTestCallbacks.messages[0], CompilerMessages.Error_KeyNotFoundInKeyBag({col: 1, form: 'hardware', keyId: 'foo', layer: 'base', row: 1}));
  });
  it('should reject layouts with invalid keys', async function() {
    let keys = await loadSectionFixture(KeysCompiler, 'sections/keys/invalid-key-missing-attrs.xml', compilerTestCallbacks) as Keys;
    assert.isNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 1);
    assert.deepEqual(compilerTestCallbacks.messages[0], CompilerMessages.Error_KeyMissingToGapOrSwitch({keyId: 'Q'}));
  });
  it('should accept layouts with gap/switch keys', async function() {
    let keys = await loadSectionFixture(KeysCompiler, 'sections/keys/gap-switch.xml', compilerTestCallbacks) as Keys;
    assert.isNotNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 0);
    assert.equal(keys.keys.length, 4);
  });
});
