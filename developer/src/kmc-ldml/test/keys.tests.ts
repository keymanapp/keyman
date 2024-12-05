import 'mocha';
import { assert } from 'chai';
import { KeysCompiler } from '../src/compiler/keys.js';
import { assertCodePoints, compilerTestCallbacks, loadSectionFixture, testCompilationCases } from './helpers/index.js';
import { KMXPlus, Constants, LdmlKeyboardTypes } from '@keymanapp/common-types';
import { LdmlCompilerMessages } from '../src/compiler/ldml-compiler-messages.js';
import { constants } from '@keymanapp/ldml-keyboard-constants';
import { MetaCompiler } from '../src/compiler/meta.js';
const keysDependencies = [ ...BASIC_DEPENDENCIES, MetaCompiler ];
import Keys = KMXPlus.Keys;
import { BASIC_DEPENDENCIES } from '../src/compiler/empty-compiler.js';
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
        assert.equal(keys.keys.length, 2 + KeysCompiler.reserved_count); //
        assert.equal(keys.flicks.length, 1); // there's always a 'null' flick
        // ids are in sorted order in memory`
        assert.isTrue(keys.keys[0].to.isOneChar);
        assert.equal(keys.keys[0].to.value, String.fromCodePoint(0x1FAA6));
        assert.equal(keys.keys[0].flags, 0);
        assert.equal(keys.keys[0].id.value, 'grave');
        assert.equal(keys.keys[1].to.value, 'oops');
        assert.isFalse(keys.keys[1].to.isOneChar);
        assert.equal(keys.keys[1].flags, constants.keys_key_flags_extend);
        assert.equal(keys.keys[1].id.value, 'mistake');
      },
    },
    {
      subpath: 'sections/keys/maximal.xml',
      callback(sect) {
        const keys = <Keys> sect;
        assert.ok(keys);
        assert.equal(compilerTestCallbacks.messages.length, 0);
        assert.equal(keys.keys.length, 13 + KeysCompiler.reserved_count); // includes flick and gesture keys

        const [w] = keys.keys.filter(({ id }) => id.value === 'w');
        assert.ok(w);
        assert.equal(w.to.value, 'w', 'substituted key value');

        const [q] = keys.keys.filter(({ id }) => id.value === 'q');
        assert.ok(q);
        assert.isFalse(!!(q.flags & constants.keys_key_flags_gap));
        assert.equal(q.width, 32, 'q\'s width'); // ceil(3.14159 * 10.0)
        assert.equal(q.flicks, 'flick0'); // note this is a string, not a StrsItem
        assert.equal(q.longPress.toString(), 'a-acute e-acute i-acute');
        assert.equal(q.longPressDefault.value, 'e-acute');
        assert.equal(q.multiTap.toString(), 'a-umlaut e-umlaut i-umlaut');

        const [flick0] = keys.flicks.filter(({ id }) => id.value === 'flick0');
        assert.ok(flick0);
        assert.equal(flick0.flicks.length, 2);

        const [flick0_nw_se] = flick0.flicks.filter(({ directions }) => directions && directions.isEqual('nw se'.split(' ')));
        assert.ok(flick0_nw_se);
        assert.equal(flick0_nw_se.keyId?.value, 'c-cedilla');

        const [flick0_ne_sw] = flick0.flicks.filter(({ directions }) => directions && directions.isEqual('ne sw'.split(' ')));
        assert.ok(flick0_ne_sw);
        assert.equal(flick0_ne_sw.keyId?.value, 'e-caret'); // via variable

        // normalization w markers
        const [amarker] = keys.keys.filter(({ id }) => id.value === 'amarker');
        assertCodePoints(amarker.to.value, `a${LdmlKeyboardTypes.MarkerParser.markerOutput(1, false)}\u{0320}\u{0301}`);

        // normalization
        const [aacute] = keys.keys.filter(({ id }) => id.value === 'a-acute');
        assertCodePoints(aacute.to.value, 'a\u{0301}');
      },
    },
    {
      // same thing, but without normalization
      subpath: 'sections/keys/maximal-nfc.xml',
      callback(sect) {
        const keys = <Keys> sect;
        assert.ok(keys);

        assert.equal(keys.keys.length, 13 + KeysCompiler.reserved_count); // includes flick and gesture keys

        const [w] = keys.keys.filter(({ id }) => id.value === 'w');
        assert.ok(w);
        assert.equal(w.to.value, 'w', 'substituted key value');

        const [q] = keys.keys.filter(({ id }) => id.value === 'q');
        assert.ok(q);
        assert.isFalse(!!(q.flags & constants.keys_key_flags_gap));
        assert.equal(q.width, 32, 'q\'s width'); // ceil(3.14159 * 10.0)
        assert.equal(q.flicks, 'flick0'); // note this is a string, not a StrsItem
        assert.equal(q.longPress.toString(), 'a-acute e-acute i-acute');
        assert.equal(q.longPressDefault.value, 'e-acute');
        assert.equal(q.multiTap.toString(), 'a-umlaut e-umlaut i-umlaut');

        const [flick0] = keys.flicks.filter(({ id }) => id.value === 'flick0');
        assert.ok(flick0);
        assert.equal(flick0.flicks.length, 2);

        const [flick0_nw_se] = flick0.flicks.filter(({ directions }) => directions && directions.isEqual('nw se'.split(' ')));
        assert.ok(flick0_nw_se);
        assert.equal(flick0_nw_se.keyId?.value, 'c-cedilla');

        const [flick0_ne_sw] = flick0.flicks.filter(({ directions }) => directions && directions.isEqual('ne sw'.split(' ')));
        assert.ok(flick0_ne_sw);
        assert.equal(flick0_ne_sw.keyId?.value, 'e-caret'); // via variable

        // normalization w markers
        const [amarker] = keys.keys.filter(({ id }) => id.value === 'amarker');
        assertCodePoints(amarker.to.value, `á${LdmlKeyboardTypes.MarkerParser.markerOutput(1, false)}\u{0320}`);

        // normalization
        const [aacute] = keys.keys.filter(({ id }) => id.value === 'a-acute');
        assertCodePoints(aacute.to.value, 'á');

      },
      warnings: [
        LdmlCompilerMessages.Hint_NormalizationDisabled()
      ],
    },
    {
      subpath: 'sections/keys/escaped.xml',
      callback(sect) {
        const keys = <Keys> sect;
        assert.ok(keys);
        assert.equal(compilerTestCallbacks.messages.length, 0);
        assert.equal(keys.keys.length, 12 + KeysCompiler.reserved_count); // flick and gesture keys

        const [q] = keys.keys.filter(({ id }) => id.value === 'q');
        assert.ok(q);
        assert.isFalse(!!(q.flags & constants.keys_key_flags_gap));
        assert.equal(q.width, 32); // ceil(3.1 * 10)
        assert.equal(q.flicks, 'flick0'); // note this is a string, not a StrsItem
        assert.equal(q.longPress.toString(), 'a-acute e-acute i-acute');
        assert.equal(q.longPressDefault.value, 'e-acute');
        assert.equal(q.multiTap.toString(), 'a-umlaut e-umlaut i-umlaut');

        const [flick0] = keys.flicks.filter(({ id }) => id.value === 'flick0');
        assert.ok(flick0);
        assert.equal(flick0.flicks.length, 2);

        const [flick0_nw_se] = flick0.flicks.filter(({ directions }) => directions && directions.isEqual('nw se'.split(' ')));
        assert.ok(flick0_nw_se);
        assert.equal(flick0_nw_se.keyId?.value, 'c');

        const [flick0_ne_sw] = flick0.flicks.filter(({ directions }) => directions && directions.isEqual('ne sw'.split(' ')));
        assert.ok(flick0_ne_sw);
        assert.equal(flick0_ne_sw.keyId?.value, 'eee');
      },
    },
    {
      subpath: 'sections/keys/gap-switch.xml',
      callback(sect) {
        const keys = <Keys> sect;
        assert.equal(compilerTestCallbacks.messages.length, 0);
        assert.equal(keys.keys.length, 4 + KeysCompiler.reserved_count);

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
        assert.equal((<Keys>keys).keys.length, 1 + KeysCompiler.reserved_count);
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
        assert.equal(keys.keys.length, 5 + KeysCompiler.reserved_count);

        const ww = keys.keys.find(({ id }) => id.value === 'ww');
        assert.ok(ww);
        const MARKER_5 = LdmlKeyboardTypes.MarkerParser.markerOutput(5);
        assert.equal(ww.to.value, MARKER_5);
        assert.equal(ww.longPressDefault.value, 'bb');
        assert.equal(ww.longPress[0].value.value, 'aa');
        assert.equal(ww.multiTap[0].value.value, 'cc');
        const [flickw] = keys.flicks?.filter(({id}) => id.value === 'flickw');
        assert.ok(flickw);
        assert.equal(flickw.flicks[0].keyId.value, 'dd');
      },
    },
  ], keysDependencies);
});

describe('keys.kmap', function () {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should compile minimal kmap data', async function() {
    let keys = await loadSectionFixture(KeysCompiler, 'sections/keys/minimal.xml', compilerTestCallbacks, keysDependencies) as Keys;
    assert.isNotNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 0);
    // skip reserved (gap) keys
    assert.equal(keys.kmap.filter(({key}) => !/ /.test(key)).length, 2);
    assert.equal(keys.kmap.length, 48); // # of non-frame keys on US keyboard
  });

  testCompilationCases(KeysCompiler, [
    {
      subpath: 'sections/keys/hardware.xml',
      callback: (sect, subpath, callbacks) => {
        const keys = sect as Keys;
        assert.isNotNull(keys);
        assert.equal(compilerTestCallbacks.messages.length, 0);
        assert.equal(keys.keys.length, 4 + KeysCompiler.reserved_count);
        // skip reserved (gap) keys
        assert.sameDeepMembers(keys.kmap.filter(({key}) => !/ /.test(key)), [
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
        LdmlCompilerMessages.Error_InvalidModifier({layer:'base',modifiers:'altR-shift'}),
      ]
    },
    {
      subpath: 'sections/keys/invalid-missing-flick.xml',
      errors: [
        LdmlCompilerMessages.Error_MissingFlicks({flickId:'an-undefined-flick-id',id:'Q'}),
      ]
    },
    {
      subpath: 'sections/layr/invalid-invalid-form.xml',
      errors: [LdmlCompilerMessages.Error_InvalidHardware({
        formId: 'holographic',
      }),],
    },
    {
      // warning on custom form
      subpath: 'sections/layr/warn-custom-us-form.xml',
      warnings: [
        LdmlCompilerMessages.Warn_CustomForm({id: "us"}),
      ],
      callback: (sect, subpath, callbacks) => {
        const keys = sect as Keys;
        assert.isNotNull(keys);
        assert.equal(compilerTestCallbacks.messages.length, 0);
        assert.equal(keys.keys.length, 3 + KeysCompiler.reserved_count);
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
        LdmlCompilerMessages.Warn_CustomForm({id: "zzz"}),
      ],
      callback: (sect, subpath, callbacks) => {
        const keys = sect as Keys;
        assert.isNotNull(keys);
        assert.equal(compilerTestCallbacks.messages.length, 0);
        assert.equal(keys.keys.length, 3 + KeysCompiler.reserved_count);
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
        LdmlCompilerMessages.Warn_CustomForm({id: "us"}),
      ],
      errors: [
        LdmlCompilerMessages.Error_InvalidScanCode({ form: "us", codes: ['ff'] }),
      ],
    },
    {
      subpath: 'sections/layr/error-custom-zzz-form.xml',
      warnings: [
        LdmlCompilerMessages.Warn_CustomForm({id: "zzz"}),
      ],
      errors: [
        LdmlCompilerMessages.Error_InvalidScanCode({ form: "zzz", codes: ['ff'] }),
      ],
    },
    {
      subpath: 'sections/keys/invalid-undefined-var-1.xml',
      errors: [
        LdmlCompilerMessages.Error_MissingStringVariable({id: "varsok"}),
      ],
    },
    {
      subpath: 'sections/keys/invalid-undefined-var-1b.xml',
      errors: [
        LdmlCompilerMessages.Error_MissingStringVariable({id: "varsok"}),
      ],
    },
    // modifiers test
    {
      // keep in sync with similar test in test-layr.ts
      subpath: 'sections/keys/many-modifiers.xml',
      callback(sect) {
        const keys = <Keys> sect;
        assert.ok(keys);
        const { kmap } = keys;
        const aMods = kmap.filter(({ key }) => key === 'a').map(({ mod }) => mod);
        assert.sameDeepMembers(aMods, [
          constants.keys_mod_none,
        ], 'modifiers for a');
        const cMods = kmap.filter(({ key }) => key === 'c').map(({ mod }) => mod);
        assert.sameDeepMembers(cMods, [
          constants.keys_mod_altR,
          constants.keys_mod_ctrl | constants.keys_mod_shift,
        ], 'modifiers for c');
      },
    },
  ], keysDependencies);

  it('should reject layouts with too many hardware rows', async function() {
    let keys = await loadSectionFixture(KeysCompiler, 'sections/keys/invalid-hardware-too-many-rows.xml', compilerTestCallbacks, keysDependencies) as Keys;
    assert.isNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 1);

    assert.deepEqual(compilerTestCallbacks.messages[0], LdmlCompilerMessages.Error_HardwareLayerHasTooManyRows());
  });

  it('should reject layouts with too many hardware keys', async function() {
    let keys = await loadSectionFixture(KeysCompiler, 'sections/keys/invalid-hardware-too-many-keys.xml', compilerTestCallbacks, keysDependencies) as Keys;
    assert.isNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 1);

    assert.deepEqual(compilerTestCallbacks.messages[0], LdmlCompilerMessages.Error_RowOnHardwareLayerHasTooManyKeys({row: 1, hardware: 'us', modifiers: 'none'}));
  });

  it('should reject layouts with undefined keys', async function() {
    let keys = await loadSectionFixture(KeysCompiler, 'sections/keys/invalid-undefined-key.xml', compilerTestCallbacks, keysDependencies) as Keys;
    assert.isNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 1);

    assert.deepEqual(compilerTestCallbacks.messages[0], LdmlCompilerMessages.Error_KeyNotFoundInKeyBag({col: 1, form: 'hardware', keyId: 'foo', layer: 'base', row: 1}));
  });
  it('should reject layouts with invalid keys', async function() {
    let keys = await loadSectionFixture(KeysCompiler, 'sections/keys/invalid-key-missing-attrs.xml', compilerTestCallbacks, keysDependencies) as Keys;
    assert.isNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 1);
    assert.deepEqual(compilerTestCallbacks.messages[0], LdmlCompilerMessages.Error_KeyMissingToGapOrSwitch({keyId: 'Q'}));
  });
  it('should accept layouts with gap/switch keys', async function() {
    let keys = await loadSectionFixture(KeysCompiler, 'sections/keys/gap-switch.xml', compilerTestCallbacks, keysDependencies) as Keys;
    assert.isNotNull(keys);
    assert.equal(compilerTestCallbacks.messages.length, 0);
    assert.equal(keys.keys.length, 4 + KeysCompiler.reserved_count);
  });
});
