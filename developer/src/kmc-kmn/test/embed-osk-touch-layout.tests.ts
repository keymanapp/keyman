/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import 'mocha';
import { assert } from 'chai';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { KMXPlus, LdmlKeyboardTypes, TouchLayout } from '@keymanapp/common-types';
import { EmbedOskTouchLayoutInKmx } from '../src/compiler/embed-osk/embed-osk-touch-layout.js';
import { KMXPlusVersion } from '@keymanapp/ldml-keyboard-constants';

const Q_KEY: TouchLayout.TouchLayoutKey = {
  "id": "K_Q",
  "text": "q",
  "hint": "1",
  "width": 80,
  "pad": 20,
  "nextlayer": "shift",
  "multitap": [
    {
      "id": "U_1235",
      // TODO-EMBED-OSK-IN-KMX: consider adding '"layer": "custom"' for verifying layer overrides
    }
  ],
  "sk": [
    {
      "id": "K_ENTER",
      "text": "*Enter*"
    }
  ],
  "flick": {
    "s": {
      "text": "!", // not '1', to avoid false matches
      "id": "K_1"
    }
  }
};

// TODO-EMBED-OSK-IN-KMX:
// const SP_KEYS: TouchLayout.TouchLayoutKey[] = [
//   { "id": "K_A", "text": "special", "sp": TouchLayout.TouchLayoutKeySp.special, "width": 100, },
//   { "id": "K_B", "text": "specialActive", "sp": TouchLayout.TouchLayoutKeySp.specialActive, "width": 100, },
//   { "id": "K_C", "text": "customSpecial", "sp": TouchLayout.TouchLayoutKeySp.customSpecial, "width": 100, },
//   { "id": "K_D", "text": "customSpecialActive", "sp": TouchLayout.TouchLayoutKeySp.customSpecialActive, "width": 100, },
//   { "id": "K_E", "text": "deadkey", "sp": TouchLayout.TouchLayoutKeySp.deadkey, "width": 100, },
//   { "id": "K_F", "text": "blank", "sp": TouchLayout.TouchLayoutKeySp.blank, "width": 100, },
//   { "id": "K_G", "text": "spacer", "sp": TouchLayout.TouchLayoutKeySp.spacer, "width": 100, },
// ];

describe('Compiler OSK Embedding', function() {
  let kmxplus: KMXPlus.KMXPlusData = null;
  let kmxfile: KMXPlus.KMXPlusFile = null;
  const callbacks = new TestCompilerCallbacks();
  let embedder: EmbedOskTouchLayoutInKmx = null;

  this.beforeEach(function() {
    callbacks.clear();

    kmxfile = KMXPlus.KMXPlusFile.createEmptyMinimalKMXPlusFile(KMXPlusVersion.Version19);

    assert.isNotNull(kmxfile);
    kmxplus = kmxfile.kmxplus;

    embedder = new EmbedOskTouchLayoutInKmx(callbacks);
  });

  this.afterEach(function() {
    if(this.currentTest.isFailed()) {
      callbacks.printMessages();
    }
    kmxfile = null;
    kmxplus = null;
  });

  describe('EmbedOskTouchLayoutInKmx', function() {

    describe('EmbedOskTouchLayoutInKmx.keyFromTouchLayoutKey', function() {
      it('should convert a touch layout key into a KMX+ key', async function() {
        const kmxRow = new KMXPlus.LayrRow();
        embedder.unitTestEndpoints.addKeyFromTouchLayoutKey(kmxplus, kmxRow, { id: 'default', row: [] }, Q_KEY);
        assert.lengthOf(callbacks.messages, 0);

        // --- kmxplus ---

        assert.hasAllKeys(kmxplus,
          ['elem', 'disp', 'keys', 'layr', 'list', 'loca', 'meta', 'strs']
        );

        // these should all be defined by addKeyFromTouchLayoutKey or initialization
        assert.isObject(kmxplus.elem);
        assert.isObject(kmxplus.disp);
        assert.isObject(kmxplus.keys);
        assert.isObject(kmxplus.layr);
        assert.isObject(kmxplus.list);
        assert.isObject(kmxplus.loca);
        assert.isObject(kmxplus.meta);
        assert.isObject(kmxplus.strs);

        // --- kmxplus.strs ---

        assert.lengthOf(kmxplus.strs.strings, 8);
        assert.equal(kmxplus.strs.strings[0].value, '');                  // null string, always defined
        assert.equal(kmxplus.strs.strings[1].value, 'default-K_Q+1');     // id of Q key
        assert.equal(kmxplus.strs.strings[2].value, 's');                 // south-flick direction
        assert.equal(kmxplus.strs.strings[3].value, 'default-K_1+2') ;    // flick id
        assert.equal(kmxplus.strs.strings[4].value, 'default-K_ENTER+3'); // longpress id
        assert.equal(kmxplus.strs.strings[5].value, 'default-U_1235+4');  // multitap id
        assert.equal(kmxplus.strs.strings[6].value, 'shift');             // nextlayer on Q key
        assert.equal(kmxplus.strs.strings[7].value, '1');                 // hint on Q key

        // --- kmxplus.keys ---

        // There should be 4 keys: K_Q, U_1235, K_ENTER, and K_1
        // TODO-EMBED-OSK-IN-KMX: we need a gap key to test pad=20
        assert.sameDeepMembers(kmxplus.keys.keys, [
          { // K_1
            flags: 0,
            flicks: null,
            id: kmxplus.strs.strings[3],
            longPress: null,
            longPressDefault: kmxplus.strs.strings[0],
            multiTap: null,
            switch: kmxplus.strs.strings[0],
            to: new KMXPlus.CharStrsItem('!'),
            width: 100
          },
          { // K_ENTER
            flags: KMXPlus.KeysKeysFlags.extend,
            flicks: null,
            id: kmxplus.strs.strings[4],
            longPress: null,
            longPressDefault: kmxplus.strs.strings[0],
            multiTap: null,
            switch: kmxplus.strs.strings[0],
            to: kmxplus.strs.strings[0],
            width: 100
          },
          { // U_1235
            flags: 0,
            flicks: null,
            id: kmxplus.strs.strings[5],
            longPress: null,
            longPressDefault: kmxplus.strs.strings[0],
            multiTap: null,
            switch: kmxplus.strs.strings[0],
            to: new KMXPlus.CharStrsItem('ስ'),
            width: 100
          },
          { // K_Q
            flags: 0,
            flicks: "default-K_Q+1",
            id: kmxplus.strs.strings[1],
            longPress: kmxplus.list.lists[2],
            longPressDefault: kmxplus.strs.strings[4],
            multiTap: kmxplus.list.lists[3],
            switch: kmxplus.strs.strings[6],
            to: new KMXPlus.CharStrsItem('q'),
            width: 80
          },
        ]);

        assert.lengthOf(kmxplus.keys.flicks, 2);

        // null flick
        assert.lengthOf(kmxplus.keys.flicks[0].flicks, 0);
        assert.equal(kmxplus.keys.flicks[0].id, kmxplus.strs.strings[0]);

        // south-flick K_1 (phone-default-K_Q)
        assert.lengthOf(kmxplus.keys.flicks[1].flicks, 1);
        assert.equal(kmxplus.keys.flicks[1].id, kmxplus.strs.strings[1]);
        assert.equal(kmxplus.keys.flicks[1].flicks[0].directions, kmxplus.list.lists[1]);
        assert.equal(kmxplus.keys.flicks[1].flicks[0].keyId, kmxplus.strs.strings[3]); // default-K_1+2

        assert.isArray(kmxplus.keys.kmap);
        assert.isEmpty(kmxplus.keys.kmap);

        // --- kmxplus.disp ---

        assert.sameDeepMembers(kmxplus.disp.disps, [
          { // K_ENTER key cap
            display: kmxplus.strs.strings[0],
            flags: KMXPlus.DispItemFlags.hintPrimary | KMXPlus.DispItemFlags.isId | KMXPlus.DispItemFlags.keyCapEnter,
            id: null,
            to: null,
            toId: kmxplus.strs.strings[4],
          },
          { // phone-default-K_Q hint
            display: kmxplus.strs.strings[7],
            flags: KMXPlus.DispItemFlags.hintNE | KMXPlus.DispItemFlags.isId,
            id: null,
            to: null,
            toId: kmxplus.strs.strings[1],
          }
        ]);

        // --- kmxplus.elem ---

        assert.sameDeepMembers(kmxplus.elem.strings, [<LdmlKeyboardTypes.ElementString>[]]);

        // --- kmxplus.layr ---

        assert.sameDeepMembers(kmxplus.layr.forms, []);

        // --- kmxplus.list ---

        assert.sameDeepMembers(kmxplus.list.lists, [
          <KMXPlus.ListItem>[],
          <KMXPlus.ListItem>[{value: kmxplus.strs.strings[2]}],
          <KMXPlus.ListItem>[{value: kmxplus.strs.strings[4]}],
          <KMXPlus.ListItem>[{value: kmxplus.strs.strings[5]}],
        ]);

        // --- kmxplus.loca ---

        assert.sameDeepMembers(kmxplus.loca.locales, []);

        // --- kmxplus.meta ---

        assert.equal(kmxplus.meta.author,  kmxplus.strs.strings[0]);
        assert.equal(kmxplus.meta.conform, kmxplus.strs.strings[0]);
        assert.equal(kmxplus.meta.layout, kmxplus.strs.strings[0]);
        assert.equal(kmxplus.meta.name, kmxplus.strs.strings[0]);
        assert.equal(kmxplus.meta.indicator, kmxplus.strs.strings[0]);
        assert.equal(kmxplus.meta.version, kmxplus.strs.strings[0]);
        assert.equal(kmxplus.meta.settings, 0);

        // --- er: the returned row ---

        assert.isArray(kmxRow.keys);
        assert.lengthOf(kmxRow.keys, 1);
        assert.equal(kmxRow.keys[0].value, kmxplus.keys.keys[3].id.value);
      });
    });

    describe('EmbedOskTouchLayoutInKmx.addRowFromTouchLayoutRow', function() {
      it('should build a row of keys', async function() {
        const entry = new KMXPlus.LayrEntry();
        const row = { id: 1, key: [ Q_KEY ] };
        embedder.unitTestEndpoints.addRowFromTouchLayoutRow(kmxplus, entry, { id: 'default', row: [] }, row);

        assert.lengthOf(callbacks.messages, 0);
        assert.lengthOf(entry.rows, 1);
        assert.lengthOf(entry.rows[0].keys, 1);
      });
    });

    describe('EmbedOskTouchLayoutInKmx.addLayerFromTouchLayoutLayer', function() {
      it('should transform a touch layout layer into a KMX+ structure', async function() {
        const layer: TouchLayout.TouchLayoutLayer = {
          id: 'default', row: [ { id: 1, key: [ Q_KEY ] } ]
        };

        const form = new KMXPlus.LayrForm();

        embedder.unitTestEndpoints.addLayerFromTouchLayoutLayer(kmxplus, form, 'phone', layer);

        assert.lengthOf(callbacks.messages, 0);
        assert.lengthOf(form.layers, 1);
        assert.equal(form.layers[0].id.value, 'default');
        assert.equal(form.layers[0].mod, 0);
        assert.lengthOf(form.layers[0].rows, 1);
      });
    });

    describe('EmbedOskTouchLayoutInKmx.platformFromTouchLayoutPlatform', function() {
      it('should transform a whole platform into a KMX+ structure', async function() {
        const platform: TouchLayout.TouchLayoutPlatform = {
          defaultHint: 'dot',
          layer: [
            { id: 'default', row: [ { id: 1, key: [ Q_KEY ] } ] }
          ],
          displayUnderlying: false,
        };

        embedder.unitTestEndpoints.addPlatformFromTouchLayoutPlatform(kmxplus, 'phone', platform, 200);

        assert.lengthOf(callbacks.messages, 0);
        assert.lengthOf(kmxplus.layr.forms, 1);
        assert.equal(kmxplus.layr.forms[0].baseLayout, kmxplus.strs.strings[1]);
        assert.equal(kmxplus.layr.forms[0].flags, 0);
        assert.equal(kmxplus.layr.forms[0].hardware, kmxplus.strs.strings[2]);
        assert.equal(kmxplus.layr.forms[0].minDeviceWidth, 200);
        assert.lengthOf(kmxplus.layr.forms[0].layers, 1);
        assert.equal(kmxplus.layr.forms[0].layers[0].id, kmxplus.strs.strings[3]);
        assert.equal(kmxplus.layr.forms[0].layers[0].mod, 0);

        assert.equal(kmxplus.strs.strings[1].value, 'en-us');
        assert.equal(kmxplus.strs.strings[2].value, 'touch');
      });
    });

    describe('EmbedOskTouchLayoutInKmx.transformTouchLayoutToKmxPlus', function() {
      it('should transform a whole touch layout file into a KMX+ structure', async function() {
        const touchLayout: TouchLayout.TouchLayoutFile = {
          phone: {
            defaultHint: 'dot',
            layer: [
              { id: 'default', row: [ { id: 1, key: [ Q_KEY ] } ] }
            ],
            displayUnderlying: false,
          }
        };

        embedder.transformTouchLayoutToKmxPlus(kmxfile, touchLayout);
        assert.lengthOf(callbacks.messages, 0);
        assert.lengthOf(kmxplus.layr.forms, 1);

        assert.lengthOf(kmxplus.strs.strings, 12);
        assert.equal(kmxplus.strs.strings[0].value, '');
        assert.equal(kmxplus.strs.strings[1].value, '\u25cc');
        assert.equal(kmxplus.strs.strings[2].value, 'en-us');
        assert.equal(kmxplus.strs.strings[3].value, 'touch');
        assert.equal(kmxplus.strs.strings[4].value, 'default');
        assert.equal(kmxplus.strs.strings[5].value, 'default-K_Q+1');
        assert.equal(kmxplus.strs.strings[6].value, 's');
        assert.equal(kmxplus.strs.strings[7].value, 'default-K_1+2');
        assert.equal(kmxplus.strs.strings[8].value, 'default-K_ENTER+3');
        assert.equal(kmxplus.strs.strings[9].value, 'default-U_1235+4');
        assert.equal(kmxplus.strs.strings[10].value, 'shift');
        assert.equal(kmxplus.strs.strings[11].value, '1');

        assert.equal(kmxplus.layr.forms[0].baseLayout, kmxplus.strs.strings[2]);
        assert.equal(kmxplus.layr.forms[0].flags, 0);
        assert.equal(kmxplus.layr.forms[0].hardware, kmxplus.strs.strings[3]);
        assert.equal(kmxplus.layr.forms[0].minDeviceWidth, 1);
        assert.lengthOf(kmxplus.layr.forms[0].layers, 1);
        assert.equal(kmxplus.layr.forms[0].layers[0].id, kmxplus.strs.strings[4]);
        assert.equal(kmxplus.layr.forms[0].layers[0].mod, 0);
      });
    });
  });
});
