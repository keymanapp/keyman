/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import 'mocha';
import { assert } from 'chai';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { KMX, KMXPlus, ModifierKeyConstant, USVirtualKeyCodes, VisualKeyboard } from '@keymanapp/common-types';
import { KMXPlusBuilder, oskFontMagicToken } from '@keymanapp/developer-utils';
import { makePathToFixture } from './helpers/index.js';
import { KmnCompilerMessages } from '../src/main.js';
import { EmbedOskInKmx } from '../src/compiler/embed-osk/embed-osk.js';
import { loadKvkFile } from '../src/compiler/osk.js';
import { EmbedOskKvkInKmx } from '../src/compiler/embed-osk/embed-osk-kvk.js';

// VK header is not used in all functions, e.g. buildLayerBags, so this is a
// default header for those tests
const NullVisualKeyboardHeader: VisualKeyboard.VisualKeyboardHeader = {
  flags: VisualKeyboard.VisualKeyboardHeaderFlags.kvkhNone,
  ansiFont: null,
  unicodeFont: null,
};

describe('Compiler OSK Embedding', function() {

  const callbacks = new TestCompilerCallbacks();

  this.beforeEach(function() {
    callbacks.clear();
  });

  this.afterEach(function() {
    if(this.currentTest.isFailed()) {
      callbacks.printMessages();
    }
  });

  describe('EmbedOskKvkInKmx', function() {
    const embedder = new EmbedOskKvkInKmx(callbacks);

    describe('EmbedOskKvkInKmx.buildLayerBags', function() {
      it('should build a bag of layers from an in-memory .kvks structure', async function() {
        const vk: VisualKeyboard.VisualKeyboard = {
          header: NullVisualKeyboardHeader,
          keys: [
            {
              vkey: USVirtualKeyCodes.K_A,
              text: 'a',
              shift: 0,
              flags: VisualKeyboard.VisualKeyboardKeyFlags.kvkkUnicode,
            },
            {
              vkey: USVirtualKeyCodes.K_B,
              text: 'B',
              shift: VisualKeyboard.VisualKeyboardShiftState.KVKS_SHIFT,
              flags: VisualKeyboard.VisualKeyboardKeyFlags.kvkkUnicode,
            },
            {
              vkey: USVirtualKeyCodes.K_C,
              text: 'Ctrl+Shift+C',
              shift: VisualKeyboard.VisualKeyboardShiftState.KVKS_CTRL | VisualKeyboard.VisualKeyboardShiftState.KVKS_SHIFT,
              flags: VisualKeyboard.VisualKeyboardKeyFlags.kvkkUnicode,
            },
          ]
        };

        const strs = new KMXPlus.Strs();
        const keys = new KMXPlus.Keys(strs);
        const bag = embedder.unitTestEndpoints.buildLayerBags(vk, strs, keys);

        assert.lengthOf(strs.strings, 7);
        assert.equal(strs.strings[0].value, '');
        assert.equal(strs.strings[1].value, 'default-K_A');
        assert.equal(strs.strings[2].value, 'a');
        assert.equal(strs.strings[3].value, 'shift-K_B');
        assert.equal(strs.strings[4].value, 'B');
        assert.equal(strs.strings[5].value, 'shift-ctrl-K_C');
        assert.equal(strs.strings[6].value, 'Ctrl+Shift+C');

        assert.lengthOf(keys.flicks, 1);
        assert.lengthOf(keys.flicks[0].flicks, 0);
        assert.equal(keys.flicks[0].id, strs.strings[0]);

        assert.deepEqual(keys.keys, [
          {
            flags: 0,
            flicks: "",
            id: strs.strings[1],
            longPress: null,
            longPressDefault: strs.strings[0],
            multiTap: null,
            switch: strs.strings[0],
            to: strs.strings[2],
            width: 100
          },
          {
            flags: 0,
            flicks: "",
            id: strs.strings[3],
            longPress: null,
            longPressDefault: strs.strings[0],
            multiTap: null,
            switch: strs.strings[0],
            to: strs.strings[4],
            width: 100
          },
          {
            flags: 0,
            flicks: "",
            id: strs.strings[5],
            longPress: null,
            longPressDefault: strs.strings[0],
            multiTap: null,
            switch: strs.strings[0],
            to: strs.strings[6],
            width: 100
          },
        ]);

        assert.isArray(keys.kmap);
        assert.isEmpty(keys.kmap);

        // bag will be a map of maps; this test has three layers with an unmodified base layer K_A, a shift+K_B, and Ctrl+Shift+C
        assert.isNotNull(bag);
        assert.equal(bag.size, 3);
        assert.isTrue(bag.has(0));
        assert.isTrue(bag.has(ModifierKeyConstant.K_SHIFTFLAG));
        assert.isTrue(bag.has(ModifierKeyConstant.K_SHIFTFLAG | ModifierKeyConstant.K_CTRLFLAG));

        const defaultLayer = bag.get(0);
        assert.equal(defaultLayer.size, 1);

        assert.isTrue(defaultLayer.has(USVirtualKeyCodes.K_A));
        const k_a = defaultLayer.get(USVirtualKeyCodes.K_A);
        assert.equal(k_a.flags, 0);
        assert.equal(k_a.flicks, "");
        assert.equal(k_a.id, strs.strings[1]);
        assert.equal(k_a.longPress, null);
        assert.equal(k_a.longPressDefault, strs.strings[0]);
        assert.equal(k_a.multiTap, null);
        assert.equal(k_a.switch, strs.strings[0]);
        assert.equal(k_a.to, strs.strings[2]);
        assert.equal(k_a.width, 100);

        const shiftLayer = bag.get(ModifierKeyConstant.K_SHIFTFLAG);
        assert.equal(shiftLayer.size, 1);

        assert.isTrue(shiftLayer.has(USVirtualKeyCodes.K_B));
        const k_b = shiftLayer.get(USVirtualKeyCodes.K_B);
        assert.equal(k_b.flags, 0);
        assert.equal(k_b.flicks, "");
        assert.equal(k_b.id, strs.strings[3]);
        assert.equal(k_b.longPress, null);
        assert.equal(k_b.longPressDefault, strs.strings[0]);
        assert.equal(k_b.multiTap, null);
        assert.equal(k_b.switch, strs.strings[0]);
        assert.equal(k_b.to, strs.strings[4]);
        assert.equal(k_b.width, 100);

        const shiftCtrlLayer = bag.get(ModifierKeyConstant.K_SHIFTFLAG | ModifierKeyConstant.K_CTRLFLAG);
        assert.equal(shiftCtrlLayer.size, 1);

        assert.isTrue(shiftCtrlLayer.has(USVirtualKeyCodes.K_C));
        const k_c = shiftCtrlLayer.get(USVirtualKeyCodes.K_C);
        assert.equal(k_c.flags, 0);
        assert.equal(k_c.flicks, "");
        assert.equal(k_c.id, strs.strings[5]);
        assert.equal(k_c.longPress, null);
        assert.equal(k_c.longPressDefault, strs.strings[0]);
        assert.equal(k_c.multiTap, null);
        assert.equal(k_c.switch, strs.strings[0]);
        assert.equal(k_c.to, strs.strings[6]);
        assert.equal(k_c.width, 100);

      });

      it('should emit WARN_EmbeddedOskDoesNotSupportBitmaps if a key with kvkkBitmap flag is found', async function() {
        const vk: VisualKeyboard.VisualKeyboard = {
          header: NullVisualKeyboardHeader,
          keys: [
            {
              vkey: USVirtualKeyCodes.K_A,
              shift: 0,
              // kvkkUnicode required because otherwise the key is ignored as 'ansi'
              flags: VisualKeyboard.VisualKeyboardKeyFlags.kvkkBitmap | VisualKeyboard.VisualKeyboardKeyFlags.kvkkUnicode,
              bitmap: new Uint8Array()
            },
          ]
        };

        const strs = new KMXPlus.Strs();
        const keys = new KMXPlus.Keys(strs);
        const bag = embedder.unitTestEndpoints.buildLayerBags(vk, strs, keys);
        assert.isNotNull(bag);
        assert.equal(bag.size, 0);
        assert.isTrue(callbacks.hasMessage(KmnCompilerMessages.WARN_EmbeddedOskDoesNotSupportBitmaps));
      });

      it('should emit HINT_EmbeddedOskDoesNotSupportNonUnicode if a key without kvkkUnicode flag is found', async function() {
        const vk: VisualKeyboard.VisualKeyboard = {
          header: NullVisualKeyboardHeader,
          keys: [
            {
              vkey: USVirtualKeyCodes.K_A,
              shift: 0,
              // !kvkkUnicode
              flags: 0 as VisualKeyboard.VisualKeyboardKeyFlags,
            },
          ]
        };

        const strs = new KMXPlus.Strs();
        const keys = new KMXPlus.Keys(strs);
        const bag = embedder.unitTestEndpoints.buildLayerBags(vk, strs, keys);
        assert.isNotNull(bag);
        assert.equal(bag.size, 0);
        assert.isTrue(callbacks.hasMessage(KmnCompilerMessages.HINT_EmbeddedOskDoesNotSupportNonUnicode));
      });
    });

    describe('EmbedOskKvkInKmx.buildForm', function() {
      it('should transform a layout of keys from a layer bag, from an in-memory .kvks structure', async function() {

        const vk: VisualKeyboard.VisualKeyboard = {
          header: NullVisualKeyboardHeader,
          keys: [
            { vkey: USVirtualKeyCodes.K_A, text: 'a', shift: 0, flags: VisualKeyboard.VisualKeyboardKeyFlags.kvkkUnicode, },
            { vkey: USVirtualKeyCodes.K_B, text: 'b', shift: 0, flags: VisualKeyboard.VisualKeyboardKeyFlags.kvkkUnicode, },
            { vkey: USVirtualKeyCodes.K_C, text: 'c', shift: 0, flags: VisualKeyboard.VisualKeyboardKeyFlags.kvkkUnicode, },
          ]
        };

        const strs = new KMXPlus.Strs();
        const keys = new KMXPlus.Keys(strs);
        const layerBags = embedder.unitTestEndpoints.buildLayerBags(vk, strs, keys);
        assert.isNotNull(layerBags);
        const form = embedder.unitTestEndpoints.buildForm(vk, layerBags, strs);
        assert.isNotNull(form);
        assert.equal(form.baseLayout.value, 'en-us'); // For v19
        assert.equal(form.flags, 0);
        assert.equal(form.fontFaceName.value, oskFontMagicToken);
        assert.equal(form.fontSizePct, 100);
        assert.equal(form.hardware.value, 'us');
        assert.equal(form.minDeviceWidth, 0);

        assert.lengthOf(form.layers, 1);
        assert.equal(form.layers[0].id.value, 'default');
        assert.equal(form.layers[0].mod, 0); // no modifiers
        assert.equal(form.layers[0].rows.length, 5);

        assert.equal(form.layers[0].rows[0].keys.length, 13);
        assert.equal(form.layers[0].rows[1].keys.length, 13);
        assert.equal(form.layers[0].rows[2].keys.length, 11);
        assert.equal(form.layers[0].rows[3].keys.length, 10);
        assert.equal(form.layers[0].rows[4].keys.length, 1);

        assert.deepEqual(form.layers[0].rows[0].keys.map(key => key.value), ['gap','gap','gap','gap','gap','gap','gap','gap','gap','gap','gap','gap','gap']);
        assert.deepEqual(form.layers[0].rows[1].keys.map(key => key.value), ['gap','gap','gap','gap','gap','gap','gap','gap','gap','gap','gap','gap','gap']);
        assert.deepEqual(form.layers[0].rows[2].keys.map(key => key.value), ['default-K_A','gap','gap','gap','gap','gap','gap','gap','gap','gap','gap']);
        assert.deepEqual(form.layers[0].rows[3].keys.map(key => key.value), ['gap','gap','default-K_C','gap','default-K_B','gap','gap','gap','gap','gap']);
        assert.deepEqual(form.layers[0].rows[4].keys.map(key => key.value), ['gap']);
      });
    });

    describe('EmbedOskKvkInKmx.transformVisualKeyboardToKmxPlus', function() {
      it('should transform a .kvks file into an KMX+ structure', async function() {
        const vk = loadKvkFile(makePathToFixture('embed-osk', 'khmer_angkor.kvks'), callbacks);
        assert.isNotNull(vk);

        const kmxPlus = new EmbedOskInKmx(callbacks,{}).unitTestEndpoints.createEmptyKmxPlusFile();
        assert.isNotNull(kmxPlus);

        embedder.unitTestEndpoints.transformVisualKeyboardToKmxPlus(kmxPlus, vk);

        // Verify various aspects of the kmxPlus based on the source .kvks
        assert.equal(kmxPlus.kmxplus.keys.flicks.length, 1);

        // number of <key>s in the .kvks = 186, vscode search
        assert.equal(kmxPlus.kmxplus.keys.keys.length, 186);

        // first key in the file is RA K_B ឞ
        assert.equal(kmxPlus.kmxplus.keys.keys[0].id.value, 'rightalt-K_B');
        assert.equal(kmxPlus.kmxplus.keys.keys[0].to.value, 'ឞ');

        // last key in the file is Shift K_BKQUOTE »
        assert.equal(kmxPlus.kmxplus.keys.keys[kmxPlus.kmxplus.keys.keys.length-1].id.value, 'shift-K_BKQUOTE');
        assert.equal(kmxPlus.kmxplus.keys.keys[kmxPlus.kmxplus.keys.keys.length-1].to.value, '»');

        // first layer is ralt
        // first key on the first row of the RALT layer should be RALT+BKQUOTE
        assert.equal(kmxPlus.kmxplus.layr.forms.length, 1);
        assert.equal(kmxPlus.kmxplus.layr.forms[0].baseLayout.value, 'en-us');
        assert.equal(kmxPlus.kmxplus.layr.forms[0].flags, KMXPlus.LayrFormFlags.chiralSeparate);
        // TODO-EMBED-OSK-IN-KMX: need to test showBaseLayout at some point
        assert.equal(kmxPlus.kmxplus.layr.forms[0].fontFaceName.value, oskFontMagicToken);
        assert.equal(kmxPlus.kmxplus.layr.forms[0].fontSizePct, 100);
        assert.equal(kmxPlus.kmxplus.layr.forms[0].hardware.value, 'us');
        assert.equal(kmxPlus.kmxplus.layr.forms[0].minDeviceWidth, 0);
        assert.equal(kmxPlus.kmxplus.layr.forms[0].layers.length, 4);
        assert.equal(kmxPlus.kmxplus.layr.forms[0].layers[0].id.value, 'rightalt');
        assert.equal(kmxPlus.kmxplus.layr.forms[0].layers[0].mod, KMX.KMXFile.RALTFLAG);
        assert.equal(kmxPlus.kmxplus.layr.forms[0].layers[0].rows.length, 5);
        assert.equal(kmxPlus.kmxplus.layr.forms[0].layers[0].rows[0].keys.length, 13);
        assert.equal(kmxPlus.kmxplus.layr.forms[0].layers[0].rows[0].keys[0].value, 'rightalt-K_BKQUOTE');

        // Finally, pass the kmxPlus data through KMXPlusBuilder, there should be no errors,
        // hints, or warnings for this file

        const builder = new KMXPlusBuilder(kmxPlus);
        const data = builder.compile();

        assert.isNotNull(data);
        assert.lengthOf(callbacks.messages, 0);
      });
    });
  });
});
