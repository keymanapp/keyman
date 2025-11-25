/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import 'mocha';
import { assert } from 'chai';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { KMX, KMXPlus, ModifierKeyConstant, USVirtualKeyCodes, VisualKeyboard } from '@keymanapp/common-types';
import { KMXPlusBuilder, oskFontMagicToken } from '@keymanapp/developer-utils';
import { makePathToFixture } from './helpers/index.js';
import { KmnCompiler, KmnCompilerMessages } from '../src/main.js';
import { EmbedOskInKmx } from '../src/compiler/embed-osk/embed-osk.js';
import { loadKvkFile } from '../src/compiler/osk.js';

const __dirname = dirname(fileURLToPath(import.meta.url)).replace(/\\/g, '/');
const keyboardsDir = __dirname + '/../../../../../common/test/keyboards/';


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

  describe('kmcmplib infrastructure', function() {

    it('should compile a keyboard and reserve space for the KMX+ data for v19+', async function() {
      // This tests kmcmplib, but is implemented in kmc-kmn for simplicity

      const compiler = new KmnCompiler();
      assert.isTrue(await compiler.init(callbacks, {
        saveDebug: false,
        shouldAddCompilerVersion: false,
        // TODO-EMBED-OSK-IN-KMX: test_skip_osk_embed: true --> or mock stub for EmbedOskInKmx function
      }));
      assert.isTrue(compiler.verifyInitialized());

      const fixtureDir = keyboardsDir + 'embed_osk/source/';
      const infile = fixtureDir + 'test_v19_kmxplus.kmn';
      const resultingKmxfile = __dirname + '/test_v19_kmxplus.kmx';
      const result = await compiler.run(infile, resultingKmxfile);
      assert.isNotNull(result);
      assert.isNotNull(result.artifacts.kmx);

      const expectedHeaderSize = KMX.KMXFile.COMP_KEYBOARD_SIZE + KMX.KMXFile.COMP_KEYBOARD_KMXPLUSINFO_SIZE;

      // We'll check that the stores and groups start after the KMX+ struct

      assert.isAtLeast(result.artifacts.kmx.data.byteLength, expectedHeaderSize);

      const kmx = new KMX.KMXFile();
      const binaryKeyboard = kmx.COMP_KEYBOARD.fromBuffer(result.artifacts.kmx.data);
      assert.isAtLeast(binaryKeyboard.dpStoreArray, expectedHeaderSize);
      assert.isAtLeast(binaryKeyboard.dpGroupArray, expectedHeaderSize);

      // Verify that the KMX+ flags are not set

      assert.equal(binaryKeyboard.dwFlags & KMX.KMXFile.KF_KMXPLUS, 0);
      assert.equal(binaryKeyboard.dwFlags & KMX.KMXFile.KF_KMXPLUSOSK, 0);

      // the KMX+ struct itself should be empty at this point

      const bufferKmxPlus = new Uint8Array(result.artifacts.kmx.data.buffer, KMX.KMXFile.COMP_KEYBOARD_SIZE);
      const binaryKmxPlus = kmx.COMP_KEYBOARD_KMXPLUSINFO.fromBuffer(bufferKmxPlus);
      assert.equal(binaryKmxPlus.dpKMXPlus, 0);
      assert.equal(binaryKmxPlus.dwKMXPlusSize, 0);
    });
  });

  describe('EmbedOskInKmx', function() {

    describe('EmbedOskInKmx.injectKmxPlusIntoKmxFile', function() {
      const embedder = new EmbedOskInKmx(callbacks, {});

      // This is not a valid KMX+ blob, but the function does not do any
      // validation of the contents of the KMX+ blob, but just injects what it
      // is given, so we can inject this and look for it at EOF
      const sentinelKmxPlusBlob = new TextEncoder().encode('SENTINEL');

      // None of these should ever be possible on outputs from kmcmplib in our
      // code paths, so we'll treat these as internal errors rather than a
      // compiler error

      it('should throw if passed a zero-byte KMX file', function() {
        const zeroByteFile = new Uint8Array(0);
        assert.throws(function() { embedder.unitTestEndpoints.injectKmxPlusIntoKmxFile(zeroByteFile, sentinelKmxPlusBlob) });
      });

      it('should throw if passed a version 17 KMX file', function() {
        // This file has a v17 header but is not a full file, just for test purposes
        // taken from k_000__null_keyboard.kmx and tweaked to v.17, made it long
        // enough for the KMX+ header but did not adjust offsets
        const invalid17KmxFile = new Uint8Array([
          // COMP_KEYBOARD
          0x4B, 0x58, 0x54, 0x53, // dwIdentifier = 'KXTS'
          0x00, 0x11, 0x00, 0x00, // dwFileVersion = 17.0
          0x00, 0x00, 0x00, 0x00, // dwCheckSum
          0x00, 0x00, 0x00, 0x00, // KeyboardID
          0x01, 0x00, 0x00, 0x00, // IsRegistered
          0x00, 0x00, 0x00, 0x00, // version (unused)
          0x06, 0x00, 0x00, 0x00, // cxStoreArray
          0x01, 0x00, 0x00, 0x00, // cxGroupArray
          0x40, 0x00, 0x00, 0x00, // dpStoreArray
          0x1A, 0x01, 0x00, 0x00, // dpGroupArray
          0xFF, 0xFF, 0xFF, 0xFF, // StartGroup[ansi]
          0x00, 0x00, 0x00, 0x00, // StartGroup[unicode]
          0x00, 0x00, 0x00, 0x00, // dwFlags
          0x00, 0x00, 0x00, 0x00, // dwHotKey
          0x3C, 0x01, 0x00, 0x00, // dpBitmapOffset
          0x00, 0x00, 0x00, 0x00, // dwBitmapSize
          // COMP_KEYBOARD_KMXPLUSINFO
          0x00, 0x00, 0x00, 0x00, // dpKMXPlus
          0x00, 0x00, 0x00, 0x00, // dwKMXPlusSize
        ]);

        assert.throws(function() { embedder.unitTestEndpoints.injectKmxPlusIntoKmxFile(invalid17KmxFile, sentinelKmxPlusBlob) }, 'Expected inputFile to be at least VERSION_190');
      });

      it('should throw if passed a version 19 KMX file that is too short', function() {
        // This file has a v19 header but is not a full file, just for test purposes
        // taken from k_000__null_keyboard.kmx and tweaked to v.19. Missing space
        // for the KMX+ header
        const invalid19KmxFileShort = new Uint8Array([
          // COMP_KEYBOARD
          0x4B, 0x58, 0x54, 0x53, // dwIdentifier = 'KXTS'
          0x00, 0x13, 0x00, 0x00, // dwFileVersion = 19.0
          0x00, 0x00, 0x00, 0x00, // dwCheckSum
          0x00, 0x00, 0x00, 0x00, // KeyboardID
          0x01, 0x00, 0x00, 0x00, // IsRegistered
          0x00, 0x00, 0x00, 0x00, // version (unused)
          0x06, 0x00, 0x00, 0x00, // cxStoreArray
          0x01, 0x00, 0x00, 0x00, // cxGroupArray
          0x40, 0x00, 0x00, 0x00, // dpStoreArray
          0x1A, 0x01, 0x00, 0x00, // dpGroupArray
          0xFF, 0xFF, 0xFF, 0xFF, // StartGroup[ansi]
          0x00, 0x00, 0x00, 0x00, // StartGroup[unicode]
          0x00, 0x00, 0x00, 0x00, // dwFlags
          0x00, 0x00, 0x00, 0x00, // dwHotKey
          0x3C, 0x01, 0x00, 0x00, // dpBitmapOffset
          0x00, 0x00, 0x00, 0x00, // dwBitmapSize
        ]);
        assert.throws(function() { embedder.unitTestEndpoints.injectKmxPlusIntoKmxFile(invalid19KmxFileShort, sentinelKmxPlusBlob) }, 'Expected inputFile to have space');
      });

      it('should throw if passed a version 19 KMX file that has no reservation for KMX+ header', function() {
        // This file has a v19 header but is not a full file, just for test purposes
        // taken from k_000__null_keyboard.kmx and tweaked to v.19. Has space for
        // KMX+ header but store and group offsets are not accounting for the space
        const invalid19KmxFileNoReservation = new Uint8Array([
          // COMP_KEYBOARD
          0x4B, 0x58, 0x54, 0x53, // dwIdentifier = 'KXTS'
          0x00, 0x13, 0x00, 0x00, // dwFileVersion = 19.0
          0x00, 0x00, 0x00, 0x00, // dwCheckSum
          0x00, 0x00, 0x00, 0x00, // KeyboardID
          0x01, 0x00, 0x00, 0x00, // IsRegistered
          0x00, 0x00, 0x00, 0x00, // version (unused)
          0x06, 0x00, 0x00, 0x00, // cxStoreArray
          0x01, 0x00, 0x00, 0x00, // cxGroupArray
          0x40, 0x00, 0x00, 0x00, // dpStoreArray -- note 0x40 offset
          0x1A, 0x01, 0x00, 0x00, // dpGroupArray
          0xFF, 0xFF, 0xFF, 0xFF, // StartGroup[ansi]
          0x00, 0x00, 0x00, 0x00, // StartGroup[unicode]
          0x00, 0x00, 0x00, 0x00, // dwFlags
          0x00, 0x00, 0x00, 0x00, // dwHotKey
          0x3C, 0x01, 0x00, 0x00, // dpBitmapOffset
          0x00, 0x00, 0x00, 0x00, // dwBitmapSize
          // COMP_KEYBOARD_KMXPLUSINFO
          0x00, 0x00, 0x00, 0x00, // dpKMXPlus
          0x00, 0x00, 0x00, 0x00, // dwKMXPlusSize
        ]);
        assert.throws(function() { embedder.unitTestEndpoints.injectKmxPlusIntoKmxFile(invalid19KmxFileNoReservation, sentinelKmxPlusBlob) }, 'Expected reservation for COMP_KEYBOARD_KMXPLUSINFO');
      });

      it('should successfully inject a KMX+ blob into a v19 KMX file', function() {
        // This file has a v19 header but is not a full file, just for test purposes
        // taken from k_000__null_keyboard.kmx and tweaked to v.19. Has space for
        // KMX+ header but store and group offsets are not accounting for the space
        const semiValid19KmxFile = new Uint8Array([
          // COMP_KEYBOARD
          0x4B, 0x58, 0x54, 0x53, // dwIdentifier = 'KXTS'
          0x00, 0x13, 0x00, 0x00, // dwFileVersion = 19.0
          0x00, 0x00, 0x00, 0x00, // dwCheckSum
          0x00, 0x00, 0x00, 0x00, // KeyboardID
          0x01, 0x00, 0x00, 0x00, // IsRegistered
          0x00, 0x00, 0x00, 0x00, // version (unused)
          0x01, 0x00, 0x00, 0x00, // cxStoreArray
          0x01, 0x00, 0x00, 0x00, // cxGroupArray
          0x48, 0x00, 0x00, 0x00, // dpStoreArray -- note 0x48 offset
          0x4C, 0x00, 0x00, 0x00, // dpGroupArray -- note 0x4C offset
          0xFF, 0xFF, 0xFF, 0xFF, // StartGroup[ansi]
          0x00, 0x00, 0x00, 0x00, // StartGroup[unicode]
          0x00, 0x00, 0x00, 0x00, // dwFlags
          0x00, 0x00, 0x00, 0x00, // dwHotKey
          0x3C, 0x01, 0x00, 0x00, // dpBitmapOffset
          0x00, 0x00, 0x00, 0x00, // dwBitmapSize
          // COMP_KEYBOARD_KMXPLUSINFO
          0x00, 0x00, 0x00, 0x00, // dpKMXPlus
          0x00, 0x00, 0x00, 0x00, // dwKMXPlusSize
          // Fake additional data - just to verify that we don't clobber it
          0x01, 0x02, 0x03, 0x04, // "dpStoreArray"
          0x05, 0x06, 0x07, 0x08, // "dpGroupArray"
        ]);

        // good path

        const data = embedder.unitTestEndpoints.injectKmxPlusIntoKmxFile(semiValid19KmxFile, sentinelKmxPlusBlob);
        assert.isNotNull(data);
        assert.equal(data.byteLength, semiValid19KmxFile.byteLength + sentinelKmxPlusBlob.byteLength);

        assert.deepEqual(
          data.slice(0x00, 0x30), semiValid19KmxFile.slice(0x00, 0x30),
          'Expected start of header to be unmodified'
        );

        assert.deepEqual(
          data.slice(0x34, 0x40), semiValid19KmxFile.slice(0x34, 0x40),
          'Expected end of header to be unmodified'
        );

        assert.deepEqual(
          data.slice(0x30, 0x34), new Uint8Array([0x40, 0, 0, 0]),
          'Expected KF_KMXPLUSOSK flag to have been set in COMP_KEYBOARD.dwFlags'
        );

        assert.deepEqual(
          data.slice(0x40, 0x44), new Uint8Array([semiValid19KmxFile.byteLength, 0, 0, 0]),
          'Expected COMP_KEYBOARD_KMXPLUSINFO.dpKMXPlus to have been updated to length of original file'
        );

        assert.deepEqual(
          data.slice(0x44, 0x48), new Uint8Array([sentinelKmxPlusBlob.byteLength, 0, 0, 0]),
          'Expected COMP_KEYBOARD_KMXPLUSINFO.dwKMXPlusSize to have been updated'
        );

        assert.deepEqual(
          data.slice(0x48, 0x50), new Uint8Array([1,2,3,4,5,6,7,8]),
          'Expected KMX data to have not been modified'
        );

        assert.deepEqual(
          data.slice(0x50, 0x58), sentinelKmxPlusBlob,
          'Expected KMX+ data to have been appended'
        );
      });
    });

    describe('EmbedOskInKmx.buildLayerBags', function() {
      const embedder = new EmbedOskInKmx(callbacks, {});

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

    describe('EmbedOskInKmx.buildForm', function() {
      const embedder = new EmbedOskInKmx(callbacks, {});

      it('should a layout of keys from a layer bag, from an in-memory .kvks structure', async function() {
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

    describe('EmbedOskInKmx.transformVisualKeyboardToKmxPlus', function() {
      const embedder = new EmbedOskInKmx(callbacks, {});

      it('should transform a .kvks file into an KMX+ structure', async function() {
        const vk = loadKvkFile(makePathToFixture('embed-osk', 'khmer_angkor.kvks'), callbacks);
        assert.isNotNull(vk);

        const kmxPlus = embedder.unitTestEndpoints.transformVisualKeyboardToKmxPlus(vk);
        assert.isNotNull(kmxPlus);

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

        // Finally, pass the through KMXPlusBuilder, there should be no errors,
        // hints, or warnings for this file

        const builder = new KMXPlusBuilder(kmxPlus);
        const data = builder.compile();

        assert.isNotNull(data);
        assert.lengthOf(callbacks.messages, 0);
      });
    });

    describe('EmbedOskInKmx.embed', function() {
      // const embedder = new EmbedOskInKmx(callbacks, {});

      it('should embed a .kvks file correctly into a .kmx file', function() {
        //
        // TODO-EMBED-OSK-IN-KMX: implement
        // const result = embedder.embed(kmx, kvksFilename, '', null);
        this.skip();
      });
    });
  });
});
