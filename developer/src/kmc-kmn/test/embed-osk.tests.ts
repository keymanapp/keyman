/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import 'mocha';
import { assert } from 'chai';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { KMX, KMXPlus, KMXPlusFileReader } from '@keymanapp/common-types';
import { KMXPlusVersion } from '@keymanapp/ldml-keyboard-constants';
import { KmnCompiler } from '../src/main.js';
import { EmbedOskInKmx } from '../src/compiler/embed-osk/embed-osk.js';
import { makePathToFixture } from './helpers/index.js';

const __dirname = dirname(fileURLToPath(import.meta.url)).replace(/\\/g, '/');
const keyboardsDir = __dirname + '/../../../../../common/test/keyboards/';

const expectedHeaderSize = KMX.KMXFile.COMP_KEYBOARD_SIZE + KMX.KMXFile.COMP_KEYBOARD_KMXPLUSINFO_SIZE;

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

  async function compileKMNFile(id: string) {
    const compiler = new KmnCompiler();
    assert.isTrue(await compiler.init(callbacks, {
      saveDebug: false,
      shouldAddCompilerVersion: false,
    }));
    assert.isTrue(compiler.verifyInitialized());

    const fixtureDir = keyboardsDir + 'embed_osk/source/';
    const infile = fixtureDir + id + '.kmn'; //';
    const resultingKmxfile = __dirname + '/' + id + '.kmx';
    return await compiler.run(infile, resultingKmxfile);
  }


  describe('kmcmplib infrastructure', function() {

    it('should compile a keyboard and reserve space for the KMX+ data for v19+', async function() {
      // This tests kmcmplib, but is implemented in kmc-kmn for simplicity
      const result = await compileKMNFile('test_v19_kmxplus');
      assert.isNotNull(result);
      assert.isNotNull(result.artifacts.kmx);

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

    describe('EmbedOskInKmx.embed', function() {
      const embedder = new EmbedOskInKmx(callbacks, {});

      it('should embed a .kvks file correctly into a .kmx file', async function() {
        const kmxArtifacts = await compileKMNFile('test_v19_kmxplus');
        assert.isNotNull(kmxArtifacts);

        const data = embedder.embed(kmxArtifacts.artifacts.kmx.data, makePathToFixture('embed-osk/khmer_angkor.kvks'), '', null);
        assert.isNotNull(data);

        const kmx = new KMX.KMXFile();
        const binaryKeyboard = kmx.COMP_KEYBOARD.fromBuffer(data);
        assert.isAtLeast(binaryKeyboard.dpStoreArray, expectedHeaderSize);
        assert.isAtLeast(binaryKeyboard.dpGroupArray, expectedHeaderSize);

        // Verify that the KMX+ flags are set

        assert.equal(binaryKeyboard.dwFlags & KMX.KMXFile.KF_KMXPLUS, 0);
        assert.equal(binaryKeyboard.dwFlags & KMX.KMXFile.KF_KMXPLUSOSK, KMX.KMXFile.KF_KMXPLUSOSK);

        // KMX+ -- sect (sec2)

        const reader = new KMXPlusFileReader(KMXPlusVersion.Version19);
        const kmxplus = reader.readFromKmx(data);

        assert.isUndefined(kmxplus.bksp);
        assert.isUndefined(kmxplus.tran);
        assert.isUndefined(kmxplus.uset);
        assert.isUndefined(kmxplus.vars);

        // KMX+ -- strs

        assert.isObject(kmxplus.strs);

        // KMX+ -- disp

        assert.isObject(kmxplus.disp);
        assert.equal(kmxplus.disp.baseCharacter.value, '\u25CC');
        assert.lengthOf(kmxplus.disp.disps, 0);

        // KMX+ -- elem

        assert.isObject(kmxplus.elem);
        assert.lengthOf(kmxplus.elem.strings, 1);
        assert.lengthOf(kmxplus.elem.strings[0], 0);

        // KMX+ -- keys

        assert.isObject(kmxplus.keys);
        assert.lengthOf(kmxplus.keys.flicks, 1);
        assert.equal(kmxplus.keys.flicks[0].id.value, '');
        assert.lengthOf(kmxplus.keys.flicks[0].flicks, 0);
        assert.lengthOf(kmxplus.keys.kmap, 0);
        assert.lengthOf(kmxplus.keys.keys, 185); // number of key entries in the .kvks file (less one commented out to create a gap)

        // first key

        assert.equal(kmxplus.keys.keys[0].flags, 0);
        assert.equal(kmxplus.keys.keys[0].flicks, '');
        assert.equal(kmxplus.keys.keys[0].id.value, 'default-K_0');
        assert.lengthOf(kmxplus.keys.keys[0].longPress, 0);
        assert.equal(kmxplus.keys.keys[0].longPressDefault.value, '');
        assert.lengthOf(kmxplus.keys.keys[0].multiTap, 0);
        assert.equal(kmxplus.keys.keys[0].switch.value, '');
        assert.equal(kmxplus.keys.keys[0].to.value, '០');
        assert.equal(kmxplus.keys.keys[0].width, 100);

        const findKey = (id: string) => kmxplus.keys.keys.find(key => key.id.value == id);

        // key on shift layer with >1 char, tests 'extend' flag - <key vkey="K_A">ាំ</key>

        const shiftA = findKey('shift-K_A');
        assert.isDefined(shiftA);
        assert.equal(shiftA.flags, KMXPlus.KeysKeysFlags.extend);
        assert.equal(shiftA.flicks, '');
        assert.equal(shiftA.id.value, 'shift-K_A');
        assert.lengthOf(shiftA.longPress, 0);
        assert.equal   (shiftA.longPressDefault.value, '');
        assert.lengthOf(shiftA.multiTap, 0);
        assert.equal(shiftA.switch.value, '');
        assert.equal(shiftA.to.value, '\u17b6\u17c6');
        assert.equal(shiftA.width, 100);

        // random key on ralt layer - <key vkey="K_L">៘</key

        const raltL = findKey('rightalt-K_L');
        assert.isDefined(raltL);
        assert.equal   (raltL.flags, 0);
        assert.equal   (raltL.flicks, '');
        assert.equal   (raltL.id.value, 'rightalt-K_L');
        assert.lengthOf(raltL.longPress, 0);
        assert.equal   (raltL.longPressDefault.value, '');
        assert.lengthOf(raltL.multiTap, 0);
        assert.equal   (raltL.switch.value, '');
        assert.equal   (raltL.to.value, '៘');
        assert.equal   (raltL.width, 100);

        // random key on shift+ralt layer - <key vkey="K_B">᧻</key>

        const shiftRaltB = findKey('rightalt-shift-K_B');
        assert.isDefined(shiftRaltB);
        assert.equal   (shiftRaltB.flags, 0);
        assert.equal   (shiftRaltB.flicks, '');
        assert.equal   (shiftRaltB.id.value, 'rightalt-shift-K_B');
        assert.lengthOf(shiftRaltB.longPress, 0);
        assert.equal   (shiftRaltB.longPressDefault.value, '');
        assert.lengthOf(shiftRaltB.multiTap, 0);
        assert.equal   (shiftRaltB.switch.value, '');
        assert.equal   (shiftRaltB.to.value, '᧻');
        assert.equal   (shiftRaltB.width, 100);

        // KMX+ -- layr

        assert.isObject(kmxplus.layr);
        assert.lengthOf(kmxplus.layr.forms, 1);
        const form = kmxplus.layr.forms[0];
        assert.equal(form.baseLayout.value, 'en-us');
        assert.equal(form.flags, KMXPlus.LayrFormFlags.chiralSeparate);
        assert.equal(form.hardware.value, 'us');
        assert.equal(form.minDeviceWidth, 0);
        assert.lengthOf(form.layers, 4);

        assert.equal(form.layers[0].id.value, 'default');
        assert.equal(form.layers[0].mod, 0);
        assert.lengthOf(form.layers[0].rows, 5);


        for(const layer of form.layers) {
          // All layers have all rows fully defined
          assert.lengthOf(layer.rows[0].keys, 13);
          assert.lengthOf(layer.rows[1].keys, 13);
          assert.lengthOf(layer.rows[2].keys, 11);
          assert.lengthOf(layer.rows[3].keys, 10);
          assert.lengthOf(layer.rows[4].keys, 1);
          // Test a few random keys on various layers (that we know are defined on all layers)
          assert.equal(layer.rows[0].keys[1].value, layer.id.value + '-K_1');
          assert.equal(layer.rows[1].keys[0].value, layer.id.value + '-K_Q');
          assert.equal(layer.rows[1].keys[5].value, layer.id.value + '-K_Y');
          assert.equal(layer.rows[2].keys[0].value, layer.id.value + '-K_A');
          assert.equal(layer.rows[3].keys[0].value, layer.id.value + '-K_Z');
        }

        // gap key on shift+ralt layer - K_X --> test layr

        assert.equal(form.layers[2].rows[3].keys[1].value, 'gap');

        // KMX+ -- list

        assert.isObject(kmxplus.list);
        assert.lengthOf(kmxplus.list.lists, 1);
        assert.lengthOf(kmxplus.list.lists[0], 0);

        // KMX+ -- loca

        assert.isObject(kmxplus.loca);
        assert.lengthOf(kmxplus.loca.locales, 0);

        // KMX+ -- meta

        assert.isObject(kmxplus.meta);
        assert.equal(kmxplus.meta.author.value, '');
        assert.equal(kmxplus.meta.conform.value, '');
        assert.equal(kmxplus.meta.indicator.value, '');
        assert.equal(kmxplus.meta.layout.value, '');
        assert.equal(kmxplus.meta.name.value, '');
        assert.equal(kmxplus.meta.normalizationDisabled, 0);
        assert.equal(kmxplus.meta.settings, 0);
        assert.equal(kmxplus.meta.version.value, '');
      });

      it('should embed a .keyman-touch-layout file correctly into a .kmx file', async function() {
        const kmxArtifacts = await compileKMNFile('test_v19_kmxplus');
        assert.isNotNull(kmxArtifacts);

        const data = embedder.embed(kmxArtifacts.artifacts.kmx.data, '', makePathToFixture('embed-osk/khmer_angkor.keyman-touch-layout'), null);
        assert.isNotNull(data);

        const kmx = new KMX.KMXFile();
        const binaryKeyboard = kmx.COMP_KEYBOARD.fromBuffer(data);
        assert.isAtLeast(binaryKeyboard.dpStoreArray, expectedHeaderSize);
        assert.isAtLeast(binaryKeyboard.dpGroupArray, expectedHeaderSize);

        // Verify that the KMX+ flags are set

        assert.equal(binaryKeyboard.dwFlags & KMX.KMXFile.KF_KMXPLUS, 0);
        assert.equal(binaryKeyboard.dwFlags & KMX.KMXFile.KF_KMXPLUSOSK, KMX.KMXFile.KF_KMXPLUSOSK);

        // KMX+ -- sect (sec2)

        const reader = new KMXPlusFileReader(KMXPlusVersion.Version19);
        const kmxplus = reader.readFromKmx(data);

        assert.isUndefined(kmxplus.bksp);
        assert.isUndefined(kmxplus.tran);
        assert.isUndefined(kmxplus.uset);
        assert.isUndefined(kmxplus.vars);

        // KMX+ -- strs

        assert.isObject(kmxplus.strs);
        assert.lengthOf(kmxplus.strs.strings, 475);

        // KMX+ -- disp

        assert.isObject(kmxplus.disp);
        assert.equal(kmxplus.disp.baseCharacter.value, '\u25CC');
        assert.lengthOf(kmxplus.disp.disps, 94);

        // NE Hint on Q key

        const qDisp = kmxplus.disp.disps.find(disp => disp.toId.value == 'default-K_Q+169');
        assert.equal(qDisp.toId.value, 'default-K_Q+169');
        assert.equal(qDisp.display.value, '្ឆ');
        assert.equal(qDisp.flags, KMXPlus.DispItemFlags.hintNE | KMXPlus.DispItemFlags.isId);

        // Enter key cap on Enter key

        const enterDisp = kmxplus.disp.disps.find(disp => disp.toId.value == 'default-K_ENTER+56');
        assert.equal(enterDisp.toId.value, 'default-K_ENTER+56');
        assert.equal(enterDisp.display.value, '');
        assert.equal(enterDisp.flags, KMXPlus.DispItemFlags.keyCapEnter | KMXPlus.DispItemFlags.isId);

        // KMX+ -- elem

        assert.isObject(kmxplus.elem);
        assert.lengthOf(kmxplus.elem.strings, 1);
        assert.lengthOf(kmxplus.elem.strings[0], 0);

        // KMX+ -- keys

        assert.isObject(kmxplus.keys);
        assert.lengthOf(kmxplus.keys.flicks, 62);

        assert.equal(kmxplus.keys.flicks[0].id.value, '');
        assert.lengthOf(kmxplus.keys.flicks[0].flicks, 0);

        assert.equal(kmxplus.keys.flicks[1].id.value, 'default-K_A+189');
        assert.lengthOf(kmxplus.keys.flicks[1].flicks, 1);
        assert.equal(kmxplus.keys.flicks[1].flicks[0].directions[0].value.value, 's');
        assert.equal(kmxplus.keys.flicks[1].flicks[0].keyId.value, 'shift-K_A+190');


        assert.equal(kmxplus.keys.flicks[2].id.value, 'default-K_B+218');
        assert.lengthOf(kmxplus.keys.flicks[2].flicks, 1);
        assert.equal(kmxplus.keys.flicks[2].flicks[0].directions[0].value.value, 's');
        assert.equal(kmxplus.keys.flicks[2].flicks[0].keyId.value, 'default-T_17D2_1794+219');

        assert.equal(kmxplus.keys.flicks[61].id.value, 'western-U_003D+387');
        assert.lengthOf(kmxplus.keys.flicks[61].flicks, 1);
        assert.equal(kmxplus.keys.flicks[61].flicks[0].directions[0].value.value, 's');
        assert.equal(kmxplus.keys.flicks[61].flicks[0].keyId.value, 'western-U_2260+388');


        assert.lengthOf(kmxplus.keys.kmap, 0);
        assert.lengthOf(kmxplus.keys.keys, 398);

        // first key

        assert.equal(kmxplus.keys.keys[0].flags, 0);
        assert.equal(kmxplus.keys.keys[0].flicks, '');
        assert.equal(kmxplus.keys.keys[0].id.value, 'default-K_0+10');
        assert.lengthOf(kmxplus.keys.keys[0].longPress, 0);
        assert.equal(kmxplus.keys.keys[0].longPressDefault.value, '');
        assert.lengthOf(kmxplus.keys.keys[0].multiTap, 0);
        assert.equal(kmxplus.keys.keys[0].switch.value, '');
        assert.equal(kmxplus.keys.keys[0].to.value, '០');
        assert.equal(kmxplus.keys.keys[0].width, 100);

        // equals key

        const equalsKey = kmxplus.keys.keys[29];
        assert.isDefined(equalsKey);
        assert.equal    (equalsKey.flags, 0);
        assert.equal    (equalsKey.flicks, '');
        assert.equal    (equalsKey.id.value, 'default-K_EQUAL+12');
        assert.lengthOf (equalsKey.longPress, 0);
        assert.equal    (equalsKey.longPressDefault.value, '');
        assert.lengthOf (equalsKey.multiTap, 0);
        assert.equal    (equalsKey.switch.value, '');
        assert.equal    (equalsKey.to.value, 'ឲ');
        assert.equal    (equalsKey.width, 100);

        // ល key

        const laKey = kmxplus.keys.keys[43];
        assert.isDefined(laKey);
        assert.equal   (laKey.flags, 0);
        assert.equal   (laKey.flicks, '');
        assert.equal   (laKey.id.value, 'default-K_L+36');
        assert.lengthOf(laKey.longPress, 0);
        assert.equal   (laKey.longPressDefault.value, '');
        assert.lengthOf(laKey.multiTap, 0);
        assert.equal   (laKey.switch.value, '');
        assert.equal   (laKey.to.value, 'ល');
        assert.equal   (laKey.width, 100);

        // K_Q - south-flick

        const qKey = kmxplus.keys.keys[64];
        assert.equal(qKey.id.value, 'default-K_Q+169');
        assert.equal(qKey.to.value, 'ឆ');
        assert.equal(qKey.flicks, 'default-K_Q+169');

        const flicks = kmxplus.keys.flicks.find(flicks => flicks.id.value == 'default-K_Q+169');
        assert.equal(flicks.id.value, 'default-K_Q+169');
        assert.lengthOf(flicks.flicks, 1);
        assert.equal(flicks.flicks[0].directions[0].value.value, 's');
        assert.equal(flicks.flicks[0].keyId.value, 'default-T_17D2_1786+170');

        // K_NUMLOCK - multitap

        const numlockKey = kmxplus.keys.keys.find(key => key.id.value == 'default-K_NUMLOCK+226');
        assert.lengthOf(numlockKey.multiTap, 1);
        assert.equal(numlockKey.multiTap[0].value.value, 'default-K_NUMERALS+227');

        // K_PERIOD - multitap

        const periodKey = kmxplus.keys.keys.find(key => key.id.value == 'default-K_PERIOD+231');
        assert.lengthOf(periodKey.longPress, 1);
        assert.equal(periodKey.longPress[0].value.value, 'shift-K_PERIOD+232');
        assert.equal(periodKey.longPressDefault.value, 'shift-K_PERIOD+232');
        assert.equal(periodKey.width, 120); // happens to be a wider key, let's test that too

        // KMX+ -- layr

        assert.isObject(kmxplus.layr);
        assert.lengthOf(kmxplus.layr.forms, 2);
        const form = kmxplus.layr.forms[0];
        assert.equal(form.baseLayout.value, 'en-us');
        assert.equal(form.flags, 0);
        assert.equal(form.hardware.value, 'touch');
        assert.equal(form.minDeviceWidth, 1); // eq. to 'phone' layout
        assert.lengthOf(form.layers, 4);

        assert.equal(form.layers[0].id.value, 'default');
        assert.equal(form.layers[0].mod, 0);
        assert.lengthOf(form.layers[0].rows, 4);

        assert.equal(form.layers[1].id.value, 'numeric');
        assert.equal(form.layers[1].mod, 0);
        assert.lengthOf(form.layers[1].rows, 4);

        const layer = form.layers[0];

        assert.lengthOf(layer.rows[0].keys, 10);
        assert.lengthOf(layer.rows[1].keys, 10);
        assert.lengthOf(layer.rows[2].keys, 10);
        assert.lengthOf(layer.rows[3].keys, 5);

        assert.equal(layer.rows[0].keys[0].value, 'default-K_Q+169');
        assert.equal(layer.rows[1].keys[0].value, 'default-K_A+189');
        assert.equal(layer.rows[2].keys[0].value, 'default-K_SHIFT+209');
        assert.equal(layer.rows[3].keys[0].value, 'default-K_NUMLOCK+226');

        // KMX+ -- list

        assert.isObject(kmxplus.list);
        assert.lengthOf(kmxplus.list.lists, 17);
        assert.lengthOf(kmxplus.list.lists[0], 0);

        // KMX+ -- loca

        assert.isObject(kmxplus.loca);
        assert.lengthOf(kmxplus.loca.locales, 0);

        // KMX+ -- meta

        assert.isObject(kmxplus.meta);
        assert.equal(kmxplus.meta.author.value, '');
        assert.equal(kmxplus.meta.conform.value, '');
        assert.equal(kmxplus.meta.indicator.value, '');
        assert.equal(kmxplus.meta.layout.value, '');
        assert.equal(kmxplus.meta.name.value, '');
        assert.equal(kmxplus.meta.normalizationDisabled, 0);
        assert.equal(kmxplus.meta.settings, 0);
        assert.equal(kmxplus.meta.version.value, '');
      });

      it('should embed both a .keyman-touch-layout file and a .kvks file together correctly into a .kmx file', async function() {
        const kmxArtifacts = await compileKMNFile('test_v19_kmxplus');
        assert.isNotNull(kmxArtifacts);

        const data = embedder.embed(kmxArtifacts.artifacts.kmx.data, makePathToFixture('embed-osk/khmer_angkor.kvks'), makePathToFixture('embed-osk/khmer_angkor.keyman-touch-layout'), null);
        assert.isNotNull(data);

        const kmx = new KMX.KMXFile();
        const binaryKeyboard = kmx.COMP_KEYBOARD.fromBuffer(data);
        assert.isAtLeast(binaryKeyboard.dpStoreArray, expectedHeaderSize);
        assert.isAtLeast(binaryKeyboard.dpGroupArray, expectedHeaderSize);

        // Verify that the KMX+ flags are set

        assert.equal(binaryKeyboard.dwFlags & KMX.KMXFile.KF_KMXPLUS, 0);
        assert.equal(binaryKeyboard.dwFlags & KMX.KMXFile.KF_KMXPLUSOSK, KMX.KMXFile.KF_KMXPLUSOSK);

        // KMX+ -- sect (sec2)

        const reader = new KMXPlusFileReader(KMXPlusVersion.Version19);
        const kmxplus = reader.readFromKmx(data);

        assert.isUndefined(kmxplus.bksp);
        assert.isUndefined(kmxplus.tran);
        assert.isUndefined(kmxplus.uset);
        assert.isUndefined(kmxplus.vars);

        // KMX+ -- strs

        assert.isObject(kmxplus.strs);
        assert.lengthOf(kmxplus.strs.strings, 663);

        // KMX+ -- disp

        assert.isObject(kmxplus.disp);
        assert.equal(kmxplus.disp.baseCharacter.value, '\u25CC');
        assert.lengthOf(kmxplus.disp.disps, 94);

        // KMX+ -- elem

        assert.isObject(kmxplus.elem);
        assert.lengthOf(kmxplus.elem.strings, 1);
        assert.lengthOf(kmxplus.elem.strings[0], 0);

        // KMX+ -- keys

        assert.isObject(kmxplus.keys);
        assert.lengthOf(kmxplus.keys.flicks, 62);

        assert.lengthOf(kmxplus.keys.kmap, 0);
        assert.lengthOf(kmxplus.keys.keys, 583);

        // first key

        assert.equal(kmxplus.keys.keys[0].flags, 0);
        assert.equal(kmxplus.keys.keys[0].flicks, '');
        assert.equal(kmxplus.keys.keys[0].id.value, 'default-K_0');
        assert.lengthOf(kmxplus.keys.keys[0].longPress, 0);
        assert.equal(kmxplus.keys.keys[0].longPressDefault.value, '');
        assert.lengthOf(kmxplus.keys.keys[0].multiTap, 0);
        assert.equal(kmxplus.keys.keys[0].switch.value, '');
        assert.equal(kmxplus.keys.keys[0].to.value, '០');
        assert.equal(kmxplus.keys.keys[0].width, 100);

        // KMX+ -- layr

        assert.isObject(kmxplus.layr);
        assert.lengthOf(kmxplus.layr.forms, 3);

        // KMX+ -- list

        assert.isObject(kmxplus.list);
        assert.lengthOf(kmxplus.list.lists, 17);
        assert.lengthOf(kmxplus.list.lists[0], 0);

        // KMX+ -- loca

        assert.isObject(kmxplus.loca);
        assert.lengthOf(kmxplus.loca.locales, 0);

        // KMX+ -- meta

        assert.isObject(kmxplus.meta);
        assert.equal(kmxplus.meta.author.value, '');
        assert.equal(kmxplus.meta.conform.value, '');
        assert.equal(kmxplus.meta.indicator.value, '');
        assert.equal(kmxplus.meta.layout.value, '');
        assert.equal(kmxplus.meta.name.value, '');
        assert.equal(kmxplus.meta.normalizationDisabled, 0);
        assert.equal(kmxplus.meta.settings, 0);
        assert.equal(kmxplus.meta.version.value, '');
      });
    });
  });
});
