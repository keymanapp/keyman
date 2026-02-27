/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import 'mocha';
import { assert } from 'chai';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { KMX } from '@keymanapp/common-types';
import { KmnCompiler } from '../src/main.js';
import { EmbedOskInKmx } from '../src/compiler/embed-osk/embed-osk.js';

const __dirname = dirname(fileURLToPath(import.meta.url)).replace(/\\/g, '/');
const keyboardsDir = __dirname + '/../../../../../common/test/keyboards/';

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
