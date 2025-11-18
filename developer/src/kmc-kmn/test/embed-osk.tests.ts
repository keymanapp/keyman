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

const __dirname = dirname(fileURLToPath(import.meta.url)).replace(/\\/g, '/');
const keyboardsDir = __dirname + '/../../../../../common/test/keyboards/';

describe('Compiler OSK Embedding', function() {

  it('should compile a keyboard and reserve space for the KMX+ data for v19+', async function() {
    const compiler = new KmnCompiler();
    const callbacks = new TestCompilerCallbacks();
    assert.isTrue(await compiler.init(callbacks, {
      saveDebug: false,
      shouldAddCompilerVersion: false,
    }));
    assert.isTrue(compiler.verifyInitialized());

    const fixtureDir = keyboardsDir + 'embed_osk/source/';
    const infile = fixtureDir + 'test_v19_kmxplus.kmn';
    const resultingKmxfile = __dirname + '/test_v19_kmxplus.kmx';
    const result = await compiler.run(infile, resultingKmxfile);
    assert.isNotNull(result);
    assert.isNotNull(result.artifacts.kmx);

    const expectedHeaderSize = KMX.KMXFile.COMP_KEYBOARD_SIZE + KMX.KMXFile.COMP_KEYBOARD_KMXPLUSINFO_SIZE;

    // We'll check that hte stores and groups start after the KMX+ struct

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
