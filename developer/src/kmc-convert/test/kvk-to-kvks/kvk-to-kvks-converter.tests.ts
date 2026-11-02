/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2026-11-02
 *
 * Tests for KvkToKvksConverter
 */
import * as path from 'node:path';
import 'mocha';
import { assert } from 'chai';
import { compilerTestCallbacks, compilerTestOptions, makePathToFixture } from '../helpers/index.js';
import { ConverterMessages } from '../../src/converter-messages.js';
import { KvkToKvksConverter } from '../../src/kvk-to-kvks/kvk-to-kvks-converter.js';

describe('KvkToKvksConverter', function () {
  it('it should convert a valid .kvk file to .kvks', async function () {
    const converter = new KvkToKvksConverter(compilerTestCallbacks, compilerTestOptions);
    const inputFilename = makePathToFixture('kvk-to-kvks', 'balochi_inpage.kvk');
    const result = await converter.run(inputFilename, '');
    assert.isEmpty(compilerTestCallbacks.messages);
    assert.isObject(result);
    assert.isObject(result.artifacts);
    assert.isUndefined(result.artifacts.keymanTouchLayout);
    assert.isUndefined(result.artifacts.kmn);

    assert.isDefined(result.artifacts.kvks);
    assert.equal(path.basename(result.artifacts.kvks.filename), 'balochi_inpage.kvks');
    assert.instanceOf(result.artifacts.kvks.data, Uint8Array);
    // We do not need more assertions for the content of the data, as this is
    // tested in developer/utils:kvk-round-trip.tests.ts.
  });

  it('should fail with ERROR_FileNotFound when an the input file is missing', async function () {
    const converter = new KvkToKvksConverter(compilerTestCallbacks, compilerTestOptions);
    const inputFilename = makePathToFixture('kvk-to-kvks', 'missing_file.kvk');
    const result = await converter.run(inputFilename, '');
    assert.isNull(result);
    assert.equal(compilerTestCallbacks.messages.length, 1);
    assert.isTrue(compilerTestCallbacks.hasMessage(ConverterMessages.ERROR_FileNotFound));
  });

  it('should fail with ERROR_InvalidFile when the source file is not a valid .kvk file', async function () {
    const converter = new KvkToKvksConverter(compilerTestCallbacks, compilerTestOptions);
    const inputFilename = makePathToFixture('kvk-to-kvks', 'invalid_file.txt');
    const result = await converter.run(inputFilename, '');
    assert.isNull(result);
    assert.equal(compilerTestCallbacks.messages.length, 1);
    assert.isTrue(compilerTestCallbacks.hasMessage(ConverterMessages.ERROR_InvalidFile));
  });
});
