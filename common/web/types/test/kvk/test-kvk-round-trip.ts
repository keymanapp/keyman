import * as fs from 'fs';
import 'mocha';
import {assert} from 'chai';
import Hexy from 'hexy';
import gitDiff from 'git-diff';
const { hexy } = Hexy;
import { loadSchema, makePathToFixture } from '../helpers/index.js';
import KvksFileReader from "../../src/kvk/kvks-file-reader.js";
import KvkFileReader from "../../src/kvk/kvk-file-reader.js";
import KvkFileWriter from "../../src/kvk/kvk-file-writer.js";
import KvksFileWriter from "../../src/kvk/kvks-file-writer.js";

/**
 *
 * @param actual input buffer
 * @param expected expected buffer
 * @returns
 */
function assertBufferMatch(actual: Buffer, expected: Buffer) {
  const sourceHex = hexy(actual);
  const expectedHex = hexy(expected);
  const diff = gitDiff(expectedHex, sourceHex);

  assert(!diff, 'Buffers did not match:\n' + diff);
  assert.equal(actual.length, expected.length, 'Buffer lengths are not the same');
  assert.deepEqual(actual, expected); // double check buffers: hexy can return '' on failure
}

describe('kvk-file-reader', function () {
  it('kvk-file-reader should round-trip with kvk-file-writer', function() {
    const path = makePathToFixture('kvk', 'khmer_angkor.kvk');
    const input = fs.readFileSync(path);
    const reader = new KvkFileReader();
    const vk = reader.read(input);
    const writer = new KvkFileWriter();
    const output = writer.write(vk);
    assertBufferMatch(input, Buffer.from(output));

    // and check the 2nd generation also
    const vk2 = reader.read(output);
    assert.deepEqual(vk2, vk);
  });
});

describe('kvks-file-reader', function () {
  it('kvks-file-reader should compile with kvk-file-writer', function() {
    const inputPath = makePathToFixture('kvk', 'khmer_angkor.kvks');
    const compiledPath = makePathToFixture('kvk', 'khmer_angkor.kvk');
    const input = fs.readFileSync(inputPath);
    const compiled = fs.readFileSync(compiledPath);
    const reader = new KvksFileReader();
    const kvks = reader.read(input);
    assert.doesNotThrow(() => {
      reader.validate(kvks, loadSchema('kvks'));
    });
    const invalidVkeys: string[] = [];
    const vk = reader.transform(kvks, invalidVkeys);
    assert.isEmpty(invalidVkeys);
    const writer = new KvkFileWriter();
    const output = writer.write(vk);
    assertBufferMatch(Buffer.from(output), compiled);
  });

  describe('kvks-file-writer', function() {
    it('kvks-file-writer should match what kvk-file-reader reads', function() {
      const kvkIn = makePathToFixture('kvk', 'khmer_angkor.kvk');
      const kvkBuf = fs.readFileSync(kvkIn);
      const kvkReader = new KvkFileReader();
      const kvksReader = new KvksFileReader();
      const vk = kvkReader.read(kvkBuf);
      const kvksWriter = new KvksFileWriter();
      const kvksOut = kvksWriter.write(vk);

      // Now, re-read kvk from the kvks
      const kvks = kvksReader.read(Buffer.from(kvksOut));
      assert.doesNotThrow(() => {
        kvksReader.validate(kvks, loadSchema('kvks'));
      });
      const invalidVkeys: string[] = [];
      const vk2 = kvksReader.transform(kvks, invalidVkeys);
      assert.isEmpty(invalidVkeys);

      // make sure the binary is the same
      assert.deepEqual(vk2, vk);
    });
  });
});
