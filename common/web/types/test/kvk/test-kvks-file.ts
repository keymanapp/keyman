import * as fs from 'fs';
import 'mocha';
import { loadKvksJsonSchema, makePathToFixture } from '../helpers/index.js';
import KvksFileReader, { KVKSParseError } from "../../src/kvk/kvks-file-reader.js";
import KvksFileWriter from "../../src/kvk/kvks-file-writer.js";
import { verify_khmer_angkor } from './test-kvk-utils.js';
import { assert } from 'chai';

describe('kvks-file-reader', function() {
  it('should read a valid file', function() {
    const path = makePathToFixture('kvk', 'khmer_angkor.kvks');
    const input = fs.readFileSync(path);

    const reader = new KvksFileReader();
    const kvks = reader.read(input);
    assert.doesNotThrow(() => {
      reader.validate(kvks, loadKvksJsonSchema());
    });
    const errors: KVKSParseError[] = [];
    const vk = reader.transform(kvks, errors);
    assert.isEmpty(errors);
    verify_khmer_angkor(vk);
  });
});

describe('kvks-file-writer', function() {
  it('should write a valid file', function() {
    const path = makePathToFixture('kvk', 'khmer_angkor.kvks');
    const input = fs.readFileSync(path);

    const reader = new KvksFileReader();
    const kvksExpected = reader.read(input);
    const errors: KVKSParseError[] = [];
    const vk = reader.transform(kvksExpected, errors);
    assert.isEmpty(errors);

    const writer = new KvksFileWriter();
    const output = writer.write(vk);

    // We compare the (re)loaded data, because there may be
    // minor, irrelevant formatting differences in the emitted xml
    const kvks = reader.read(Buffer.from(output, 'utf8'));
    assert.deepEqual(kvks, kvksExpected);
  });
});
