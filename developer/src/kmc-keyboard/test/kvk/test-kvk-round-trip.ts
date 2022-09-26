import * as fs from 'fs';
import 'mocha';
import {assert} from 'chai';
import { compilerTestCallbacks, makePathToFixture } from '../helpers/index.js';
import KvksFileReader, { KVKSParseError } from "../../src/kvk/kvks-file-reader.js";
import KvkFileReader from "../../src/kvk/kvk-file-reader.js";
import KvkFileWriter from "../../src/kvk/kvk-file-writer.js";

describe('kvk-file-reader', function () {
  it('kvk-file-reader should round-trip with kvk-file-writer', function() {
    const path = makePathToFixture('kvk', 'khmer_angkor.kvk');
    const input = fs.readFileSync(path);
    const reader = new KvkFileReader();
    const vk = reader.read(input);
    const writer = new KvkFileWriter();
    const output = writer.write(vk);
    assert.deepEqual(input, output);
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
      reader.validate(kvks, compilerTestCallbacks.loadKvksJsonSchema());
    });
    const errors: KVKSParseError[] = [];
    const vk = reader.transform(kvks, errors);
    assert.isEmpty(errors);
    const writer = new KvkFileWriter();
    const output = writer.write(vk);
    assert.deepEqual(output, compiled);
  });
});
