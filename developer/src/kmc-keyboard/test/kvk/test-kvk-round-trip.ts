import * as fs from 'fs';
import 'mocha';
import {assert} from 'chai';
import { CompilerCallbacks, makePathToFixture } from '../helpers/index.js';
import KvksFileReader from "../../src/kvk/kvks-file-reader.js";
import KvkFileReader from "../../src/kvk/kvk-file-reader.js";
import KvkFileWriter from "../../src/kvk/kvk-file-writer.js";

describe('kvk-file-reader', function () {
  it('kvk-file-reader should round-trip with kvk-file-writer', function() {
    const path = makePathToFixture('kvk', 'khmer_angkor.kvk');
    const input = fs.readFileSync(path);
    const reader = new KvkFileReader();
    const vk = reader.read(input);
    const writer = new KvkFileWriter(vk);
    const output = writer.compile();
    fs.writeFileSync(path + '.json', JSON.stringify(vk,null,2));
    assert.deepEqual(input, output);
  });
});

describe('kvks-file-reader', function () {
  it('kvks-file-reader should compile with kvk-file-writer', function() {
    const callbacks = new CompilerCallbacks();
    const inputPath = makePathToFixture('kvk', 'khmer_angkor.kvks');
    const compiledPath = makePathToFixture('kvk', 'khmer_angkor.kvk');
    const input = fs.readFileSync(inputPath);
    const compiled = fs.readFileSync(compiledPath);
    const reader = new KvksFileReader(callbacks);
    const vk = reader.loadVisualKeyboard(input);
    const writer = new KvkFileWriter(vk);
    const output = writer.compile();
    fs.writeFileSync(inputPath + '.json', JSON.stringify(vk,null,2));
    fs.writeFileSync(inputPath + '.out', output);
    assert.deepEqual(output, compiled);
  });
});
