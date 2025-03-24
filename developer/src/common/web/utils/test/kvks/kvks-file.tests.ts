import * as fs from 'fs';
import 'mocha';
import { makePathToFixture } from '../helpers/index.js';
import KvksFileReader from "../../src/types/kvks/kvks-file-reader.js";
import KvksFileWriter from "../../src/types/kvks/kvks-file-writer.js";
import { verify_khmer_angkor, verify_balochi_inpage } from './kvk-utils.tests.js';
import { assert } from 'chai';

describe('kvks-file-reader', function() {
  it('should read a valid file', function() {
    const path = makePathToFixture('kvks', 'khmer_angkor.kvks');
    const input = fs.readFileSync(path);

    const reader = new KvksFileReader();
    const kvks = reader.read(input);
    assert.doesNotThrow(() => {
      reader.validate(kvks);
    });
    const invalidVkeys: string[] = [];
    const vk = reader.transform(kvks, invalidVkeys);
    assert.isEmpty(invalidVkeys);
    verify_khmer_angkor(vk);
  });

  it('should read a valid file with bitmaps', function() {
    const path = makePathToFixture('kvks', 'balochi_inpage.kvks');
    const input = fs.readFileSync(path);
    const reader = new KvksFileReader();
    const kvks = reader.read(input);
    const invalidVkeys: string[] = [];
    const vk = reader.transform(kvks, invalidVkeys);
    assert.isEmpty(invalidVkeys);
    verify_balochi_inpage(vk);
  });

  it('should give a sensible error on a .kvk file', function() {
    const path = makePathToFixture('kvks', 'khmer_angkor.kvk');
    const input = fs.readFileSync(path);

    const reader = new KvksFileReader();
    assert.throws(() => reader.read(input), 'File appears to be a binary .kvk file');
  });
});

describe('kvks-file-writer', function() {
  it('should write a valid file', function() {
    const path = makePathToFixture('kvks', 'khmer_angkor.kvks');
    const input = fs.readFileSync(path);

    const reader = new KvksFileReader();
    const kvksExpected = reader.read(input);
    const invalidVkeys: string[] = [];
    const vk = reader.transform(kvksExpected, invalidVkeys);
    assert.isEmpty(invalidVkeys);

    const writer = new KvksFileWriter();
    const output = writer.write(vk);

    // We compare the (re)loaded data, because there may be
    // minor, irrelevant formatting differences in the emitted xml
    const kvks = reader.read(Buffer.from(output, 'utf8'));
    assert.deepEqual(kvks, kvksExpected);
  });
});
