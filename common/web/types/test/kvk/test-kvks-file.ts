import * as fs from 'fs';
import 'mocha';
import { loadKvksJsonSchema, makePathToFixture } from '../helpers/index.js';
import KvksFileReader, { KVKSParseError } from "../../src/kvk/kvks-file-reader.js";
import { verify_khmer_angkor } from './test-kvk-utils.js';
import { assert } from 'chai';

describe('kvks-file-reader', function() {
  it('kvks-file-reader should read a valid file', function() {
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
