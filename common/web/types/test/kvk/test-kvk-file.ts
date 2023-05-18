import * as fs from 'fs';
import 'mocha';
import { makePathToFixture } from '../helpers/index.js';
import KvkFileReader from "../../src/kvk/kvk-file-reader.js";
import { verify_khmer_angkor } from './test-kvk-utils.js';

describe('kvk-file-reader', function () {
  it('kvk-file-reader should read a valid file', function() {
    const path = makePathToFixture('kvk', 'khmer_angkor.kvk');
    const input = fs.readFileSync(path);
    const reader = new KvkFileReader();
    const vk = reader.read(input);
    verify_khmer_angkor(vk);
  });
});
