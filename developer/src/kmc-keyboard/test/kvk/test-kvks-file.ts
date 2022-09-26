import * as fs from 'fs';
import 'mocha';
import { compilerTestCallbacks, makePathToFixture } from '../helpers/index.js';
import KvksFileReader from "../../src/kvk/kvks-file-reader.js";
import { verify_khmer_angkor } from './test-kvk-utils.js';

describe('kvks-file-reader', function() {
  it('kvks-file-reader should read a valid file', function() {
    const path = makePathToFixture('kvk', 'khmer_angkor.kvks');
    const input = fs.readFileSync(path);
    const reader = new KvksFileReader(compilerTestCallbacks);
    const vk = reader.read(input);
    verify_khmer_angkor(vk);
  });
});
