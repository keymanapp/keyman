import * as fs from 'fs';
import 'mocha';
import { assert } from 'chai';
import { makePathToFixture } from '../helpers/index.js';
import { KmxFileReader } from "../../src/kmx/kmx-file-reader.js";
import { KMXFile } from "../../src/kmx/kmx.js";

describe('kmx-file-reader', function () {
  it('should read a valid file', function() {
    const path = makePathToFixture('kmx', 'khmer_angkor.kmx');
    const input = fs.readFileSync(path);
    const reader = new KmxFileReader();
    const kmx = reader.read(input);

    let store = kmx.stores.find(store => store.dwSystemID == KMXFile.TSS_VERSION);
    assert.isObject(store);
    assert.equal(store.dpString, '10.0');

    store = kmx.stores.find(store => store.dwSystemID == KMXFile.TSS_NAME);
    assert.isObject(store);
    assert.equal(store.dpString, 'Khmer Angkor');

    store = kmx.stores.find(store => store.dwSystemID == KMXFile.TSS_KEYBOARDVERSION);
    assert.isObject(store);
    assert.equal(store.dpString, '1.3');

    // TODO: add header, group, key tests once we have added support in KmxFileReader
  });
});
