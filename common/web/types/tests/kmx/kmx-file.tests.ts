import * as fs from 'fs';
import 'mocha';
import { assert } from 'chai';
import { makePathToFixture } from '../helpers/index.js';
import { KmxFileReader } from "../../src/kmx/kmx-file-reader.js";
import { KMX_Version, KMXFile, versionStringToKmxVersion } from "../../src/kmx/kmx.js";

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

describe('versionStringToKmxVersion', function() {
  [
    // We only care about v6.0 and up these days
    {s:'6.0', v:KMX_Version.VERSION_60},
    {s:'6', v:KMX_Version.VERSION_60},
    {s:'7.0', v:KMX_Version.VERSION_70},
    {s:'7', v:KMX_Version.VERSION_70},
    {s:'8.0', v:KMX_Version.VERSION_80},
    {s:'8', v:KMX_Version.VERSION_80},
    {s:'9.0', v:KMX_Version.VERSION_90},
    {s:'9', v:KMX_Version.VERSION_90},
    {s:'10.0', v:KMX_Version.VERSION_100},
    {s:'10', v:KMX_Version.VERSION_100},
    {s:'14.0', v:KMX_Version.VERSION_140},
    {s:'14', v:KMX_Version.VERSION_140},
    {s:'15.0', v:KMX_Version.VERSION_150},
    {s:'15', v:KMX_Version.VERSION_150},
    {s:'16.0', v:KMX_Version.VERSION_160},
    {s:'16', v:KMX_Version.VERSION_160},
    {s:'17.0', v:KMX_Version.VERSION_170},
    {s:'17', v:KMX_Version.VERSION_170},
    {s:'19.0', v:KMX_Version.VERSION_190},
    {s:'19', v:KMX_Version.VERSION_190},
  ].forEach(function(v) {
    it(`should convert valid version string ${v.s}`, function() {
      const actual = versionStringToKmxVersion(v.s);
      assert.equal(actual, v.v);
    });
  });

  ['zero','six','VERSION_60','1.0','-6','','5.0.1','19.0-alpha',null,undefined,]
  .forEach(function(v) {
    it(`should reject invalid version string '${v}'`, function() {
      const actual = versionStringToKmxVersion(v);
      assert.isNull(actual);
    });
  });
});