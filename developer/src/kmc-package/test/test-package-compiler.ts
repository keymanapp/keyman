import 'mocha';
import * as fs from 'fs';
import * as path from 'path';
import {assert} from 'chai';

import KmpCompiler from '../src/kmp-compiler.js';
import {makePathToFixture} from './helpers/index.js';
import JSZip from 'jszip';
import KEYMAN_VERSION from "@keymanapp/keyman-version/keyman-version.mjs";

let zip = JSZip();

describe('KmpCompiler', function () {
  const MODELS = [
    'example.qaa.sencoten',
  ];

  let kmpCompiler = new KmpCompiler();

  for (let modelID of MODELS) {
    let kpsPath = makePathToFixture(modelID, `${modelID}.model.kps`);
    let kmpJsonPath = makePathToFixture(modelID, `${modelID}.model.kmp.json`);
    let kmpJsonFixture = JSON.parse(fs.readFileSync(kmpJsonPath, 'utf-8'));
    // We override the fixture version so that we can compare with the compiler output
    kmpJsonFixture.system.keymanDeveloperVersion = KEYMAN_VERSION.VERSION;

    //
    // Test just the transform from kps to kmp.json
    //
    it(`should transform ${modelID}.model.kps to kmp.json`, function () {
      let source = fs.readFileSync(kpsPath, 'utf-8');
      let kmpJson: KmpJsonFile;

      assert.doesNotThrow(() => {
        kmpJson = kmpCompiler.transformKpsToKmpObject(kpsPath, source);
      });

      // Test that the kmp.json data is identical
      assert.deepEqual(kmpJson, kmpJsonFixture);

      // Note that in-memory kmp.json still contains paths in the files array.
      // However, when building the .kmp, the final written kmp.json data is
      // modified to strip paths.

      // This was used when building initial test data
      //fs.writeFileSync(kmpJsonPath, JSON.stringify(kmpJson), 'utf-8');
    });
  }

  it('should generates a valid .kmp (zip) file', async function() {
    // const kmpPath = makePathToFixture('khmer_angkor', 'build', 'khmer_angkor.kmp');
    const kpsPath = makePathToFixture('khmer_angkor', 'source', 'khmer_angkor.kps');
    const kmpJsonRefPath = makePathToFixture('khmer_angkor', 'ref', 'kmp.json');

    const kmpCompiler = new KmpCompiler();
    const source = fs.readFileSync(kpsPath, 'utf-8');
    const kmpJsonFixture: KmpJsonFile = JSON.parse(fs.readFileSync(kmpJsonRefPath, 'utf-8'));

    // We override the fixture version so that we can compare with the compiler output
    kmpJsonFixture.system.keymanDeveloperVersion = KEYMAN_VERSION.VERSION;

    let kmpJson = null;
    assert.doesNotThrow(() => {
      kmpJson = kmpCompiler.transformKpsToKmpObject(kpsPath, source);
    });

    const kmpData = await kmpCompiler.buildKmpFile(kpsPath, kmpJson);

    let jszip = await zip.loadAsync(kmpData);
    // kmp file should contain the following files
    const expectedFiles = [
      'FONTLOG.txt', 'image002.png', 'KAK_Documentation_EN.pdf', 'KAK_Documentation_KH.pdf',
      'keyboard_layout.png', 'khmer_angkor.js', 'khmer_angkor.kmx', 'khmer_angkor.kvk',
      'khmer_busra_kbd.ttf', 'Mondulkiri-R.ttf', 'OFL.txt', 'OFL-FAQ.txt', 'readme.htm',
      'splash.gif', 'welcome.htm'
    ];
    expectedFiles.forEach(file => {
      assert.isNotNull(jszip.file(path.basename(file)), `should contain file ${file}`);
    });

    assert.isNotNull(jszip.file('kmp.json'));

    assert.equal(Object.entries(jszip.files).length, expectedFiles.length + 1); // +1 for kmp.json

    let kmpJsonData = JSON.parse(await jszip.file('kmp.json').async('string'));
    assert.deepEqual(kmpJsonData, kmpJsonFixture);
  });

});
