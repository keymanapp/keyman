import 'mocha';
import * as fs from 'fs';
import {assert} from 'chai';

import KmpCompiler from '../src/kmp-compiler.js';
import {makePathToFixture} from './helpers/index.js';
import JSZip from 'jszip';
import KEYMAN_VERSION from "@keymanapp/keyman-version";
import { type KmpJsonFile } from '../src/kmp-json-file.js';

describe('KmpCompiler', function () {
  const MODELS : string[] = [
    'example.qaa.sencoten',
    'withfolders.qaa.sencoten',
  ];

  let kmpCompiler = new KmpCompiler();

  for (let modelID of MODELS) {
    const kpsPath = modelID.includes('withfolders') ?
      makePathToFixture(modelID, 'source', `${modelID}.model.kps`) : makePathToFixture(modelID, `${modelID}.model.kps`);
    const jsPath = modelID.includes('withfolders') ?
      makePathToFixture(modelID, 'source', `${modelID}.model.js`) : makePathToFixture(modelID, `${modelID}.model.js`);
    const kmpJsonIntermediatePath = makePathToFixture(modelID, `${modelID}.model.kmp.intermediate.json`);
    const kmpJsonZippedPath = makePathToFixture(modelID, `${modelID}.model.kmp.zipped.json`);
    const kmpJsonIntermediateFixture = JSON.parse(fs.readFileSync(kmpJsonIntermediatePath, 'utf-8'));
    const kmpJsonZippedFixture = JSON.parse(fs.readFileSync(kmpJsonZippedPath, 'utf-8'));

    // We override the fixture version so that we can compare with the compiler output
    kmpJsonIntermediateFixture.system.keymanDeveloperVersion = KEYMAN_VERSION.VERSION;

    //
    // Test just the transform from kps to kmp.json
    //
    it(`should transform ${modelID}.model.kps to kmp.json`, function () {
      let source = fs.readFileSync(kpsPath, 'utf-8');
      let kmpJson: KmpJsonFile;

      assert.doesNotThrow(() => {
        kmpJson = kmpCompiler.transformKpsToKmpObject(source, kpsPath);
      });

      // Test that the kmp.json data is identical
      assert.deepEqual(kmpJson, kmpJsonIntermediateFixture);

      // Note that in-memory kmp.json still contains paths in the files array.
      // However, when building the .kmp, the final written kmp.json data is
      // modified to strip paths.

      // This was used when building initial test data
      //fs.writeFileSync(kmpJsonPath, JSON.stringify(kmpJson), 'utf-8');
    });
    it(`should build a full .kmp for ${modelID}`, async function() {
      const source = fs.readFileSync(kpsPath, 'utf-8');
      const zip = JSZip();
      // Build kmp.json in memory
      const kmpJson: KmpJsonFile = kmpCompiler.transformKpsToKmpObject(source, kpsPath);
      // Build file.kmp in memory
      const promise = kmpCompiler.buildKmpFile(kpsPath, kmpJson);
      promise.then(data => {
        // Check that file.kmp contains just 2 files - kmp.json and file.model.js,
        // and that they match exactly what we expect
        return zip.loadAsync(data, {checkCRC32: true}).then(zipFile => {
          assert.equal(zipFile.length, 2);
          return Promise.all([
            zipFile.file("kmp.json").async('uint8array').then(kmpJsonOutput => {
              assert.deepEqual(kmpJsonOutput, kmpJsonZippedFixture);
            }),
            zipFile.file(`${modelID}.model.js`).async('uint8array').then(modelJsFile => {
              assert.deepEqual(modelJsFile, fs.readFileSync(jsPath));
            })
          ]);
        });
      });

      return promise;
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
      kmpJson = kmpCompiler.transformKpsToKmpObject(source, kpsPath);
    });

    const kmpData = await kmpCompiler.buildKmpFile(kpsPath, kmpJson);

    const zip = JSZip();

    let jszip = await zip.loadAsync(kmpData);
    assert.isNotNull(jszip.file('kmp.json')); // kmp.json should be present
    // kmp file should contain the following files
    const expectedFiles = [
      'FONTLOG.txt', 'image002.png', 'KAK_Documentation_EN.pdf', 'KAK_Documentation_KH.pdf',
      'keyboard_layout.png', 'khmer_angkor.js', 'khmer_angkor.kmx', 'khmer_angkor.kvk',
      'khmer_busra_kbd.ttf', 'Mondulkiri-R.ttf', 'OFL.txt', 'OFL-FAQ.txt', 'readme.htm',
      'splash.gif', 'welcome.htm',
      'kmp.json', // standard .kmp metadata file
    ];

    assert.sameMembers(Object.entries(jszip.files).map(([s, o]) => o.name).sort(),
      expectedFiles.sort(),
      'khmer_angkor.kmp file should have exactly the expected files');

    let kmpJsonData = JSON.parse(await jszip.file('kmp.json').async('string'));
    assert.deepEqual(kmpJsonData, kmpJsonFixture);
  });

});
