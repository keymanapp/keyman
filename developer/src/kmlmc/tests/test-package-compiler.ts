import 'mocha';
import * as fs from 'fs';
import * as JSZip from 'jszip';
import {assert} from 'chai';

import KmpCompiler from '../dist/package-compiler/kmp-compiler';
import {makePathToFixture} from './helpers';

let zip = JSZip();

describe('KmpCompiler', function () {
  const MODELS = [
    'example.qaa.sencoten',
    'withfolders.qaa.sencoten'
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

      // This was used when building initial test data
      //fs.writeFileSync(kmpJsonPath, JSON.stringify(kmpJson), 'utf-8');
    });

    it(`should build a full .kmp for ${modelID}`, async function() {
      const source = fs.readFileSync(kpsPath, 'utf-8');
      // Build kmp.json in memory
      const kmpJson: KmpJsonFile = kmpCompiler.transformKpsToKmpObject(source, kpsPath);
      // Build file.kmp in memory
      const promise = kmpCompiler.buildKmpFile(kmpJson);
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
});
