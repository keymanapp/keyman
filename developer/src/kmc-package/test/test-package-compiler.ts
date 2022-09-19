import 'mocha';
import * as fs from 'fs';
import {assert} from 'chai';

import KmpCompiler from '../src/kmp-compiler.js';
import {makePathToFixture} from './helpers/index.js';

describe('KmpCompiler', function () {
  const MODELS = [
    'example.qaa.sencoten',
  ];

  let kmpCompiler = new KmpCompiler();

  for (let modelID of MODELS) {
    let kpsPath = makePathToFixture(modelID, `${modelID}.model.kps`);
    let kmpJsonPath = makePathToFixture(modelID, `${modelID}.model.kmp.json`);
    let kmpJsonFixture = JSON.parse(fs.readFileSync(kmpJsonPath, 'utf-8'));

    //
    // Test just the transform from kps to kmp.json
    //
    it(`should transform ${modelID}.model.kps to kmp.json`, function () {
      let source = fs.readFileSync(kpsPath, 'utf-8');
      let kmpJson: KmpJsonFile;

      assert.doesNotThrow(() => {
        kmpJson = kmpCompiler.transformKpsToKmpObject(source);
      });

      // Test that the kmp.json data is identical
      assert.deepEqual(kmpJson, kmpJsonFixture);

      // This was used when building initial test data
      //fs.writeFileSync(kmpJsonPath, JSON.stringify(kmpJson), 'utf-8');
    });

    // TODO: end-to-end test to build .model.kmp file; however, this depends on building model and kmp.json first
    //let kmpPath = makePathToFixture(modelID, `${modelID}.model.kmp`);
  }
});
