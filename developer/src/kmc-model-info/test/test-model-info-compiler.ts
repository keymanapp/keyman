import * as fs from 'fs';
import { assert } from 'chai';
import 'mocha';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { makePathToFixture } from './helpers/index.js';
import { writeModelMetadataFile } from '../src/model-info-compiler.js';
import { KmpCompiler } from '@keymanapp/kmc-package';

const callbacks = new TestCompilerCallbacks();

beforeEach(function() {
  callbacks.clear();
});

describe('model-info-compiler', function () {
  it('compile a .model_info file correctly', function() {
    const kpsFileName = makePathToFixture('sil.cmo.bw', 'source', 'sil.cmo.bw.model.kps');
    const kmpFileName = makePathToFixture('sil.cmo.bw', 'build', 'sil.cmo.bw.model.kmp');
    const buildModelInfoFilename = makePathToFixture('sil.cmo.bw', 'build', 'sil.cmo.bw.model_info');

    const kmpCompiler = new KmpCompiler(callbacks);
    const kmpJsonData = kmpCompiler.transformKpsToKmpObject(kpsFileName);
    const modelFileName = makePathToFixture('sil.cmo.bw', 'build', 'sil.cmo.bw.model.js');

    const data = writeModelMetadataFile(callbacks, {
      kmpFileName,
      kmpJsonData,
      model_id: 'sil.cmo.bw',
      modelFileName,
      sourcePath: 'release/sil/sil.cmo.bw'
    });
    assert.isNotNull(data);

    const actual = JSON.parse(new TextDecoder().decode(data));
    let expected = JSON.parse(fs.readFileSync(buildModelInfoFilename, 'utf-8'));

    // `lastModifiedDate` is dependent on time of run (not worth mocking)
    delete actual['lastModifiedDate'];
    delete expected['lastModifiedDate'];

    assert.deepEqual(actual, expected);
  });
});
