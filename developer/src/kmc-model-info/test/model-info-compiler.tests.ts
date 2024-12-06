import * as fs from 'fs';
import { assert } from 'chai';
import 'mocha';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { makePathToFixture } from './helpers/index.js';
import { ModelInfoCompiler } from '../src/model-info-compiler.js';
import { KmpCompiler } from '@keymanapp/kmc-package';

const callbacks = new TestCompilerCallbacks();

beforeEach(function() {
  callbacks.clear();
});

describe('model-info-compiler', function () {
  it('compile a .model_info file correctly', async function() {
    const kpjFilename = makePathToFixture('sil.cmo.bw', 'sil.cmo.bw.model.kpj');
    const kpsFilename = makePathToFixture('sil.cmo.bw', 'source', 'sil.cmo.bw.model.kps');
    const kmpFileName = makePathToFixture('sil.cmo.bw', 'build', 'sil.cmo.bw.model.kmp');
    const buildModelInfoFilename = makePathToFixture('sil.cmo.bw', 'build', 'sil.cmo.bw.model_info');

    const kmpCompiler = new KmpCompiler();
    assert.isTrue(await kmpCompiler.init(callbacks, {}));
    const kmpJsonData = kmpCompiler.transformKpsToKmpObject(kpsFilename);
    const modelFileName = makePathToFixture('sil.cmo.bw', 'build', 'sil.cmo.bw.model.js');

    const sources = {
      kmpFileName,
      kmpJsonData,
      model_id: 'sil.cmo.bw',
      modelFileName,
      sourcePath: 'release/sil/sil.cmo.bw',
      kpsFilename,
      forPublishing: true,
    };
    const compiler = new ModelInfoCompiler();
    assert.isTrue(await compiler.init(callbacks, {sources}));
    const result = await compiler.run(kpjFilename, null);
    if(result == null) {
      callbacks.printMessages();
    }
    assert.isNotNull(result);

    const actual = JSON.parse(new TextDecoder().decode(result.artifacts.model_info.data));
    let expected = JSON.parse(fs.readFileSync(buildModelInfoFilename, 'utf-8'));

    // `lastModifiedDate` is dependent on time of run (not worth mocking)
    delete actual['lastModifiedDate'];
    delete expected['lastModifiedDate'];

    assert.deepEqual(actual, expected);
  });
});
