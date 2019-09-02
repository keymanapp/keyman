import 'mocha';
import {assert} from 'chai';

import {compileModel} from '../dist/util/util';
import {makePathToFixture, compileModelSourceCode, CompilationResult} from './helpers';

describe('compileModel', function () {
  // Try to compile ALL of the correct models.
  const MODELS = [
    'example.qaa.sencoten',
    'example.qaa.trivial',
    'example.qaa.utf16le',
  ];

  for (let modelID of MODELS) {
    let modelPath = makePathToFixture(modelID, `${modelID}.model.ts`);

    it(`should compile ${modelID}`, function () {
      let code = compileModel(modelPath);
      let r;
      assert.doesNotThrow(() => {
        r = compileModelSourceCode(code);
      });
      let compilation = r as CompilationResult;

      assert.isFalse(compilation.hasSyntaxError);
      assert.isNull(compilation.error);
      assert.equal(compilation.modelConstructorName, 'TrieModel');
    });
  }
});
