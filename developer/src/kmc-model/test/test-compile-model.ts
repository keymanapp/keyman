import 'mocha';
import {assert} from 'chai';

import {compileModel} from '../src/main.js';
import {makePathToFixture, compileModelSourceCode, CompilationResult} from './helpers/index.js';

describe('compileModel', function () {
  // Try to compile ALL of the correct models.
  const MODELS = [
    'example.qaa.sencoten',
    'example.qaa.trivial',
    'example.qaa.utf16le',
    'example.qaa.wordbreaker',
    'example.qaa.joinwordbreaker',
    'example.qaa.scriptusesspaces',
  ];

  for (let modelID of MODELS) {
    let modelPath = makePathToFixture(modelID, `${modelID}.model.ts`);

    it(`should compile ${modelID}`, function () {
      let code = compileModel(modelPath);
      let r: unknown;
      assert.doesNotThrow(() => {
        r = compileModelSourceCode(code);
      });
      let compilation = r as CompilationResult;

      assert.isFalse(compilation.hasSyntaxError, 'model code had syntax error');
      assert.isNull(compilation.error, `compilation error: ${compilation.error}`);
      assert.equal(compilation.modelConstructorName, 'TrieModel');
    });
  }
});
