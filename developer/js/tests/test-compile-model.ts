import 'mocha';
import {assert} from 'chai';

import {compileModel} from '../dist/util';
import {makePathToFixture, compileModelSourceCode, CompilationResult} from './helpers';

describe('compileModel', function () {
  const MODEL_ID = 'example.qaa.trivial';
  const MODEL_PATH = makePathToFixture(MODEL_ID, `${MODEL_ID}.model.ts`);

  it('should load word lists relative to the model.ts file', function () {

    let code = compileModel(MODEL_PATH);
    let r;
    assert.doesNotThrow(() => {
      r = compileModelSourceCode(code);
    });
    let compilation = r as CompilationResult;

    assert.isFalse(compilation.hasSyntaxError);
    assert.equal(compilation.modelConstructorName, 'TrieModel');
  });
});
