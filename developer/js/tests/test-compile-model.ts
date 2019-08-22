import 'mocha';
import {assert} from 'chai';
import * as path from 'path';

import {compileModel} from '../dist/util';
import {makePathToFixture, compileModelSourceCode, CompilationResult} from './helpers';

describe('compileModel', function () {
  const MODEL_ID = 'example.qaa.trivial';
  const MODEL_DIR = makePathToFixture(MODEL_ID);

  it('should load word lists relative to the model.ts file', function () {

    let code = compileModel(path.join(MODEL_DIR, 'model.ts'));
    let r;
    assert.doesNotThrow(() => {
      r = compileModelSourceCode(code);
    });
    let compilation = r as CompilationResult;

    assert.isFalse(compilation.hasSyntaxError);
    assert.equal(compilation.modelConstructorName, 'TrieModel');
  });
});