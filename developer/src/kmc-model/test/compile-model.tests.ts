import 'mocha';
import {assert} from 'chai';

import { LexicalModelCompiler } from '../src/main.js';
import {makePathToFixture, compileModelSourceCode, CompilationResult} from './helpers/index.js';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { KeymanFileTypes } from '@keymanapp/common-types';

describe('LexicalModelCompiler', function () {
  let callbacks = new TestCompilerCallbacks();

  this.timeout(5000);

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
    let modelPath = makePathToFixture(modelID, modelID + KeymanFileTypes.Source.Model);

    it(`should compile ${modelID}`, async function () {
      const compiler = new LexicalModelCompiler();
      assert.isTrue(await compiler.init(callbacks, null));
      const result = await compiler.run(modelPath, null);
      callbacks.printMessages();
      assert.isNotNull(result);
      const decoder = new TextDecoder();
      const code = decoder.decode(result.artifacts.js.data);
      let r = compileModelSourceCode(code);
      let compilation = r as CompilationResult;

      assert.isFalse(compilation.hasSyntaxError, 'model code had syntax error');
      assert.isNull(compilation.error, `compilation error: ${compilation.error}`);
      assert.equal(compilation.modelConstructorName, 'TrieModel');
    });
  }
});
