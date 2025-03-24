import { LexicalModelCompiler } from '../src/lexical-model-compiler.js';
import {assert} from 'chai';
import 'mocha';

import { makePathToFixture, compileModelSourceCode } from './helpers/index.js';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';

describe('LexicalModelCompiler', function () {
  describe('specifying punctuation', function () {
    const MODEL_ID = 'example.qaa.trivial';
    const PATH = makePathToFixture(MODEL_ID);

    it('should compile punctuation into the generated code', async function () {
      const callbacks = new TestCompilerCallbacks();

      let compiler = new LexicalModelCompiler();
      assert.isTrue(await compiler.init(callbacks, null));
      let code = compiler.generateLexicalModelCode(MODEL_ID, {
        format: 'trie-1.0',
        sources: ['wordlist.tsv'],
        punctuation: {
          quotesForKeepSuggestion: { open: `«`, close: `»`},
          insertAfterWord: " " , // OGHAM SPACE MARK
        }
      }, PATH) as string;

      // Check that the punctuation actually made into the code:
      assert.match(code, /«/);
      assert.match(code, /»/);
      // Ensure we inserted that OGHAM SPACE MARK!
      assert.match(code, /\u1680/);

      // Make sure it compiles!
      let compilation = compileModelSourceCode(code);
      assert.isFalse(compilation.hasSyntaxError);
      assert.isNotNull(compilation.exportedModel);
    });
  })
});
