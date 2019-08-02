import LexicalModelCompiler from '../';
import {assert} from 'chai';
import 'mocha';

import path = require('path');


describe('LexicalModelCompiler', function () {
  describe('spefifying punctuation', function () {
    const MODEL_ID = 'example.qaa.trivial';
    const PATH = path.join(__dirname, 'fixtures', MODEL_ID)

    it('should compile punctuation into the generated code', function () {
      let compiler = new LexicalModelCompiler;
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
      // TODO: more robust assertions?
    });
  })
});
