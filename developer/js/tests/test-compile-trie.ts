import LexicalModelCompiler from '../';
import {assert} from 'chai';
import 'mocha';

import {makePathToFixture, compileModelSourceCode} from './helpers';


describe('LexicalModelCompiler', function () {
  describe('#generateLexicalModelCode', function () {
    it('should compile a trivial word list', function () {
      const MODEL_ID = 'example.qaa.trivial';
      const PATH = makePathToFixture(MODEL_ID);

      let compiler = new LexicalModelCompiler;
      let code = compiler.generateLexicalModelCode(MODEL_ID, {
        format: 'trie-1.0',
        sources: ['wordlist.tsv']
      }, PATH) as string;

      let result = compileModelSourceCode(code);
      assert.isFalse(result.hasSyntaxError);
      assert.isNotNull(result.exportedModel);
      assert.equal(result.modelConstructorName, 'TrieModel');

      // Sanity check: the word list has three total unweighted words, with a
      // total weight of 3!
      assert.match(code, /\btotalWeight\b["']?:\s*3\b/);
    });

    it('should compile a word list exported by Microsoft Excel', function () {
      const MODEL_ID = 'example.qaa.utf16le';
      const PATH = makePathToFixture(MODEL_ID);

      let compiler = new LexicalModelCompiler;
      let code = compiler.generateLexicalModelCode(MODEL_ID, {
        format: 'trie-1.0',
        sources: ['wordlist.txt']
      }, PATH) as string;

      let result = compileModelSourceCode(code);
      assert.isFalse(result.hasSyntaxError);
      assert.isNotNull(result.exportedModel);
      assert.equal(result.modelConstructorName, 'TrieModel');

      // Sanity check: the word list has three total unweighted words, with a
      // total weight of 44,103!
      assert.match(code, /\btotalWeight\b["']?:\s*44103\b/);
    });
  })
});
