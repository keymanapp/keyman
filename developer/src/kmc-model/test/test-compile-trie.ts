import { LexicalModelCompiler } from '../src/lexical-model-compiler.js';
import {assert} from 'chai';
import 'mocha';

import {makePathToFixture, compileModelSourceCode} from './helpers/index.js';
import { createTrieDataStructure } from '../src/build-trie.js';
import { ModelCompilerError } from '../src/model-compiler-messages.js';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';

describe('LexicalModelCompiler', function () {
  const callbacks = new TestCompilerCallbacks();
  this.beforeEach(function() {
    callbacks.clear();
  });

  describe('#generateLexicalModelCode', function () {
    it('should compile a trivial word list', async function () {
      const MODEL_ID = 'example.qaa.trivial';
      const PATH = makePathToFixture(MODEL_ID);

      let compiler = new LexicalModelCompiler();
      assert.isTrue(await compiler.init(callbacks, null));
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

      // Sanity check: the word breaker is a property of the object.
      assert.match(code, /\bwordBreaker\b["']?:\s*wordBreakers\b/);
    });

    it('should compile a word list exported by Microsoft Excel', async function () {
      const MODEL_ID = 'example.qaa.utf16le';
      const PATH = makePathToFixture(MODEL_ID);

      let compiler = new LexicalModelCompiler();
      assert.isTrue(await compiler.init(callbacks, null));
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
  });

  it('should compile a word list with a custom word breaking function', async function () {
    const MODEL_ID = 'example.qaa.trivial';
    const PATH = makePathToFixture(MODEL_ID);

    let compiler = new LexicalModelCompiler();
    assert.isTrue(await compiler.init(callbacks, null));
    let code = compiler.generateLexicalModelCode(MODEL_ID, {
      format: 'trie-1.0',
      sources: ['wordlist.tsv'],
      // This is a possible word breaking function:
      wordBreaker(phrase: string): Span[] {
        return [];
      }
    }, PATH) as string;

    let result = compileModelSourceCode(code);
    assert.isFalse(result.hasSyntaxError, `Syntax error in ${code}`);
    assert.isNotNull(result.exportedModel);
    assert.equal(result.modelConstructorName, 'TrieModel');

    // Sanity check: the word breaker is a property of the object.
    assert.match(code, /\bwordBreaker\b["']?:\s+function\b/);
  });

  it('should not generate unpaired surrogate code units', async function () {
    const MODEL_ID = 'example.qaa.smp';
    const PATH = makePathToFixture(MODEL_ID);

    let compiler = new LexicalModelCompiler();
    assert.isTrue(await compiler.init(callbacks, null));
    let code = compiler.generateLexicalModelCode(MODEL_ID, {
      format: 'trie-1.0',
      sources: ['wordlist.tsv']
    }, PATH) as string;

    let result = compileModelSourceCode(code);
    assert.isFalse(result.hasSyntaxError);
    assert.isNotNull(result.exportedModel);
    assert.equal(result.modelConstructorName, 'TrieModel');

    // Test every character in the string to make sure we don't have
    // unpaired surrogates which destroy everything.
    // We can assume that the first and last chars are not SMP
    for(var i = 1; i < code.length - 1; i++) {
      assert.notEqual(0xFFFD, code.charCodeAt(i));
      if(code.charCodeAt(i) >= 0xD800 && code.charCodeAt(i) < 0xDC00) {
        assert.isTrue((code.charCodeAt(i+1) >= 0xDC00 && code.charCodeAt(i+1) < 0xE000),
          'Unpaired lead surrogate U+'+code.charCodeAt(i).toString(16)+' at position '+i+' of \''+code+'\'');
      } else if(code.charCodeAt(i) >= 0xDC00 && code.charCodeAt(i) < 0xE000) {
        assert.isTrue((code.charCodeAt(i-1) >= 0xD800 && code.charCodeAt(i-1) < 0xDC00),
          'Unpaired trail surrogate U+'+code.charCodeAt(i).toString(16)+' at position '+i+' of \''+code+'\'');
      }
    }

    // Sanity check: the word list has three total unweighted words, with a
    // total weight of 27,596!
    assert.match(code, /\btotalWeight\b["']?:\s*27596\b/);
  });

  it('should include the source code of its search term to key function', async function () {
    const MODEL_ID = 'example.qaa.trivial';
    const PATH = makePathToFixture(MODEL_ID);
    let compiler = new LexicalModelCompiler();
    assert.isTrue(await compiler.init(callbacks, null));
    let code = compiler.generateLexicalModelCode(MODEL_ID, {
      format: 'trie-1.0',
      sources: ['wordlist.tsv']
      // NOTE: we intentionally OMIT the searchTermToKey function
      // so that the default can be provided.
    }, PATH) as string;

    assert.match(code, /(["']|)searchTermToKey\1:\s*function\b/,
      'expected to find searchTermToKey specified as a function'
    );
  });
});

describe('createTrieDataStructure()', function () {
  const WORDLIST_FILENAME = makePathToFixture('example.qaa.trivial', 'wordlist.tsv');

  it('must be given an explicit searchTermToKey function', function () {
    assert.throws(function () {
      createTrieDataStructure([WORDLIST_FILENAME]);
    }, ModelCompilerError)
  });

  it('uses the provided searchTermToKey function', function () {
    // check if the expected key is in the resultant data structure.
    // N.B., we assume the wordlist contains the wordform "turtles"
    let lowercaseSourceCode = createTrieDataStructure([WORDLIST_FILENAME], (wf) => {
      return wf.toLowerCase()
    })
    assert.match(lowercaseSourceCode, /"key":\s*"turtles"/);
    assert.notMatch(lowercaseSourceCode, /"key":\s*"TURTLES"/);

    let uppercaseSourceCode = createTrieDataStructure([WORDLIST_FILENAME], (wf) => {
      return wf.toUpperCase()
    })
    assert.match(uppercaseSourceCode, /"key":\s*"TURTLES"/);
    assert.notMatch(uppercaseSourceCode, /"key":\s*"turtles"/);
  })
});
