import { LexicalModelCompiler } from '../src/lexical-model-compiler.js';
import {assert} from 'chai';
import 'mocha';

import { makePathToFixture, compileModelSourceCode } from './helpers/index.js';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';

describe('LexicalModelCompiler - pseudoclosure compilation + use', function () {
  const callbacks = new TestCompilerCallbacks();

  const MODEL_ID = 'example.qaa.trivial';
  const PATH = makePathToFixture(MODEL_ID);

  describe('specifying custom methods: applyCasing and searchTermToKey', function () {
    let casingWithPrependedSymbols: CasingFunction = function(casingName: CasingForm, text: string, defaultApplyCasing: CasingFunction) {
      switch(casingName) {
        // Use of symbols, and of the `casingName` name, exist to serve as regex targets.
        case 'lower':
          return '-' + defaultApplyCasing(casingName, text);
        case 'upper':
          return '+' + defaultApplyCasing(casingName, text);
        case 'initial':
          return '^' + defaultApplyCasing(casingName, text);
        default:
          return defaultApplyCasing(casingName, text);
      }
    };

    it('variant 1:  applyCasing prepends symbols, searchTermToKey removes them', async function() {
      let compiler = new LexicalModelCompiler();
      assert.isTrue(await compiler.init(callbacks, null));
      let code = compiler.generateLexicalModelCode(MODEL_ID, {
        format: 'trie-1.0',
        sources: ['wordlist.tsv'],
        languageUsesCasing: true, // applyCasing won't appear without this!
        applyCasing: casingWithPrependedSymbols,
        // Parameter name `rawSearchTerm` selected for uniqueness, regex matching test target.
        searchTermToKey: function(rawSearchTerm: string, applyCasing: CasingFunction) {
          // Strips any applyCasing symbols ('-', '+', '^') out of the compiled Trie.
          // We should be able to test that they do _not_ occur within internal nodes of the Trie.
          return applyCasing('lower', rawSearchTerm)
            // We replace all prepended applyCasing symbols with a separate symbol not
            // otherwise seen within the model.
            .replace(/^-/, '§')
            .replace(/^\+/, '§')
            .replace(/^\^/, '§');
        }
      }, PATH) as string;

      // Check that pseudoclosure forwarding is set up correctly.
      assert.match(code, /\.model\.searchTermToKey/);
      assert.match(code, /\.model\.applyCasing/);

      // Check that the methods actually made into the code; use our custom parameter names:
      assert.match(code, /casingName/);
      assert.match(code, /rawSearchTerm/);

      // Check that the prepended symbols from applyCasing also appear in the code:
      assert.match(code, /'-'/);
      assert.match(code, /'\+'/);
      assert.match(code, /'\^'/);
      // From searchTermToKey:
      assert.match(code, /'§'/);

      let modelInitIndex = code.indexOf('LMLayerWorker.loadModel');
      let modelInitCode = code.substring(modelInitIndex);
      // Chop off the IIFE terminator.  Not strictly necessary, but whatever.
      modelInitCode = modelInitCode.substring(0, modelInitCode.lastIndexOf('\n})();'))

      // Check that the prepended symbols from applyCasing do not appear in the Trie.
      assert.notMatch(modelInitCode, /['"]-['"]/);
      assert.notMatch(modelInitCode, /['"]\+['"]/);
      assert.notMatch(modelInitCode, /['"]\^['"]/);

      // Instead, our custom keyer should ensure that the following symbol DOES appear.
      // Verifies that the compiler uses the custom searchTermToKey definition.
      assert.match(modelInitCode, /[^ ]§/);

      // Make sure it compiles!
      let compilation = compileModelSourceCode(code);
      assert.isFalse(compilation.hasSyntaxError);
      assert.isNotNull(compilation.exportedModel);
    });

    it('variant 2:  applyCasing prepends symbols, searchTermToKey keeps them', async function() {
      let compiler = new LexicalModelCompiler();
      assert.isTrue(await compiler.init(callbacks, null));
      let code = compiler.generateLexicalModelCode(MODEL_ID, {
        format: 'trie-1.0',
        sources: ['wordlist.tsv'],
        languageUsesCasing: true, // applyCasing won't appear without this!
        applyCasing: casingWithPrependedSymbols,
        // Parameter name `rawSearchTerm` selected for uniqueness, regex matching test target.
        searchTermToKey: function(rawSearchTerm: string, applyCasing: CasingFunction) {
          // Strips any applyCasing symbols ('-', '+', '^') out of the compiled Trie.
          // We should be able to test that they do _not_ occur within internal nodes of the Trie.
          return applyCasing('lower', rawSearchTerm);
        }
      }, PATH) as string;

      // Check that pseudoclosure forwarding is set up correctly.
      assert.match(code, /\.model\.searchTermToKey/);
      assert.match(code, /\.model\.applyCasing/);

      // Check that the methods actually made into the code; use our custom parameter names:
      assert.match(code, /casingName/);
      assert.match(code, /rawSearchTerm/);

      // Check that the prepended symbols from applyCasing also appear in the code:
      assert.match(code, /'-'/);
      assert.match(code, /'\+'/);
      assert.match(code, /'\^'/);

      let modelInitIndex = code.indexOf('LMLayerWorker.loadModel');
      let modelInitCode = code.substring(modelInitIndex);
      // Chop off the IIFE terminator.  Not strictly necessary, but whatever.
      modelInitCode = modelInitCode.substring(0, modelInitCode.lastIndexOf('\n})();'))

      // Check that the prepended lowercase "-" DOES appear within the Trie, as keying
      // does not remove it in this variant.  Verifies that the compiler actually
      // used the custom applyCasing definition!
      assert.match(modelInitCode, /[^ ]-/); // ' -' is indicative of a compressed number

      // Make sure it compiles!
      let compilation = compileModelSourceCode(code);
      assert.isFalse(compilation.hasSyntaxError);
      assert.isNotNull(compilation.exportedModel);
    });

    it('variant 3:  applyCasing prepends symbols, default searchTermToKey', async function() {
      let compiler = new LexicalModelCompiler();
      assert.isTrue(await compiler.init(callbacks, null));
      let code = compiler.generateLexicalModelCode(MODEL_ID, {
        format: 'trie-1.0',
        sources: ['wordlist.tsv'],
        languageUsesCasing: true, // applyCasing won't appear without this!
        applyCasing: casingWithPrependedSymbols
      }, PATH) as string;

      // Check that pseudoclosure forwarding is set up correctly.
      assert.match(code, /\.model\.searchTermToKey/);
      assert.match(code, /\.model\.applyCasing/);

      // Check that the methods actually made into the code; use our custom parameter names:
      assert.match(code, /casingName/);
      assert.match(code, /function defaultCasedSearchTermToKey/);

      // Check that the prepended symbols from applyCasing also appear in the code:
      assert.match(code, /'-'/);
      assert.match(code, /'\+'/);
      assert.match(code, /'\^'/);

      let modelInitIndex = code.indexOf('LMLayerWorker.loadModel');
      let modelInitCode = code.substring(modelInitIndex);
      // Chop off the IIFE terminator.  Not strictly necessary, but whatever.
      modelInitCode = modelInitCode.substring(0, modelInitCode.lastIndexOf('\n})();'))

      // Check that the prepended lowercase "-" DOES appear within the Trie, as keying
      // does not remove it in this variant.  Verifies that the compiler actually
      // used the custom applyCasing definition!
      assert.match(modelInitCode, /[^ ]-/); // ' -' is indicative of a compressed number

      // Make sure it compiles!
      let compilation = compileModelSourceCode(code);
      assert.isFalse(compilation.hasSyntaxError);
      assert.isNotNull(compilation.exportedModel);
    });
  });

  describe('relying on default applyCasing + searchTermToKey', function() {
    it('languageUsesCasing: true', async function() {
      let compiler = new LexicalModelCompiler();
      assert.isTrue(await compiler.init(callbacks, null));
      let code = compiler.generateLexicalModelCode(MODEL_ID, {
        format: 'trie-1.0',
        sources: ['wordlist.tsv'],
        languageUsesCasing: true, // applyCasing should appear!
      }, PATH) as string;

      // Does the cased version of the search-term default appear?
      assert.match(code, /function defaultCasedSearchTermToKey/);
      assert.match(code, /\.model\.searchTermToKey/);

      // Does the default casing function appear?
      assert.match(code, /function defaultApplyCasing/);
      assert.match(code, /\.defaults\.applyCasing/);

      assert.match(code, /languageUsesCasing/);

      // Make sure it compiles!
      let compilation = compileModelSourceCode(code);
      assert.isFalse(compilation.hasSyntaxError);
      assert.isNotNull(compilation.exportedModel);
    });

    it('languageUsesCasing: false', async function() {
      let compiler = new LexicalModelCompiler();
      assert.isTrue(await compiler.init(callbacks, null));
      let code = compiler.generateLexicalModelCode(MODEL_ID, {
        format: 'trie-1.0',
        sources: ['wordlist.tsv'],
        languageUsesCasing: false, // applyCasing should not appear!
      }, PATH) as string;

      // Does the cased version of the search-term default appear?
      assert.match(code, /function defaultSearchTermToKey/);
      assert.match(code, /\.model\.searchTermToKey/);

      // Does the default casing function appear?
      // Since `languageUsesCasing` is explicitly `false`, there should be no usage.
      assert.notMatch(code, /function defaultApplyCasing/);
      assert.notMatch(code, /\.defaults\.applyCasing/);

      assert.match(code, /languageUsesCasing/);

      // Make sure it compiles!
      let compilation = compileModelSourceCode(code);
      assert.isFalse(compilation.hasSyntaxError);
      assert.isNotNull(compilation.exportedModel);
    });

    it('languageUsesCasing: undefined', async function() {
      let compiler = new LexicalModelCompiler();
      assert.isTrue(await compiler.init(callbacks, null));
      let code = compiler.generateLexicalModelCode(MODEL_ID, {
        format: 'trie-1.0',
        sources: ['wordlist.tsv']
        // languageUsesCasing: undefined
      }, PATH) as string;

      // Does the cased version of the search-term default appear?
      // Needed to match 14.0 defaults.
      assert.match(code, /function defaultCasedSearchTermToKey/);
      assert.match(code, /\.model\.searchTermToKey/);

      // Does the default casing function appear?
      assert.match(code, /function defaultApplyCasing/);
      assert.match(code, /\.defaults\.applyCasing/);

      // Did the compilation process explictly leave `languageUsesCasing` undefined?
      assert.notMatch(code, /languageUsesCasing/);

      // Make sure it compiles!
      let compilation = compileModelSourceCode(code);
      assert.isFalse(compilation.hasSyntaxError);
      assert.isNotNull(compilation.exportedModel);
    });
  })
});
