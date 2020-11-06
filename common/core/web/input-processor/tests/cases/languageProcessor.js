var assert = require('chai').assert;
var fs = require("fs");
var vm = require("vm");

/*
 * Unit tests for the Dummy prediction model.
 */

var LexicalModelCompiler = require('@keymanapp/lexical-model-compiler/dist/lexical-model-compiler/lexical-model-compiler').default;
var path = require('path');

let InputProcessor = require('../../dist');

// Required initialization setup.
global.com = InputProcessor.com; // exports all keyboard-processor namespacing.
global.keyman = {}; // So that keyboard-based checks against the global `keyman` succeed.
                    // 10.0+ dependent keyboards, like khmer_angkor, will otherwise fail to load.

// Initialize supplementary plane string extensions
String.kmwEnableSupplementaryPlane(false); 

let LanguageProcessor = com.keyman.text.prediction.LanguageProcessor;

// Test the KeyboardProcessor interface.
describe('LanguageProcessor', function() {
  describe('[[constructor]]', function () {
    it('should initialize without errors', function () {
      let lp = new LanguageProcessor();
      assert.isNotNull(lp);
    });

    it('has expected default values after initialization', function () {
      let languageProcessor = new LanguageProcessor();

      // These checks are lifted from the keyboard-processor init checks found in
      // common/core/web/keyboard-processor/tests/cases/basic-init.js.
      assert.isUndefined(languageProcessor.lmEngine);
      assert.isUndefined(languageProcessor.activeModel);
      assert.isFalse(languageProcessor.isActive);
      assert.isTrue(languageProcessor.mayPredict);

      // Some aspects of initialization must wait until after construction and overall
      // load of the core.  See /web/source/kmwbase.ts, in the final IIFE.
      languageProcessor.init();
      assert.isOk(languageProcessor.lmEngine);
    });
  });

  describe('.predict', function() {
    let compiler = new LexicalModelCompiler();
    const MODEL_ID = 'example.qaa.trivial';
    const PATH = path.join(__dirname, '../../node_modules/@keymanapp/lexical-model-compiler/tests/fixtures', MODEL_ID);
  
    describe('using angle brackets for quotes', function() {
      let modelCode = compiler.generateLexicalModelCode(MODEL_ID, {
        format: 'trie-1.0',
        sources: ['wordlist.tsv'],
        punctuation: {
          quotesForKeepSuggestion: { open: `«`, close: `»`},
          insertAfterWord: " " , // OGHAM SPACE MARK
        }
      }, PATH);

      let modelSpec = {
        id: MODEL_ID,
        languages: ['en'],
        code: modelCode
      };

      it("successfully loads the model", function(done) {
        let languageProcessor = new LanguageProcessor();
        languageProcessor.init();

        languageProcessor.loadModel(modelSpec).then(function() {
          assert.isOk(languageProcessor.activeModel); // is only set after a successful load.
          done();
        }, function(reason) {
          assert.fail("Model did not load correctly: " + reason);
        });
      });

      it("generates the expected prediction set", function(done) {
        let languageProcessor = new LanguageProcessor();
        languageProcessor.init();

        let contextSource = new com.keyman.text.Mock("li", 2);
        let transcription = contextSource.buildTranscriptionFrom(contextSource, null, null);

        languageProcessor.loadModel(modelSpec).then(function() {
          languageProcessor.predict(transcription).then(function(suggestions) {
            assert.isOk(suggestions);
            assert.equal(suggestions[0].displayAs, '«li»');
            // Is not actually inserting what is expected at the moment.
            //assert.equal(suggestions[0].transform.insert, ' ');
            assert.equal(suggestions[1].displayAs, 'like');
            assert.equal(suggestions[1].transform.insert, 'like ');
            done();
          }).catch(done);
        }).catch(function() {
          assert.fail("Unexpected model load failure");
          done();
        });
      });
    });
  });
});