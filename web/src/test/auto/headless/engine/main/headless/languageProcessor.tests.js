import { assert } from 'chai';

import { LanguageProcessor, TranscriptionCache } from 'keyman/engine/main';
import { SourcemappedWorker as LMWorker } from "@keymanapp/lexical-model-layer/node";
import { Mock } from 'keyman/engine/js-processor';

/*
 * Unit tests for the Dummy prediction model.
 */

import { LexicalModelCompiler } from '@keymanapp/kmc-model';
import path from 'path';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';

import { env } from 'node:process';
const KEYMAN_ROOT = env.KEYMAN_ROOT;

// Required initialization setup.
global.keyman = {}; // So that keyboard-based checks against the global `keyman` succeed.
                    // 10.0+ dependent keyboards, like khmer_angkor, will otherwise fail to load.

// Initialize supplementary plane string extensions
String.kmwEnableSupplementaryPlane(false);

// Test the KeyboardProcessor interface.
describe('LanguageProcessor', function() {
  let languageProcessor;
  const callbacks = new TestCompilerCallbacks();

  beforeEach(function() {
    languageProcessor = new LanguageProcessor(LMWorker, new TranscriptionCache());
    callbacks.clear();
  });

  afterEach(function() {
    languageProcessor.shutdown();
  });

  describe('[[constructor]]', function () {
    it('should initialize without errors', function () {
      assert.isNotNull(languageProcessor);
    });

    it('should initialize without errors even if worker construction throws an error', function () {
      let workerlessProcessor;
      try {
        workerlessProcessor = new LanguageProcessor({
          constructInstance() {
            throw new Error("Simulating a platform without Worker support");
          }
        }, new TranscriptionCache());
        assert.isFalse(workerlessProcessor.canEnable);
      } catch {
        assert.fail('Worker construction error was unhandled');
      } finally {
        workerlessProcessor?.shutdown();
      }
    })

    it('has expected default values after initialization', function () {
      // These checks are lifted from the keyboard init checks found in
      // web/src/test/auto/headless/engine/js-processor/basic-init.js.
      assert.isDefined(languageProcessor.lmEngine);
      assert.isUndefined(languageProcessor.activeModel);
      assert.isFalse(languageProcessor.isActive);
      assert.isTrue(languageProcessor.mayPredict);

      // Some aspects of initialization must wait until after construction and overall
      // load of the core.  See /web/source/kmwbase.ts, in the final IIFE.
      assert.isOk(languageProcessor.lmEngine);
    });
  });

  describe('.predict', function() {
    let compiler = null;

    this.beforeAll(async function() {
      compiler = new LexicalModelCompiler();
      assert.isTrue(await compiler.init(callbacks, {}));
    });

    const MODEL_ID = 'example.qaa.trivial';

    // ES-module mode leaves out `__dirname`, so we rebuild it using other components.
    const PATH = path.join(`${KEYMAN_ROOT}/developer/src/kmc-model/test/fixtures`, MODEL_ID);

    describe('using angle brackets for quotes', function() {
      let modelCode = null, modelSpec = null;
      this.beforeAll(function() {
        modelCode = compiler.generateLexicalModelCode(MODEL_ID, {
          format: 'trie-1.0',
          sources: ['wordlist.tsv'],
          punctuation: {
            quotesForKeepSuggestion: { open: `«`, close: `»`},
            insertAfterWord: " " , // OGHAM SPACE MARK
          }
        }, PATH);

        modelSpec = {
          id: MODEL_ID,
          languages: ['en'],
          code: modelCode
        };
      });

      it("successfully loads the model", function(done) {
        languageProcessor.loadModel(modelSpec).then(function() {
          assert.isOk(languageProcessor.activeModel); // is only set after a successful load.
          done();
        }, function(reason) {
          assert.fail("Model did not load correctly: " + reason);
        });
      });

      it("generates the expected prediction set", function(done) {
        let contextSource = new Mock("li", 2);
        let transcription = contextSource.buildTranscriptionFrom(contextSource, null, null);

        languageProcessor.loadModel(modelSpec).then(function() {
          languageProcessor.predict(transcription).then(function(suggestions) {
            assert.isOk(suggestions);
            assert.equal(suggestions[0].displayAs, '«li»');
            assert.equal(suggestions[0].transform.insert, 'li ');
            assert.equal(suggestions[1].displayAs, 'like');
            assert.equal(suggestions[1].transform.insert, 'like ');
            done();
          }).catch(done);
        }).catch(function() {
          assert.fail("Unexpected model load failure");
          done();
        });
      });

      describe('properly cases generated suggestions', function() {
        let modelCode = null, modelSpec = null;
        this.beforeAll(function () {
          modelCode = compiler.generateLexicalModelCode(MODEL_ID, {
            format: 'trie-1.0',
            sources: ['wordlist.tsv'],
            languageUsesCasing: true,
            //applyCasing // we rely on the compiler's default implementation here.
          }, PATH);

          modelSpec = {
            id: MODEL_ID,
            languages: ['en'],
            code: modelCode
          };
        });

        describe("does not alter casing when input is lowercased", function() {
          it("when input is fully lowercased", function(done) {
            let contextSource = new Mock("li", 2);
            let transcription = contextSource.buildTranscriptionFrom(contextSource, null, null);

            languageProcessor.loadModel(modelSpec).then(function() {
              languageProcessor.predict(transcription).then(function(suggestions) {
                assert.isOk(suggestions);
                assert.equal(suggestions[1].displayAs, 'like');
                assert.equal(suggestions[1].transform.insert, 'like ');
                done();
              }).catch(done);
            }).catch(function() {
              assert.fail("Unexpected model load failure");
              done();
            });
          });

          it("when input has non-initial uppercased letters", function(done) {
            let contextSource = new Mock("lI", 2);
            let transcription = contextSource.buildTranscriptionFrom(contextSource, null, null);

            languageProcessor.loadModel(modelSpec).then(function() {
              languageProcessor.predict(transcription).then(function(suggestions) {
                // The source suggestion is simply 'like'.
                assert.isOk(suggestions);
                assert.equal(suggestions[1].displayAs, 'like');
                assert.equal(suggestions[1].transform.insert, 'like ');
                done();
              }).catch(done);
            }).catch(function() {
              assert.fail("Unexpected model load failure");
              done();
            });
          });

          it("unless the suggestion has uppercased letters", function(done) {
            let contextSource = new Mock("i", 1);
            let transcription = contextSource.buildTranscriptionFrom(contextSource, null, null);

            languageProcessor.loadModel(modelSpec).then(function() {
              languageProcessor.predict(transcription).then(function(suggestions) {
                assert.isOk(suggestions);
                assert.equal(suggestions[1].displayAs, 'I');
                assert.equal(suggestions[1].transform.insert, 'I ');
                done();
              }).catch(done);
            }).catch(function() {
              assert.fail("Unexpected model load failure");
              done();
            });
          });
        });

        describe("uppercases suggestions when input is fully capitalized ", function() {
          it("for suggestions with default casing  (== 'lower')", function(done) {
            let contextSource = new Mock("LI", 2);
            let transcription = contextSource.buildTranscriptionFrom(contextSource, null, null);

            languageProcessor.loadModel(modelSpec).then(function() {
              languageProcessor.predict(transcription).then(function(suggestions) {
                // The source suggestion is simply 'like'.
                assert.isOk(suggestions);
                assert.equal(suggestions[1].displayAs, 'LIKE');
                assert.equal(suggestions[1].transform.insert, 'LIKE ');
                done();
              }).catch(done);
            }).catch(function() {
              assert.fail("Unexpected model load failure");
              done();
            });
          });

          it("for precapitalized suggestions", function(done) {
            let contextSource = new Mock("I", 1);
            let transcription = contextSource.buildTranscriptionFrom(contextSource, null, null);

            languageProcessor.loadModel(modelSpec).then(function() {
              languageProcessor.predict(transcription).then(function(suggestions) {
                assert.isOk(suggestions);
                assert.equal(suggestions[0].displayAs, 'I');
                assert.equal(suggestions[0].transform.insert, 'I ');
                done();
              }).catch(done);
            }).catch(function() {
              assert.fail("Unexpected model load failure");
              done();
            });
          });
        });

        describe("initial-cases suggestions when input uses initial casing ", function() {
          describe("when input is a single capitalized letter", function() {
            it("for suggestions with default casing (== 'lower')", function(done) {
              let contextSource = new Mock("L", 1);
              let transcription = contextSource.buildTranscriptionFrom(contextSource, null, null);

              languageProcessor.loadModel(modelSpec).then(function() {
                languageProcessor.predict(transcription).then(function(suggestions) {
                  // The source suggestion is simply 'like'.
                  assert.isOk(suggestions);
                  assert.equal(suggestions[1].displayAs, 'Like');
                  assert.equal(suggestions[1].transform.insert, 'Like ');
                  done();
                }).catch(done);
              }).catch(function() {
                assert.fail("Unexpected model load failure");
                done();
              });
            });
          });

          describe("input length > 1", function() {
            it("for suggestions with default casing (== 'lower')", function(done) {
              let contextSource = new Mock("Li", 2);
              let transcription = contextSource.buildTranscriptionFrom(contextSource, null, null);

              languageProcessor.loadModel(modelSpec).then(function() {
                languageProcessor.predict(transcription).then(function(suggestions) {
                  // The source suggestion is simply 'like'.
                  assert.isOk(suggestions);
                  assert.equal(suggestions[1].displayAs, 'Like');
                  assert.equal(suggestions[1].transform.insert, 'Like ');
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
    });
  });
});