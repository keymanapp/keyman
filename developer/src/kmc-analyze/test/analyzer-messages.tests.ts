/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { assert } from 'chai';
import 'mocha';
import { AnalyzerMessages } from '../src/analyzer-messages.js';
import { TestCompilerCallbacks, verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';
import { CompilerErrorNamespace } from '@keymanapp/developer-utils';
import { AnalyzeOskCharacterUse } from '../src/osk-character-use/index.js';


describe('AnalyzerMessages', function () {

  const callbacks = new TestCompilerCallbacks();

  this.beforeEach(function() {
    callbacks.clear();
  });

  this.afterEach(function() {
    if(this.currentTest?.isFailed()) {
      callbacks.printMessages();
    }
  });

  it('should have a valid AnalyzerMessages object', function() {
    return verifyCompilerMessagesObject(AnalyzerMessages, CompilerErrorNamespace.Analyzer);
  });

  it('should generate Warn_PreviousMapFileCouldNotBeLoaded when previous map file is missing or invalid', function () {
    const analyzer = new AnalyzeOskCharacterUse(callbacks);

    const invalidFile = 'nonexistent-file.json';

    assert.isNull(
      analyzer.unitTestEndPoints.loadPreviousMap(invalidFile),
      'Expected null when map file cannot be loaded'
    );

    assert.isTrue(
      callbacks.hasMessage(AnalyzerMessages.WARN_PreviousMapFileCouldNotBeLoaded),
      'Expected warning when previous map file could not be loaded');
  });
});

