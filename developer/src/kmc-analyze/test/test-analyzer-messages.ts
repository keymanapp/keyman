/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
// import { assert } from 'chai';
import 'mocha';
import { AnalyzerMessages } from '../src/analyzer-messages.js';
import { TestCompilerCallbacks, verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';
import { CompilerErrorNamespace } from '@keymanapp/developer-utils';

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

  // TODO: test each message
});
