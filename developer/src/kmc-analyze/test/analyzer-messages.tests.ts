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

  it('Warn_PreviousMapFileCouldNotBeLoaded returns expected message', function () {
    const msg = AnalyzerMessages.Warn_PreviousMapFileCouldNotBeLoaded({ filename: 'file.map' });
    if (typeof msg !== 'object') throw new Error('Expected an object');
    if (!('code' in msg)) throw new Error('Expected message to have a code');
    if (typeof msg.message !== 'string') throw new Error('Expected message to have a message string');

    if (!msg.message.includes('file.map')) throw new Error('Message does not include filename');
    if (!msg.message.includes('missing or not a valid JSON')) throw new Error('Unexpected message format');
  });
});
