import 'mocha';
import { assert } from 'chai';
import { ModelCompilerMessages } from '../src/model-compiler-messages.js';
import { TestCompilerCallbacks, verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';
import { CompilerErrorNamespace } from '@keymanapp/developer-utils';
import { LexicalModelCompiler } from '../src/lexical-model-compiler.js';
import { makePathToFixture } from './helpers/index.js';

describe('ModelCompilerMessages', function () {

  const callbacks = new TestCompilerCallbacks();

  this.beforeEach(function() {
    callbacks.clear();
  });

  this.afterEach(function() {
    if(this.currentTest?.isFailed()) {
      callbacks.printMessages();
    }
  });

  it('should have a valid ModelCompilerMessages object', function() {
    return verifyCompilerMessagesObject(ModelCompilerMessages, CompilerErrorNamespace.ModelCompiler);
  });

  async function testForMessage(fixture: string[], messageId?: number) {
    const compiler = new LexicalModelCompiler();
    assert.isTrue(await compiler.init(callbacks, null));

    const modelPath = makePathToFixture(...fixture);

    // Note: throwing away compile results (just to memory)
    await compiler.run(modelPath, null);

    if(messageId) {
      assert.isTrue(callbacks.hasMessage(messageId), `messageId ${messageId.toString(16)} not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));
      assert.lengthOf(callbacks.messages, 1, `messages should have 1 entry, instead has: `+JSON.stringify(callbacks.messages,null,2));
    } else {
      assert.lengthOf(callbacks.messages, 0, `messages should be empty, but instead got: `+JSON.stringify(callbacks.messages,null,2));
    }
  }

  // ERROR_ModelFileNotFound

  it('should generate ERROR_ModelFileNotFound if a .model.ts file is missing', async function() {
    await testForMessage(
      ['invalid-models', 'missing-file.model.ts'],
      ModelCompilerMessages.ERROR_ModelFileNotFound
    );
  });

  // ERROR_WordlistFileNotFound

  it('should generate ERROR_WordlistFileNotFound if a .tsv file is missing', async function() {
    await testForMessage(
      ['invalid-models', 'example.qaa.missing-wordlist', 'example.qaa.missing-wordlist.model.ts'],
      ModelCompilerMessages.ERROR_WordlistFileNotFound
    );
  });

});
