import 'mocha';
import { assert } from 'chai';
import { CompilerMessages, KmnCompilerMessages } from '../src/compiler/kmn-compiler-messages.js';
import { TestCompilerCallbacks, verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';
import { makePathToFixture } from './helpers/index.js';
import { KmnCompiler } from '../src/main.js';
import { CompilerErrorNamespace } from '@keymanapp/common-types';

describe('CompilerMessages', function () {
  const callbacks = new TestCompilerCallbacks();

  it('should have a valid CompilerMessages object', function() {
    return verifyCompilerMessagesObject(CompilerMessages, CompilerErrorNamespace.KmnCompiler);
  });

  //
  // Message tests
  //

  async function testForMessage(context: Mocha.Context, fixture: string[], messageId?: number) {
    context.timeout(10000);

    callbacks.clear();

    const compiler = new KmnCompiler();
    assert(await compiler.init(callbacks));
    assert(compiler.verifyInitialized());

    const kmnPath = makePathToFixture(...fixture);

    // Note: throwing away compile results (just to memory)
    compiler.runCompiler(kmnPath, {saveDebug: true, shouldAddCompilerVersion: false});

    if(messageId) {
      assert.isTrue(callbacks.hasMessage(messageId), `messageId ${messageId.toString(16)} not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));
      assert.lengthOf(callbacks.messages, 1);
    } else {
      assert.lengthOf(callbacks.messages, 0, `messages should be empty, but instead got: `+JSON.stringify(callbacks.messages,null,2));
    }
  }

  // ERROR_InvalidKvksFile

  it('should generate ERROR_InvalidKvksFile if the kvks is not valid XML', async function() {
    await testForMessage(this, ['invalid-keyboards', 'error_invalid_kvks_file.kmn'], CompilerMessages.ERROR_InvalidKvksFile);
  });

  // WARN_InvalidVkeyInKvksFile

  it('should generate WARN_InvalidVkeyInKvksFile if the kvks contains an invalid virtual key', async function() {
    await testForMessage(this, ['invalid-keyboards', 'warn_invalid_vkey_in_kvks_file.kmn'], CompilerMessages.WARN_InvalidVkeyInKvksFile);
  });

  // ERROR_DuplicateGroup

  it('should generate CERR_DuplicateGroup if the kmn contains two groups with the same name', async function() {
    await testForMessage(this, ['invalid-keyboards', 'error_duplicate_group.kmn'], KmnCompilerMessages.ERROR_DuplicateGroup); //TODO: consolidate messages from kmcmplib, CompilerMessages.CERR_DuplicateGroup
    assert.equal(callbacks.messages[0].message, "A group with this name has already been defined. Group 'ខ្មែរ' declared on line 9");
  });

  // ERROR_DuplicateStore

  it('should generate CERR_DuplicateStore if the kmn contains two stores with the same name', async function() {
    await testForMessage(this, ['invalid-keyboards', 'error_duplicate_store.kmn'], KmnCompilerMessages.ERROR_DuplicateStore);
    assert.equal(callbacks.messages[0].message, "A store with this name has already been defined. Store 'ខ្មែរ' declared on line 11");
  });

  // ERROR_VirtualKeyInContext

  it('should generate ERROR_VirtualKeyInContext if a virtual key is found in the context part of a rule', async function() {
    await testForMessage(this, ['invalid-keyboards', 'error_virtual_key_in_context.kmn'], KmnCompilerMessages.ERROR_VirtualKeyInContext);
    assert.equal(callbacks.messages[0].message, "Virtual keys are not permitted in context");
  });

});
