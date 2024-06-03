import 'mocha';
import { assert } from 'chai';
import { KmwCompilerMessages } from '../../src/kmw-compiler/kmw-compiler-messages.js';
import { TestCompilerCallbacks, verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';
import { makePathToFixture } from '../helpers/index.js';
import { KmnCompiler } from '../../src/main.js';
import { CompilerErrorNamespace } from '@keymanapp/common-types';

describe('KmwCompilerMessages', function () {
  const callbacks = new TestCompilerCallbacks();

  it('should have a valid KmwCompilerMessages object', function() {
    return verifyCompilerMessagesObject(KmwCompilerMessages, CompilerErrorNamespace.KmwCompiler);
  });

  //
  // Message tests
  //

  async function testForMessage(context: Mocha.Context, fixture: string[], messageId?: number) {
    context.timeout(10000);

    callbacks.clear();

    const compiler = new KmnCompiler();
    assert(await compiler.init(callbacks, {saveDebug: true, shouldAddCompilerVersion: false}));
    assert(compiler.verifyInitialized());

    const kmnPath = makePathToFixture(...fixture);

    // Note: throwing away compile results (just to memory)
    await compiler.run(kmnPath, null);

    if(messageId) {
      assert.isTrue(callbacks.hasMessage(messageId), `messageId ${messageId.toString(16)} not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));
      assert.lengthOf(callbacks.messages, 1);
    } else {
      assert.lengthOf(callbacks.messages, 0, `messages should be empty, but instead got: `+JSON.stringify(callbacks.messages,null,2));
    }
  }

  // HINT_TouchLayoutUsesUnsupportedGesturesDownlevel

  it('should generate HINT_TouchLayoutUsesUnsupportedGesturesDownlevel if the touch layout has gestures but the keyboard is an old version', async function() {
    await testForMessage(this, ['kmw', 'validate_gesture.kmn'], KmwCompilerMessages.HINT_TouchLayoutUsesUnsupportedGesturesDownlevel);
  });

  // TODO: other messages

  // ERROR_NotAnyRequiresVersion14

  // it('should generate ERROR_NotAnyRequiresVersion14 if ...', async function() {
  //   await testForMessage(this, ['kmw', '....kmn'], KmwCompilerMessages.ERROR_NotAnyRequiresVersion14);
  // });

  // ERROR_TouchLayoutIdentifierRequires15

  // it('should generate ERROR_TouchLayoutIdentifierRequires15 if ...', async function() {
  //   await testForMessage(this, ['kmw', '....kmn'], KmwCompilerMessages.ERROR_TouchLayoutIdentifierRequires15);
  // });

  // ERROR_InvalidTouchLayoutFileFormat

  // it('should generate ERROR_InvalidTouchLayoutFileFormat if ...', async function() {
  //   await testForMessage(this, ['kmw', '....kmn'], KmwCompilerMessages.ERROR_InvalidTouchLayoutFileFormat);
  // });

  // ERROR_TouchLayoutFileDoesNotExist

  // it('should generate ERROR_TouchLayoutFileDoesNotExist if ...', async function() {
  //   await testForMessage(this, ['kmw', '....kmn'], KmwCompilerMessages.ERROR_TouchLayoutFileDoesNotExist);
  // });

  // ERROR_NotSupportedInKeymanWebOutput

  it('should generate ERROR_NotSupportedInKeymanWebOutput if the command is not supported in output for KeymanWeb', async function() {
    await testForMessage(this, ['kmw', 'error_not_supported_in_keyman_web_output.kmn'], KmwCompilerMessages.ERROR_NotSupportedInKeymanWebOutput);
  });


});
