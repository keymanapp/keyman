import 'mocha';
import { KeyboardInfoCompilerMessages } from '../src/keyboard-info-compiler-messages.js';
import { verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';
import { CompilerErrorNamespace } from '@keymanapp/common-types';

describe('KeyboardInfoCompilerMessages', function () {
  it('should have a valid KeyboardInfoCompilerMessages object', function() {
    return verifyCompilerMessagesObject(KeyboardInfoCompilerMessages, CompilerErrorNamespace.KeyboardInfoCompiler);
  });
});
