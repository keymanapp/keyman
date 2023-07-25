import 'mocha';
import { CompilerMessages } from '../src/compiler/messages.js';
import { verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';
import { CompilerErrorNamespace } from '@keymanapp/common-types';

describe('CompilerMessages', function () {
  it('should have a valid CompilerMessages object', function() {
    return verifyCompilerMessagesObject(CompilerMessages, CompilerErrorNamespace.LdmlKeyboardCompiler);
  });
});
