import 'mocha';
import { LdmlCompilerMessages } from '../src/compiler/ldml-compiler-messages.js';
import { verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';
import { CompilerErrorNamespace } from '@keymanapp/developer-utils';

describe('LdmlCompilerMessages', function () {
  it('should have a valid LdmlCompilerMessages object', function() {
    return verifyCompilerMessagesObject(LdmlCompilerMessages, CompilerErrorNamespace.LdmlKeyboardCompiler);
  });
});
