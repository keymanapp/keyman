import 'mocha';
import { verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';
import { CompilerMessages } from '../src/compiler/messages.js';

describe('CompilerMessages', function () {
  it('should have a valid CompilerMessages object', function() {
    return verifyCompilerMessagesObject(CompilerMessages);
  });
});
