import 'mocha';
import { ModelCompilerMessages } from '../src/model-compiler-messages.js';
import { verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';
import { CompilerErrorNamespace } from '@keymanapp/common-types';

describe('ModelCompilerMessages', function () {
  it('should have a valid ModelCompilerMessages object', function() {
    return verifyCompilerMessagesObject(ModelCompilerMessages, CompilerErrorNamespace.ModelCompiler);
  });
});
