import 'mocha';
import { ModelInfoCompilerMessages } from '../src/model-info-compiler-messages.js';
import { verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';
import { CompilerErrorNamespace } from '@keymanapp/common-types';

describe('ModelInfoCompilerMessages', function () {
  it('should have a valid ModelInfoCompilerMessages object', function() {
    return verifyCompilerMessagesObject(ModelInfoCompilerMessages, CompilerErrorNamespace.ModelInfoCompiler);
  });
});
