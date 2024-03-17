import 'mocha';
import { GeneratorMessages } from '../src/generator-messages.js';
import { verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';
import { CompilerErrorNamespace } from '@keymanapp/common-types';

describe('GeneratorMessages', function () {
  it('should have a valid GeneratorMessages object', function() {
    return verifyCompilerMessagesObject(GeneratorMessages, CompilerErrorNamespace.Generator);
  });
});
