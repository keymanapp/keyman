import 'mocha';
import { ModelCompilerMessages } from '../src/model-compiler-errors.js';
import { verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';

describe('ModelCompilerMessages', function () {
  it('should have a valid ModelCompilerMessages object', function() {
    return verifyCompilerMessagesObject(ModelCompilerMessages);
  });
});
