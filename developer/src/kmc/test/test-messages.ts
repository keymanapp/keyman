import 'mocha';
import { InfrastructureMessages } from '../src/messages/messages.js';
import { verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';

describe('InfrastructureMessages', function () {
  it('should have a valid InfrastructureMessages object', function() {
    return verifyCompilerMessagesObject(InfrastructureMessages);
  });
});
