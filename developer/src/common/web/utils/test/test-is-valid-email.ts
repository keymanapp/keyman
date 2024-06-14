import { assert } from 'chai';
import 'mocha';
import { isValidEmail } from '../src/is-valid-email.js';

describe('test-is-valid-email', function () {
  it('should accept a valid email address', function() {
    [
      'email@example.com',
      'email.example-true@example.com',
      'email@example-example.com',
      'email...@example.com',
      'email_example1@1example.com',
      'email+example@mail.example.com',
      'Email@Example.Com'
    ].forEach(email => assert.isTrue(isValidEmail(email), `expected '${email}' to be valid`));

    // This is accepted, but it's really a bit wonky. But that's an upstream
    // issue with overly lax regex and not something we'll attempt to fix:
    //
    // assert.isTrue(isValidEmail('.@example.com'));
  });

  it('should reject invalid email addresses', function() {
    [
      'email@example.com, email2@example.com',
      '<Mr Email> email@example.com',
      'email@example_domain.com',
      'email',
      'email@.',
      'email@example..com',
      'email@',
      '@example',
    ].forEach(email => assert.isFalse(isValidEmail(email), `expected '${email}' to be invalid`));
  });
});
