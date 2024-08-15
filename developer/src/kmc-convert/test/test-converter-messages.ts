/*
 * Keyman is copyright (C) SIL International. MIT License.
 */
import 'mocha';
import { ConverterMessages } from './converter-messages.js';
import { verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';
import { CompilerErrorNamespace } from '@keymanapp/developer-utils';

describe('ConverterMessages', function () {
  it('should have a valid ConverterMessages object', function() {
    return verifyCompilerMessagesObject(ConverterMessages, CompilerErrorNamespace.Converter);
  });
});
