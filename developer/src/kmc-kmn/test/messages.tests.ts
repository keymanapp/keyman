import 'mocha';
import { assert } from 'chai';
import { KmnCompilerMessageRanges, KmnCompilerMessages } from '../src/compiler/kmn-compiler-messages.js';
import { TestCompilerCallbacks, verifyCompilerMessagesObject } from '@keymanapp/developer-test-helpers';
import { makePathToFixture } from './helpers/index.js';
import { KmnCompiler } from '../src/main.js';
import { CompilerErrorMask, CompilerErrorNamespace } from '@keymanapp/developer-utils';

describe('KmnCompilerMessages', function () {
  const callbacks = new TestCompilerCallbacks();

  it('should have a valid KmnCompilerMessages object', function() {
    return verifyCompilerMessagesObject(KmnCompilerMessages, CompilerErrorNamespace.KmnCompiler);
  });

  it('should have a 1:1 correspondence with kmn_compiler_errors.h', function() {

    const headerFilename = 'kmn_compiler_errors.h';
    const tsFilename = 'kmn-compiler-messages.ts';

    // For each message declared in KmnCompilerMessages, look for a
    // corresponding message in kmn_compiler_errors.h
    const headerPath = makePathToFixture(`../../../common/include/${headerFilename}`);

    // Matching the following line pattern from kmn_compiler_errors.h:
    //
    //   FATAL_BadCallParams =                                 SevFatal | 0x002,
    const messageRegex = /^\s+((FATAL|ERROR|WARN|HINT|INFO)_[a-z0-9_]+)\s+=\s+Sev(Fatal|Error|Warn|Hint|Info)\s+\|\s+0x([0-9a-f]+)/i;

    // Convert matching lines into array of message code data
    const cppMessages = callbacks.fs.readFileSync(headerPath, 'utf-8')
      .replace(/\r/g, '')
      .split('\n')
      .filter(line => line.match(messageRegex)) // filter first, run regex twice, ;-)
      .map(line => {
        const m = line.match(messageRegex);
        return { name: m[1], nameSeverity: m[2], maskSeverity: m[3], code: parseInt(m[4],16), found: false }
      });

    const cppMessagesByName: {[index:string]: {name: string, nameSeverity: string, maskSeveity: string, code: number}} = cppMessages
      .reduce((obj, m) => {obj[m.name] = m; return obj}, {} as any);

    // We validate that the nameSeverity and maskSeverity line up, as a sanity
    // check. This conceptually belongs in kmcmplib tests, but much easier to
    // achieve here
    cppMessages.forEach(m => {
      assert.strictEqual(m.maskSeverity.toUpperCase(), m.nameSeverity, `${headerFilename} has mismatching severity for ${m.name} and ${m.maskSeverity}`)
    });

    // Use same pattern as verifyCompilerMessagesObject to get list of messages
    // from KmnCompilerMessages
    const keys = Object.keys(KmnCompilerMessages);
    const m = KmnCompilerMessages as Record<string,any>;

    for(const key of keys) {
      // Verify each object member matches the pattern we expect
      if(typeof m[key] == 'number') {

        if((m[key] & CompilerErrorMask.BaseError) < KmnCompilerMessageRanges.RANGE_KMN_COMPILER_MIN ||
          (m[key] & CompilerErrorMask.BaseError) > KmnCompilerMessageRanges.RANGE_KMN_COMPILER_MAX) {
          // Message is not from kmcmplib
          continue;
        }

        const cppMessage = cppMessagesByName[key];
        assert.isDefined(cppMessage, `${key} in ${tsFilename} not found in ${headerFilename}`);
        assert.equal(cppMessage.name, key, `${key} name in ${tsFilename} does not precisely match ${headerFilename}`);
        assert.equal(cppMessage.code, m[key] & CompilerErrorMask.BaseError, `${key} in ${tsFilename} has a different value than ${headerFilename}`);

        // Remove the entry so we can spot unpaired messages afterwards
        delete cppMessagesByName[key];
      }
    }

    // Ensure there are no remaining messages in kmn_compiler_errors.h
    assert.isEmpty(Object.keys(cppMessagesByName), `${headerFilename} has entries not found in ${tsFilename}: ${JSON.stringify(cppMessagesByName)}`);
  });

  //
  // Message tests
  //

  async function testForMessage(context: Mocha.Context, fixture: string[], messageId?: number) {
    context.timeout(10000);

    callbacks.clear();

    const compiler = new KmnCompiler();
    assert(await compiler.init(callbacks, {saveDebug: true, shouldAddCompilerVersion: false}));
    assert(compiler.verifyInitialized());

    const kmnPath = makePathToFixture(...fixture);

    // Note: throwing away compile results (just to memory)
    await compiler.run(kmnPath, null);

    if(messageId) {
      assert.isTrue(callbacks.hasMessage(messageId), `messageId ${messageId.toString(16)} not generated, instead got: `+JSON.stringify(callbacks.messages,null,2));
      assert.lengthOf(callbacks.messages, 1, `messages should have 1 entry, instead has: `+JSON.stringify(callbacks.messages,null,2));
    } else {
      assert.lengthOf(callbacks.messages, 0, `messages should be empty, but instead got: `+JSON.stringify(callbacks.messages,null,2));
    }
  }

  // ERROR_InvalidKvksFile

  it('should generate ERROR_InvalidKvksFile if the kvks is not valid XML', async function() {
    await testForMessage(this, ['invalid-keyboards', 'error_invalid_kvks_file.kmn'], KmnCompilerMessages.ERROR_InvalidKvksFile);
  });

  // WARN_InvalidVkeyInKvksFile

  it('should generate WARN_InvalidVkeyInKvksFile if the kvks contains an invalid virtual key', async function() {
    await testForMessage(this, ['invalid-keyboards', 'warn_invalid_vkey_in_kvks_file.kmn'], KmnCompilerMessages.WARN_InvalidVkeyInKvksFile);
  });

  // ERROR_DuplicateGroup

  it('should generate ERROR_DuplicateGroup if the kmn contains two groups with the same name', async function() {
    await testForMessage(this, ['invalid-keyboards', 'error_duplicate_group.kmn'], KmnCompilerMessages.ERROR_DuplicateGroup);
  });

  // ERROR_DuplicateStore

  it('should generate ERROR_DuplicateStore if the kmn contains two stores with the same name', async function() {
    await testForMessage(this, ['invalid-keyboards', 'error_duplicate_store.kmn'], KmnCompilerMessages.ERROR_DuplicateStore);
  });

  // ERROR_VirtualKeyInContext

  it('should generate ERROR_VirtualKeyInContext if a virtual key is found in the context part of a rule', async function() {
    await testForMessage(this, ['invalid-keyboards', 'error_virtual_key_in_context.kmn'], KmnCompilerMessages.ERROR_VirtualKeyInContext);
  });

  // WARN_TouchLayoutUnidentifiedKey

  it('should generate WARN_TouchLayoutUnidentifiedKey if a key has no identifier in the touch layout', async function() {
    await testForMessage(this, ['invalid-keyboards', 'warn_touch_layout_unidentified_key.kmn'], KmnCompilerMessages.WARN_TouchLayoutUnidentifiedKey);
    // TODO(lowpri): that message could be slightly more helpful!
  });

  // ERROR_NotSupportedInKeymanWebOutput

  it('should generate ERROR_NotSupportedInKeymanWebOutput if a rule has `return` in the output', async function() {
    await testForMessage(this, ['invalid-keyboards', 'error_not_supported_in_keyman_web_output.kmn'], KmnCompilerMessages.ERROR_NotSupportedInKeymanWebOutput);
  });

  // WARN_VirtualKeyInOutput

  it('should generate WARN_VirtualKeyInOutput if a virtual key is found in the output part of a rule', async function() {
    await testForMessage(this, ['invalid-keyboards', 'warn_virtual_key_in_output.kmn'], KmnCompilerMessages.WARN_VirtualKeyInOutput);
  });

  // ERROR_OutsTooLong

  it('should generate ERROR_OutsTooLong if a store referenced in outs() is too long and would overflow the buffer', async function() {
    await testForMessage(this, ['invalid-keyboards', 'error_outs_too_long.kmn'], KmnCompilerMessages.ERROR_OutsTooLong);
  });

  // ERROR_ExtendedStringTooLong

  it('should generate ERROR_ExtendedStringTooLong if an extended string is too long and would overflow the buffer', async function() {
    await testForMessage(this, ['invalid-keyboards', 'error_extended_string_too_long.kmn'], KmnCompilerMessages.ERROR_ExtendedStringTooLong);
  });

  // ERROR_VirtualKeyExpansionTooLong

  it('should generate ERROR_VirtualKeyExpansionTooLong if a virtual key expansion is too long and would overflow the buffer', async function() {
    await testForMessage(this, ['invalid-keyboards', 'error_virtual_key_expansion_too_long.kmn'], KmnCompilerMessages.ERROR_VirtualKeyExpansionTooLong);
  });

  // ERROR_CharacterRangeTooLong

  it('should generate ERROR_CharacterRangeTooLong if a character range would expand to be too long and would overflow the buffer', async function() {
    await testForMessage(this, ['invalid-keyboards', 'error_character_range_too_long.kmn'], KmnCompilerMessages.ERROR_CharacterRangeTooLong);
  });

  // ERROR_CannotReadBitmapFile

  it('should generate ERROR_CannotReadBitmapFile if the bitmap file is missing', async function() {
    await testForMessage(this, ['invalid-keyboards', 'error_cannot_read_bitmap_file.kmn'], KmnCompilerMessages.ERROR_CannotReadBitmapFile);
  });

  // WARN_IndexStoreShort

  it('should generate WARN_IndexStoreShort if a store referenced in index() is shorter than the corresponding any() store', async function() {
    await testForMessage(this, ['keyboards', 'warn_index_store_short.kmn'], KmnCompilerMessages.WARN_IndexStoreShort);
    callbacks.clear();
    await testForMessage(this, ['keyboards', 'warn_index_store_short_key.kmn'], KmnCompilerMessages.WARN_IndexStoreShort);
  });

  // HINT_IndexStoreLong

  it('should generate HINT_IndexStoreLong if a store referenced in index() is longer than the corresponding any() store', async function() {
    await testForMessage(this, ['keyboards', 'hint_index_store_long.kmn'], KmnCompilerMessages.HINT_IndexStoreLong);
    callbacks.clear();
    await testForMessage(this, ['keyboards', 'hint_index_store_long_key.kmn'], KmnCompilerMessages.HINT_IndexStoreLong);
  });

});
