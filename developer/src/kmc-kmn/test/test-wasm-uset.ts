import 'mocha';
import { assert } from 'chai';
import { KmnCompiler } from '../src/main.js';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { CompilerMessages } from '../src/compiler/kmn-compiler-messages.js';
import { compilerErrorFormatCode } from '@keymanapp/common-types';

describe('Compiler UnicodeSet function', function() {
  it('should fixup "short" \\u{} escapes', function () {
    assert.equal(KmnCompiler.fixNewPattern(`\\u{A}`), `\\u000A`); // "
    assert.equal(KmnCompiler.fixNewPattern(`\\u{22}`), `\\u0022`); // "
    assert.equal(KmnCompiler.fixNewPattern(`\\u{ead}`), `\\u0ead`); // "
  });
  it('should fixup \\u1234 format escapes', function() {
    assert.equal(KmnCompiler.fixNewPattern(`\\u{1234}`), `\\u1234`);
    assert.equal(KmnCompiler.fixNewPattern(`\\u1234`), `\\u1234`);
    assert.equal(KmnCompiler.fixNewPattern(`[\\u{1234}-\\u{5678}]`), `[\\u1234-\\u5678]`);
    assert.equal(KmnCompiler.fixNewPattern(`something else`), `something else`);
  });
  it('should fixup supplemental \\u format escapes', function() {
    assert.equal(KmnCompiler.fixNewPattern(`\\u{1F640}`), `\\U0001F640`);
    assert.equal(KmnCompiler.fixNewPattern(`\\u{10FFFD}`),`\\U0010FFFD`);
  });

  it('should start', async function() {
    const compiler = new KmnCompiler();
    const callbacks = new TestCompilerCallbacks();
    assert(await compiler.init(callbacks, null));
    assert(compiler.verifyInitialized());
  });

  it('should compile a basic uset', async function() {
    const compiler = new KmnCompiler();
    const callbacks = new TestCompilerCallbacks();
    assert(await compiler.init(callbacks, null));
    assert(compiler.verifyInitialized());

    const pat = "[abc]";
    const set = compiler.parseUnicodeSet(pat, 23);

    assert(set.length === 1);
    assert(set.ranges[0][0] === 'a'.charCodeAt(0));
    assert(set.ranges[0][1] === 'c'.charCodeAt(0));
    assert.deepEqual(callbacks.messages, []);
    callbacks.clear();
    const len2 = compiler.sizeUnicodeSet(pat);
    assert.deepEqual(callbacks.messages, []);
    assert.equal(len2, set.length);
  });
  it('should compile a more complex uset', async function() {
    const compiler = new KmnCompiler();
    const callbacks = new TestCompilerCallbacks();
    assert(await compiler.init(callbacks, null));
    assert(compiler.verifyInitialized());

    const pat = "[[🙀A-C]-[CB]]";
    const set = compiler.parseUnicodeSet(pat, 23);

    assert.equal(set.length, 2);
    assert.equal(set.ranges[0][0], 'A'.charCodeAt(0));
    assert.equal(set.ranges[0][1], 'A'.charCodeAt(0));
    assert.equal(set.ranges[1][0], 0x1F640);
    assert.equal(set.ranges[1][1], 0x1F640);
    assert.deepEqual(callbacks.messages, []);
    callbacks.clear();
    const len2 = compiler.sizeUnicodeSet(pat);
    assert.deepEqual(callbacks.messages, []);
    assert.equal(len2, set.length);
  });
  it('should compile an even more complex uset', async function() {
    const compiler = new KmnCompiler();
    const callbacks = new TestCompilerCallbacks();
    assert(await compiler.init(callbacks, null));
    assert(compiler.verifyInitialized());

    const pat = "[\\u{10FFFD}\\u{2019}\\u{22}\\u{a}\\u{ead}\\u{1F640}]";
    const set = compiler.parseUnicodeSet(pat, 23);

    assert.equal(set.length, 6);
    // verify we're all single chars
    for (let i = 0; i<set.length; i++) {
      assert.equal(set.ranges[i][0], set.ranges[i][1], `Range ${i} should be a single char`);
    }
    // check the single char value
    assert.equal(set.ranges[0][0], 0x000A);
    assert.equal(set.ranges[1][0], 0x0022);
    assert.equal(set.ranges[2][0], 0x0EAD);
    assert.equal(set.ranges[3][0], 0x2019);
    assert.equal(set.ranges[4][0], 0x1F640);
    assert.equal(set.ranges[5][0], 0x10FFFD);
    assert.deepEqual(callbacks.messages, []);
    callbacks.clear();
    const len2 = compiler.sizeUnicodeSet(pat);
    assert.deepEqual(callbacks.messages, []);
    assert.equal(len2, set.length);
  });
  it('should fail in various ways', async function() {
    const compiler = new KmnCompiler();
    const callbacks = new TestCompilerCallbacks();
    assert(await compiler.init(callbacks, null));
    assert(compiler.verifyInitialized());
    // map from string to failing error
    const failures = {
      '[:Adlm:]': CompilerMessages.ERROR_UnicodeSetHasProperties, // what it saye
      '[acegik]': CompilerMessages.FATAL_UnicodeSetOutOfRange, // 6 ranges, allocated 1
      '[[\\p{Mn}]&[A-Z]]': CompilerMessages.ERROR_UnicodeSetHasProperties,
      '[abc{def}]': CompilerMessages.ERROR_UnicodeSetHasStrings,
      '[[]': CompilerMessages.ERROR_UnicodeSetSyntaxError,
    };
    for(const [pat, expected] of Object.entries(failures)) {
      {
        // verify fails parse
        callbacks.clear();
        assert.notOk(compiler.parseUnicodeSet(pat, 1));
        assert.equal(callbacks.messages.length, 1);
        const firstMessage = callbacks.messages[0];
        const code = firstMessage.code;
        assert.equal(code, expected, `${compilerErrorFormatCode(code)}≠${compilerErrorFormatCode(expected)} got ${firstMessage.message} for parsing ${pat}`);
      }
      // skip 'out of range' because that one won't fail during sizing.
      if (expected !== CompilerMessages.FATAL_UnicodeSetOutOfRange) {
        // verify fails size
        callbacks.clear();
        assert.equal(compiler.sizeUnicodeSet(pat), -1, `sizing ${pat}`);
        assert.equal(callbacks.messages.length, 1);
        const firstMessage = callbacks.messages[0];
        const code = firstMessage.code;
        assert.equal(code, expected, `${compilerErrorFormatCode(code)}≠${compilerErrorFormatCode(expected)} got ${firstMessage.message} for sizing ${pat}`);
      }
    }
  });
});
