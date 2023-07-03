import 'mocha';
import { assert } from 'chai';
import { KmnCompiler } from '../src/main.js';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { CompilerMessages } from '../src/compiler/messages.js';
import { compilerErrorFormatCode } from '@keymanapp/common-types';

describe('Compiler UnicodeSet function', function() {
  it('should start', async function() {
    const compiler = new KmnCompiler();
    const callbacks = new TestCompilerCallbacks();
    assert(await compiler.init(callbacks));
    assert(compiler.verifyInitialized());
  });

  it('should compile a basic uset', async function() {
    const compiler = new KmnCompiler();
    const callbacks = new TestCompilerCallbacks();
    assert(await compiler.init(callbacks));
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
    assert(await compiler.init(callbacks));
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
  it('should fail in various ways', async function() {
    const compiler = new KmnCompiler();
    const callbacks = new TestCompilerCallbacks();
    assert(await compiler.init(callbacks));
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
      callbacks.clear();
      assert.notOk(compiler.parseUnicodeSet(pat, 1));
      assert.equal(callbacks.messages.length, 1);
      const firstMessage = callbacks.messages[0];
      const code = firstMessage.code;
      assert.equal(code, expected, `${compilerErrorFormatCode(code)}≠${compilerErrorFormatCode(expected)} got ${firstMessage.message} for ${pat}`);
    }
  });
});
