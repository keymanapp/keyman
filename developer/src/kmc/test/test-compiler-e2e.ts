import 'mocha';
import * as fs from 'fs';
import * as path from 'path';
import {assert} from 'chai';
import hextobin from '@keymanapp/hextobin';
import Compiler from '../src/keyman/compiler/compiler';
import KMXBuilder from '../src/keyman/kmx/kmx-builder';
import {CompilerCallbacks, makePathToFixture} from './helpers/index';

function checkMessages(callbacks: CompilerCallbacks) {
  if(callbacks.messages.length > 0) {
    console.log(callbacks.messages);
  }
  assert.isEmpty(callbacks.messages);
}

function compileKeyboard(inputFilename: string): Uint8Array {
  const callbacks = new CompilerCallbacks();
  const k = new Compiler(callbacks);
  const source = k.load(inputFilename);
  checkMessages(callbacks);
  assert.isNotNull(source, 'k.load should not have returned null');

  const valid = k.validate(source);
  checkMessages(callbacks);
  assert.isTrue(valid, 'k.validate should not have failed');

  const kmx = k.compile(source);
  checkMessages(callbacks);
  assert.isNotNull(kmx, 'k.compile should not have returned null');

  // Use the builder to generate the binary output file
  const builder = new KMXBuilder(kmx, true);
  const result = builder.compile();
  checkMessages(callbacks);
  return result;
}

describe('compiler-tests', function() {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should-build-fixtures', async function() {
    // Let's build basic.xml
    // It should match basic.kmx (built from basic.txt)

    const inputFilename = makePathToFixture('basic.xml');
    const outputFilename = makePathToFixture('basic.txt');

    // Compile:
    const code = compileKeyboard(inputFilename);
    assert.isNotNull(code);

    // Compare output
    let expected = await hextobin(outputFilename, undefined, {silent:true});
    assert.deepEqual<Uint8Array>(code, expected);
  });
});