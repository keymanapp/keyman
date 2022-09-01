import 'mocha';
import * as fs from 'fs';
import * as path from 'path';
import {assert} from 'chai';
import hextobin from '@keymanapp/hextobin';
import Compiler from '../src/keyman/compiler/compiler';
import KMXBuilder from '../src/keyman/kmx/kmx-builder';
import {CompilerCallbacks, makePathToFixture} from './helpers/index';

function compileKeyboard(inputFilename: string): Uint8Array {
  const callbacks = new CompilerCallbacks();
  const k = new Compiler(callbacks);
  let source = k.load(inputFilename);
  if(!source) {
    return null;
  }
  if(!k.validate(source)) {
    return null;
  }
  let kmx = k.compile(source);
  if(!kmx) {
    return null;
  }

  // Use the builder to generate the binary output file
  let builder = new KMXBuilder(kmx, true);
  let result = builder.compile();
  assert(callbacks.messages.length == 0);
  return result;
}

describe('compiler-tests', function() {
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