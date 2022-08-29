import 'mocha';
import * as fs from 'fs';
import * as path from 'path';
import {assert} from 'chai';
import hextobin from '@keymanapp/hextobin';
import Compiler from '../src/keyman/compiler/compiler';
import {makePathToFixture} from './helpers/index';

class CompilerCallbacks {
  loadFile(baseFilename: string, filename:string): Buffer {
    // TODO: translate filename based on the baseFilename
    return fs.readFileSync(filename);
  }
  reportMessage(severity: number, message: string): void {
    console.log(message);
  }
}

function compileKeyboard(inputFilename: string): Uint8Array {
  const c = new CompilerCallbacks();
  const k = new Compiler(c);
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
  return k.write(kmx);
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