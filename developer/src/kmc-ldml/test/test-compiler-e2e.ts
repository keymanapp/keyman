import 'mocha';
import {assert} from 'chai';
import x_hextobin from '@keymanapp/hextobin';
import { KMXBuilder } from '@keymanapp/common-types';
import {checkMessages, compileKeyboard, compilerTestCallbacks, compilerTestOptions, makePathToFixture} from './helpers/index.js';
import { LdmlKeyboardCompiler } from '../src/compiler/compiler.js';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';

const hextobin = (x_hextobin as any).default;

describe('compiler-tests', function() {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should-build-fixtures', async function() {
    // Let's build basic.xml
    // It should match basic.kmx (built from basic.txt)

    const inputFilename = makePathToFixture('basic.xml');
    const binaryFilename = makePathToFixture('basic.txt');

    // Compile the keyboard
    const kmx = await compileKeyboard(inputFilename, {...compilerTestOptions, debug: true, addCompilerVersion: false});
    assert.isNotNull(kmx);

    // Use the builder to generate the binary output file
    const builder = new KMXBuilder(kmx, true);
    const code = builder.compile();
    checkMessages();
    assert.isNotNull(code);

    // Compare output
    let expected = await hextobin(binaryFilename, undefined, {silent:true});

    assert.deepEqual<Uint8Array>(code, expected);
  });

  it('should handle non existent files', () => {
    const filename = 'DOES_NOT_EXIST.xml';
    const k = new LdmlKeyboardCompiler(compilerTestCallbacks, { ...compilerTestOptions, debug: true, addCompilerVersion: false });
    const source = k.load(filename);
    assert.notOk(source, `Trying to load(${filename})`);
  });
  it('should handle unparseable files', () => {
    const filename = makePathToFixture('basic-kvk.txt'); // not an .xml file
    const k = new LdmlKeyboardCompiler(compilerTestCallbacks, { ...compilerTestOptions, debug: true, addCompilerVersion: false });
    const source = k.load(filename);
    assert.notOk(source, `Trying to load(${filename})`);
  });
  it('should handle not-valid files', () => {
    const filename = makePathToFixture('test-fr.xml'); // not a keyboard .xml file
    const k = new LdmlKeyboardCompiler(compilerTestCallbacks, { ...compilerTestOptions, debug: true, addCompilerVersion: false });
    const source = k.load(filename);
    assert.notOk(source, `Trying to load(${filename})`);
  });
  it('should throw on broken schema', () => {
    const filename = makePathToFixture('basic.xml');
    const callbacks = new TestCompilerCallbacks();
    // simulate broken schema
    callbacks.loadSchema = () => new Uint8Array();
    const k = new LdmlKeyboardCompiler(callbacks, { ...compilerTestOptions, debug: true, addCompilerVersion: false });
    const source = k.load(filename);
    assert.notOk(source, `Trying to load(${filename})`);
  });
  it('should handle non existent test files', () => {
    const filename = 'DOES_NOT_EXIST.xml';
    const k = new LdmlKeyboardCompiler(compilerTestCallbacks, { ...compilerTestOptions, debug: true, addCompilerVersion: false });
    const source = k.loadTestData(filename);
    assert.notOk(source, `Trying to loadTestData(${filename})`);
  });
  it('should handle unparseable test files', () => {
    const filename = makePathToFixture('basic-kvk.txt'); // not an .xml file
    const k = new LdmlKeyboardCompiler(compilerTestCallbacks, { ...compilerTestOptions, debug: true, addCompilerVersion: false });
    const source = k.load(filename);
    assert.notOk(source, `Trying to loadTestData(${filename})`);
  });
});
