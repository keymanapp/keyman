import 'mocha';
import {assert} from 'chai';
import hextobin from '@keymanapp/hextobin';
import { KMXBuilder } from '@keymanapp/common-types';
import {checkMessages, compileKeyboard, compilerTestCallbacks, compilerTestOptions, makePathToFixture} from './helpers/index.js';
import { LdmlKeyboardCompiler } from '../src/compiler/compiler.js';

describe('compiler-tests', function() {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  it('should-build-fixtures', async function() {
    // Let's build basic.xml
    // It should match basic.kmx (built from basic.txt)

    const inputFilename = makePathToFixture('basic.xml');
    const binaryFilename = makePathToFixture('basic.txt');

    // Compile the keyboard
    const kmx = await compileKeyboard(inputFilename, {...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false});
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
    const k = new LdmlKeyboardCompiler(compilerTestCallbacks, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false });
    const source = k.load(filename);
    assert.notOk(source, `Trying to load(${filename})`);
  });
  it('should handle unparseable files', () => {
    const filename = makePathToFixture('basic-kvk.txt'); // not an .xml file
    const k = new LdmlKeyboardCompiler(compilerTestCallbacks, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false });
    const source = k.load(filename);
    assert.notOk(source, `Trying to load(${filename})`);
  });
  it('should handle not-valid files', () => {
    const filename = makePathToFixture('test-fr.xml'); // not a keyboard .xml file
    const k = new LdmlKeyboardCompiler(compilerTestCallbacks, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false });
    const source = k.load(filename);
    assert.notOk(source, `Trying to load(${filename})`);
  });
  it('should handle non existent test files', () => {
    const filename = 'DOES_NOT_EXIST.xml';
    const k = new LdmlKeyboardCompiler(compilerTestCallbacks, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false });
    const source = k.loadTestData(filename);
    assert.notOk(source, `Trying to loadTestData(${filename})`);
  });
  it('should handle unparseable test files', () => {
    const filename = makePathToFixture('basic-kvk.txt'); // not an .xml file
    const k = new LdmlKeyboardCompiler(compilerTestCallbacks, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false });
    const source = k.load(filename);
    assert.notOk(source, `Trying to loadTestData(${filename})`);
  });
});
