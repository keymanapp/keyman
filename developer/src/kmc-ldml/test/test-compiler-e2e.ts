import 'mocha';
import {assert} from 'chai';
import hextobin from '@keymanapp/hextobin';
import { KMXBuilder } from '@keymanapp/common-types';
import {checkMessages, compileKeyboard, compilerTestCallbacks, compilerTestOptions, makePathToFixture} from './helpers/index.js';
import { LdmlKeyboardCompiler } from '../src/compiler/compiler.js';
import { CompilerMessages } from '../src/compiler/messages.js';

describe('compiler-tests', function() {
  this.slow(500); // 0.5 sec -- json schema validation takes a while

  before(function() {
    compilerTestCallbacks.clear();
  });

  it('should-build-fixtures', async function() {
    this.timeout(4000);
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

  it('should handle non existent files', async () => {
    const filename = 'DOES_NOT_EXIST.xml';
    const k = new LdmlKeyboardCompiler();
    await k.init(compilerTestCallbacks, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false });
    const source = k.load(filename);
    assert.notOk(source, `Trying to load(${filename})`);
  });
  it('should handle unparseable files', async () => {
    const filename = makePathToFixture('basic-kvk.txt'); // not an .xml file
    const k = new LdmlKeyboardCompiler();
    await k.init(compilerTestCallbacks, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false });
    const source = k.load(filename);
    assert.notOk(source, `Trying to load(${filename})`);
  });
  it('should handle not-valid files', async () => {
    const filename = makePathToFixture('test-fr.xml'); // not a keyboard .xml file
    const k = new LdmlKeyboardCompiler();
    await k.init(compilerTestCallbacks, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false });
    const source = k.load(filename);
    assert.notOk(source, `Trying to load(${filename})`);
  });
  it('should handle non existent test files', async () => {
    const filename = 'DOES_NOT_EXIST.xml';
    const k = new LdmlKeyboardCompiler();
    await k.init(compilerTestCallbacks, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false });
    const source = k.loadTestData(filename);
    assert.notOk(source, `Trying to loadTestData(${filename})`);
  });
  it('should handle unparseable test files', async () => {
    const filename = makePathToFixture('basic-kvk.txt'); // not an .xml file
    const k = new LdmlKeyboardCompiler();
    await k.init(compilerTestCallbacks, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false });
    const source = k.load(filename);
    assert.notOk(source, `Trying to loadTestData(${filename})`);
  });
  it('should fail on illegal chars - sections/strs/invalid-illegal.xml', async function() {
    const inputFilename = makePathToFixture('sections/strs/invalid-illegal.xml');
    const kmx = await compileKeyboard(inputFilename, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false },
      [
        // validation messages
        CompilerMessages.Error_IllegalCharacters({ count: 5, lowestCh: 0xFDD0 }),
        CompilerMessages.Hint_PUACharacters({ count: 2, lowestCh: 0xE010 }),
      ],
      true, // validation should fail
      [
        // compiler messages (not reached, we've already failed)
      ]);
    assert.isNull(kmx); // should fail post-validate
  });
  it('should hint on pua chars', async function() {
    const inputFilename = makePathToFixture('sections/strs/hint-pua.xml');
    // Compile the keyboard
    const kmx = await compileKeyboard(inputFilename, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false },
      [
        // validation messages
        CompilerMessages.Hint_PUACharacters({ count: 2, lowestCh: 0xE010 }),
      ],
      false, // validation should pass
      [
        // same messages
        CompilerMessages.Hint_PUACharacters({ count: 2, lowestCh: 0xE010 }),
      ]);
    assert.isNotNull(kmx);
  });
  it.skip('should warn on unassigned chars', async function() {
    // unassigned not implemented yet
    const inputFilename = makePathToFixture('sections/strs/warn-unassigned.xml');
    const kmx = await compileKeyboard(inputFilename, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false },
      [
        // validation messages
        CompilerMessages.Hint_PUACharacters({ count: 2, lowestCh: 0xE010 }),
        CompilerMessages.Warn_UnassignedCharacters({ count: 1, lowestCh: 0x0CFFFD }),
      ],
      false, // validation should pass
      [
        // same messages
        CompilerMessages.Hint_PUACharacters({ count: 2, lowestCh: 0xE010 }),
        CompilerMessages.Warn_UnassignedCharacters({ count: 1, lowestCh: 0x0CFFFD }),
      ]);
    assert.isNotNull(kmx);
  });
  it('should warn on mark keys with no display', async function () {
    // unassigned not implemented yet
    const inputFilename = makePathToFixture('sections/keys/warn-no-keycap.xml');
    const msgs = [
      CompilerMessages.Hint_NoDisplayForMarker({id: 'mark-dotbelow', output:'\\m{dotbelow}'}),
    ]
    const kmx = await compileKeyboard(inputFilename, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false },
      msgs,
      false, // validation should pass
      msgs,
    );
    assert.isNotNull(kmx);
  });
  it('should fail on extra escapes - sections/tran/fail-bad-tran-2.xml', async function() {
    const inputFilename = makePathToFixture('sections/tran/fail-bad-tran-2.xml');
    const kmx = await compileKeyboard(inputFilename, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false },
      [
        CompilerMessages.Error_InvalidQuadEscape({ cp: 295 }),
      ],
      true, // validation should fail
      [
        // compiler messages (not reached, we've already failed)
      ]);
    assert.isNull(kmx); // should fail post-validate
  });
});
