import 'mocha';
import {assert} from 'chai';
import hextobin from '@keymanapp/hextobin';
import {compileKeyboard, compilerTestCallbacks, compilerTestOptions, makePathToFixture} from './helpers/index.js';
import { compareXml } from './helpers/compareXml.js';
import { LdmlKeyboardCompiler } from '../src/compiler/compiler.js';
import { kmxToXml } from '../src/util/serialize.js';
import { writeFileSync } from 'node:fs';
import { LdmlCompilerMessages } from '../src/main.js';

/** Overall compiler tests */
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

    // Compare output
    const expected = await hextobin(binaryFilename, undefined, {silent:true});

    // now compare it to use with run()
    // Let's build basic.xml
    // It should match basic.kmx (built from basic.txt)
    const k = new LdmlKeyboardCompiler();
    await k.init(compilerTestCallbacks, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false });

    const { artifacts } = await k.run(inputFilename, "basic-xml.kmx"); // need the exact name passed to build-fixtures
    assert.isNotNull(artifacts);
    const { kmx, kvk  } = artifacts;
    assert.isNotNull(kmx);
    assert.deepEqual<Uint8Array>(kmx?.data, expected);

    // TODO-LDML: compare the .kvk file to something else?
    assert.isNotNull(kvk?.data);
  });

  it('should-validate-on-run compiling sections/strs/invalid-illegal.xml', async function() {
    this.timeout(4000);
    const inputFilename = makePathToFixture('sections/strs/invalid-illegal.xml');

    // should fail validation
    const k = new LdmlKeyboardCompiler();
    await k.init(compilerTestCallbacks, { ...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false });

    const runOutput = await k.run(inputFilename, "invalid-illegal.kmx"); // need the exact name passed to build-fixtures
    assert.isNull(runOutput, "Expect invalid-illegal to fail to run()");
    assert.sameDeepMembers(compilerTestCallbacks.messages, [
      // copied from strs.tests.ts
      // validation messages
      LdmlCompilerMessages.Error_IllegalCharacters({ count: 5, lowestCh: 0xFDD0 }),
      LdmlCompilerMessages.Hint_PUACharacters({ count: 2, lowestCh: 0xE010 }),
    ]);
  });


  it('should-serialize-kmx', async function() {
    this.timeout(4000);
    // Let's build basic.xml
    // It should match basic.kmx (built from basic.txt)
    const inputFilename = makePathToFixture('basic.xml');

    // Compile the keyboard
    const kmx = await compileKeyboard(inputFilename, {...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false});
    assert.isNotNull(kmx);

    // now output it as XML
    const outputFilename = makePathToFixture('basic-serialized.xml');
    const asXml = kmxToXml(kmx);
    writeFileSync(outputFilename, asXml, 'utf-8');

    compareXml(outputFilename, inputFilename);
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
});
