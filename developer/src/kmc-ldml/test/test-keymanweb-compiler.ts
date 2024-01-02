import 'mocha';
import { assert } from 'chai';
import { checkMessages, compilerTestCallbacks, compilerTestOptions, makePathToFixture } from './helpers/index.js';
import { LdmlKeyboardKeymanWebCompiler } from '../src/compiler/keymanweb-compiler.js';
import { LdmlKeyboardCompiler } from '../src/compiler/compiler.js';
import * as fs from 'fs';

describe('LdmlKeyboardKeymanWebCompiler', function() {

  it('should build a .js file', async function() {
    // Let's build basic.xml
    // It should generate content identical to basic.js
    const inputFilename = makePathToFixture('basic.xml');
    const outputFilename = makePathToFixture('basic.js');
    const outputFilenameNoDebug = makePathToFixture('basic-no-debug.js');

    // Load input data; we'll use the LDML keyboard compiler loader to save us
    // effort here
    const k = new LdmlKeyboardCompiler();
    await k.init(compilerTestCallbacks, {...compilerTestOptions, saveDebug: true, shouldAddCompilerVersion: false});
    const source = k.load(inputFilename);
    checkMessages();
    assert.isNotNull(source, 'k.load should not have returned null');

    // Sanity check ... this is also checked in other tests
    const valid = await k.validate(source);
    checkMessages();
    assert.isTrue(valid, 'k.validate should not have failed');

    // Actual test: compile to javascript
    const jsCompiler = new LdmlKeyboardKeymanWebCompiler(compilerTestCallbacks, {...compilerTestOptions, saveDebug: true});
    const output = jsCompiler.compile('basic.xml', source);
    assert.isNotNull(output);

    // Does the emitted js match?
    const outputFixture = fs.readFileSync(outputFilename, 'utf-8').replaceAll(/\r\n/g, '\n');
    assert.strictEqual(output, outputFixture);

    // Second test: compile to javascript without debug formatting
    const jsCompilerNoDebug = new LdmlKeyboardKeymanWebCompiler(compilerTestCallbacks, {...compilerTestOptions, saveDebug: false});
    const outputNoDebug = jsCompilerNoDebug.compile('basic.xml', source);
    assert.isNotNull(outputNoDebug);

    // Does the emitted js match? The nodebug has no newline at end, but allow one in the fixture
    const outputFixtureNoDebug = fs.readFileSync(outputFilenameNoDebug, 'utf-8').replaceAll(/\r\n/g, '\n').trim();
    assert.strictEqual(outputNoDebug, outputFixtureNoDebug);

    // TODO(lowpri): consider using Typescript parser to generate AST for further validation
  });
});
