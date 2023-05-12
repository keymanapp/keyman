import 'mocha';
import sinon from 'sinon';
import chai, { assert } from 'chai';
import sinonChai from 'sinon-chai';
import { KmnCompiler } from '../src/main.js';
import { dirname } from 'path';
import { fileURLToPath } from 'url';
import fs from 'fs';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';

const __dirname = dirname(fileURLToPath(import.meta.url)).replace(/\\/g, '/');
const baselineDir = __dirname + '/../../../../../common/test/keyboards/baseline/';
chai.use(sinonChai);

describe('Compiler class', function() {
  let consoleLog: any;

  // TODO: do we need this?
  beforeEach(function() {
    consoleLog = sinon.spy(console, 'log');
  });

  afterEach(function() {
    consoleLog.restore();
  });

  it('should throw on failure', async function() {
    const compiler = new Compiler();
    const callbacks : any = null; // ERROR
    try {
      await compiler.init(callbacks)
      assert.fail('Expected exception');
    } catch(e) {
      assert.ok(e);
    }
    assert.throws(() => compiler.verifyInitted());
  });

  it('should start', async function() {
    const compiler = new KmnCompiler();
    const callbacks = new TestCompilerCallbacks();
    assert(await compiler.init(callbacks));
    assert(compiler.verifyInitialized());
  });

  it('should compile a basic keyboard', async function() {
    const compiler = new KmnCompiler();
    const callbacks = new TestCompilerCallbacks();
    assert(await compiler.init(callbacks));
    assert(compiler.verifyInitialized());

    const fixtureName = baselineDir + 'k_000___null_keyboard.kmx';
    const infile = baselineDir + 'k_000___null_keyboard.kmn';
    const outfile = __dirname + '/k_000___null_keyboard.kmx';

    assert(compiler.run(infile, outfile, {saveDebug: true, shouldAddCompilerVersion: false}));

    assert(fs.existsSync(outfile));
    const outfileData = fs.readFileSync(outfile);
    const fixtureData = fs.readFileSync(fixtureName);
    assert.equal(outfileData.byteLength, fixtureData.byteLength);
    assert.deepEqual(outfileData, fixtureData);
  });

  // Note, above test case is essentially a subset of this one, but will leave both because
  // the basic keyboard test is slightly simpler to read
  it('should build all baseline fixtures', async function() {
    const compiler = new KmnCompiler();
    const callbacks = new TestCompilerCallbacks();
    assert(await compiler.init(callbacks));
    assert(compiler.verifyInitialized());

    const files = fs.readdirSync(baselineDir);
    for(let file of files) {
      if(file.match(/\.kmx$/)) {
        const fixtureName = baselineDir + file;
        const infile = baselineDir + file.replace(/x$/, 'n');
        const outfile = __dirname + '/' + file;

        assert(compiler.run(infile, outfile, {saveDebug: true, shouldAddCompilerVersion: false}));

        assert(fs.existsSync(outfile));
        const outfileData = fs.readFileSync(outfile);
        const fixtureData = fs.readFileSync(fixtureName);
        assert.equal(outfileData.byteLength, fixtureData.byteLength);
        assert.deepEqual(outfileData, fixtureData);
      }
    }
  });
});
