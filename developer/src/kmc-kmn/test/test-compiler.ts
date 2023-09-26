import 'mocha';
import { assert } from 'chai';
import { KmnCompiler } from '../src/main.js';
import { dirname } from 'path';
import { fileURLToPath } from 'url';
import fs from 'fs';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';

const __dirname = dirname(fileURLToPath(import.meta.url)).replace(/\\/g, '/');
const keyboardsDir = __dirname + '/../../../../../common/test/keyboards/';
const baselineDir = keyboardsDir + 'baseline/';

describe('Compiler class', function() {
  it('should throw on failure', async function() {
    const compiler = new KmnCompiler();
    const callbacks : any = null; // ERROR
    try {
      await compiler.init(callbacks)
      assert.fail('Expected exception');
    } catch(e) {
      assert.ok(e);
    }
    assert.throws(() => compiler.verifyInitialized());
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

  it('should compile a keyboard with visual keyboard', async function() {
    const compiler = new KmnCompiler();
    const callbacks = new TestCompilerCallbacks();
    assert.isTrue(await compiler.init(callbacks));
    assert.isTrue(compiler.verifyInitialized());

    const fixtureDir = keyboardsDir + 'caps_lock_layer_3620/'
    const infile = fixtureDir + 'source/caps_lock_layer_3620.kmn';
    const kmxFixture = fixtureDir + '/binary/caps_lock_layer_3620.kmx';
    const kvkFixture = fixtureDir + '/binary/caps_lock_layer_3620.kvk';

    const resultingKmxfile = __dirname + '/caps_lock_layer_3620.kmx';
    const resultingKvkfile = __dirname + '/caps_lock_layer_3620.kvk';

    assert.isTrue(compiler.run(infile, resultingKmxfile, {saveDebug: true, shouldAddCompilerVersion: false}));

    assert.isTrue(fs.existsSync(resultingKmxfile));
    assert.isTrue(fs.existsSync(resultingKvkfile));

    const kmxData = fs.readFileSync(resultingKmxfile);
    const kmxFixtureData = fs.readFileSync(kmxFixture);
    assert.equal(kmxData.byteLength, kmxFixtureData.byteLength);
    assert.deepEqual(kmxData, kmxFixtureData);

    const kvkData = fs.readFileSync(resultingKvkfile);
    const kvkFixtureData = fs.readFileSync(kvkFixture);
    assert.equal(kvkData.byteLength, kvkFixtureData.byteLength);
    assert.deepEqual(kvkData, kvkFixtureData);
  });

});
