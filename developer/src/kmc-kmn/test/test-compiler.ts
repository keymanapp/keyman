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
      await compiler.init(callbacks, null)
      assert.fail('Expected exception');
    } catch(e) {
      assert.ok(e);
    }
    assert.throws(() => compiler.verifyInitialized());
  });

  it('should start', async function() {
    const compiler = new KmnCompiler();
    const callbacks = new TestCompilerCallbacks();
    assert(await compiler.init(callbacks, null));
    assert(compiler.verifyInitialized());
  });

  it('should compile a basic keyboard', async function() {
    const compiler = new KmnCompiler();
    const callbacks = new TestCompilerCallbacks();
    assert(await compiler.init(callbacks, {saveDebug: true, shouldAddCompilerVersion: false}));
    assert(compiler.verifyInitialized());

    const fixtureName = baselineDir + 'k_000___null_keyboard.kmx';
    const infile = baselineDir + 'k_000___null_keyboard.kmn';
    const outFile = __dirname + '/k_000___null_keyboard.kmx';

    if(fs.existsSync(outFile)) {
      fs.rmSync(outFile);
    }

    const result = await compiler.run(infile, outFile);
    assert.isNotNull(result);
    assert.isTrue(await compiler.write(result.artifacts));

    assert(fs.existsSync(outFile));
    const outfileData = fs.readFileSync(outFile);
    const fixtureData = fs.readFileSync(fixtureName);
    assert.equal(outfileData.byteLength, fixtureData.byteLength);
    assert.deepEqual(outfileData, fixtureData);
  });

  // Note, above test case is essentially a subset of this one, but will leave both because
  // the basic keyboard test is slightly simpler to read
  it('should build all baseline fixtures', async function() {
    this.timeout(10000); // there are quite a few fixtures, sometimes CI agents are slow
    const compiler = new KmnCompiler();
    const callbacks = new TestCompilerCallbacks();
    assert(await compiler.init(callbacks, {saveDebug: true, shouldAddCompilerVersion: false}));
    assert(compiler.verifyInitialized());

    const files = fs.readdirSync(baselineDir);
    for(let file of files) {
      if(file.match(/\.kmx$/)) {
        const fixtureName = baselineDir + file;
        const infile = baselineDir + file.replace(/x$/, 'n');
        const outFile = __dirname + '/' + file;

        if(fs.existsSync(outFile)) {
          fs.rmSync(outFile);
        }

        const result = await compiler.run(infile, outFile);
        assert.isNotNull(result);
        assert.isTrue(await compiler.write(result.artifacts));

        assert(fs.existsSync(outFile));
        const outfileData = fs.readFileSync(outFile);
        const fixtureData = fs.readFileSync(fixtureName);
        assert.equal(outfileData.byteLength, fixtureData.byteLength, `file ${file} has the wrong byte length`);
        assert.deepEqual(outfileData, fixtureData, `file ${file} does not match fixture`);
      }
    }
  });

  it('should compile a keyboard with visual keyboard', async function() {
    const compiler = new KmnCompiler();
    const callbacks = new TestCompilerCallbacks();
    assert.isTrue(await compiler.init(callbacks, {
      saveDebug: true,
      shouldAddCompilerVersion: false,
    }));
    assert.isTrue(compiler.verifyInitialized());

    const fixtureDir = keyboardsDir + 'caps_lock_layer_3620/'
    const infile = fixtureDir + 'source/caps_lock_layer_3620.kmn';
    const kmxFixture = fixtureDir + '/binary/caps_lock_layer_3620.kmx';
    const kvkFixture = fixtureDir + '/binary/caps_lock_layer_3620.kvk';

    const resultingKmxfile = __dirname + '/caps_lock_layer_3620.kmx';
    const resultingKvkfile = __dirname + '/caps_lock_layer_3620.kvk';

    if(fs.existsSync(resultingKmxfile)) {
      fs.rmSync(resultingKmxfile);
    }

    if(fs.existsSync(resultingKvkfile)) {
      fs.rmSync(resultingKvkfile);
    }

    const result = await compiler.run(infile, resultingKmxfile);
    assert.isNotNull(result);
    assert.isTrue(await compiler.write(result.artifacts));

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
