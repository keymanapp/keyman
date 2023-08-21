import 'mocha';
import { assert } from 'chai';
// import sinonChai from 'sinon-chai';
import { dirname } from 'path';
import { fileURLToPath } from 'url';
import fs from 'fs';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { CompilerResult, KmnCompiler } from '../../src/compiler/compiler.js';
import { ETLResult, extractTouchLayout as parseWebTestResult } from './util.js';
import { KeymanFileTypes } from '@keymanapp/common-types';

const __dirname = dirname(fileURLToPath(import.meta.url)).replace(/\\/g, '/');
const fixturesDir = __dirname + '/../../../test/fixtures/kmw/';
//const baselineDir = __dirname + '/../../../../../common/test/keyboards/baseline/';
// chai.use(sinonChai);

const debug=false;

const generateTestFilenames = (id: string) => ({
  fixture: fixturesDir + id + KeymanFileTypes.Binary.WebKeyboard,
  source: fixturesDir + id + KeymanFileTypes.Source.KeymanKeyboard,
  intermediate: fixturesDir + id + KeymanFileTypes.Binary.Keyboard,
  binary: fixturesDir + id + '.test' + KeymanFileTypes.Binary.WebKeyboard
});

describe('KeymanWeb Compiler', function() {
  const callbacks = new TestCompilerCallbacks();
  const kmnCompiler: KmnCompiler = new KmnCompiler();

  this.beforeAll(async function() {
    assert.isTrue(await kmnCompiler.init(callbacks));
  });

  this.afterEach(function() {
    callbacks.printMessages();
    callbacks.clear();
  });

  it('should compile a complex keyboard', async function() {
    run_test_keyboard(kmnCompiler, 'khmer_angkor');
  });

  it('should handle option stores', async function() {
    //
    // This is enough to verify that the option store is set appropriately with
    // KLOAD because the fixture has that code present:
    //
    //    this.s_foo_6=KeymanWeb.KLOAD(this.KI,"foo","0");
    //
    run_test_keyboard(kmnCompiler, 'test_options');
  });

  it('should translate every "character style" key correctly', async function() {
    //
    // This is enough to verify that every character style key is encoded in the
    // same way as the fixture.
    //
    run_test_keyboard(kmnCompiler, 'test_keychars');
  });

  it('should handle readonly groups', async function() {
    run_test_keyboard(kmnCompiler, 'test_readonly_groups');
  });
});


function run_test_keyboard(kmnCompiler: KmnCompiler, id: string): { result: CompilerResult, actualCode: string, actual: ETLResult, expectedCode: string, expected: ETLResult } {
  const filenames = generateTestFilenames(id);

  let result = kmnCompiler.runCompiler(filenames.source, filenames.intermediate, {
    shouldAddCompilerVersion: false,
    saveDebug: true,
  });
  assert.isNotNull(result);

  let value = {
    result,
    actualCode: new TextDecoder().decode(result.js.data),
    expectedCode: fs.readFileSync(filenames.fixture, 'utf8'),
    expected: <ETLResult>null,
    actual: <ETLResult>null,
  };
  value.actual = parseWebTestResult(value.actualCode);
  value.expected = parseWebTestResult(value.expectedCode);

  if(debug) {
    // This is mostly to allow us to verify that parseWebTestResult is doing what we want
    // fs.writeFileSync(filenames.binary + '.strip.js', value.actual.js);
    // fs.writeFileSync(filenames.fixture + '.strip.js', value.expected.js);
    fs.writeFileSync(filenames.binary, value.actualCode);
  }

  assert.deepEqual(value.actual.js, value.expected.js);
  assert.deepEqual(JSON.parse(value.actual.touchLayout), JSON.parse(value.expected.touchLayout));

  return value;
}
