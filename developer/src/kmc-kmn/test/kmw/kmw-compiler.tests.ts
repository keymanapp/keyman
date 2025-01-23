import 'mocha';
import { assert } from 'chai';
// import sinonChai from 'sinon-chai';
import { dirname } from 'path';
import { fileURLToPath } from 'url';
import fs from 'fs';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { KmnCompilerResult, KmnCompiler } from '../../src/compiler/compiler.js';
import { ETLResult, extractTouchLayout } from './util.js';
import { KeymanFileTypes } from '@keymanapp/common-types';
import { KmnCompilerMessages } from '../../src/compiler/kmn-compiler-messages.js';
import { KmwCompilerMessages } from '../../src/kmw-compiler/kmw-compiler-messages.js';

const __dirname = dirname(fileURLToPath(import.meta.url)).replace(/\\/g, '/');
const fixturesDir = __dirname + '/../../../test/fixtures/kmw/';
//const baselineDir = __dirname + '/../../../../../common/test/keyboards/baseline/';
// chai.use(sinonChai);

const debug=false;

const generateTestFilenames = (id: string) => ({
  fixture: fixturesDir + id + KeymanFileTypes.Binary.WebKeyboard,
  source: fixturesDir + id + KeymanFileTypes.Source.KeymanKeyboard,
  binary: fixturesDir + id + '.test' + KeymanFileTypes.Binary.WebKeyboard
});

describe('KeymanWeb Compiler', function() {
  const callbacks = new TestCompilerCallbacks();
  const kmnCompiler: KmnCompiler = new KmnCompiler();

  this.beforeAll(async function() {
    assert.isTrue(await kmnCompiler.init(callbacks, {
      shouldAddCompilerVersion: false,
      saveDebug: true,
    }));
  });

  this.afterEach(function() {
    if(this.currentTest?.isFailed() || debug) {
      callbacks.printMessages();
    }
    callbacks.clear();
  });

  it('should compile a complex keyboard', async function() {
    await run_test_keyboard(kmnCompiler, 'khmer_angkor');
  });

  it('should handle option stores', async function() {
    //
    // This is enough to verify that the option store is set appropriately with
    // KLOAD because the fixture has that code present:
    //
    //    this.s_foo_6=KeymanWeb.KLOAD(this.KI,"foo","0");
    //
    await run_test_keyboard(kmnCompiler, 'test_options');
  });

  it('should translate every "character style" key correctly', async function() {
    //
    // This is enough to verify that every character style key is encoded in the
    // same way as the fixture.
    //
    await run_test_keyboard(kmnCompiler, 'test_keychars');
  });

  it('should handle readonly groups', async function() {
    await run_test_keyboard(kmnCompiler, 'test_readonly_groups');
  });

  it('should handle context(n) in output of rule, v10.0 generation', async function() {
    await run_test_keyboard(kmnCompiler, 'test_contextn_in_output');
  });

  it('should handle context(n) in output of rule, v9.0 generation', async function() {
    await run_test_keyboard(kmnCompiler, 'test_contextn_in_output_9');
  });

  it('should handle context(n) in context part of rule, v9.0 generation', async function() {
    await run_test_keyboard(kmnCompiler, 'test_context_in_context_9');
  });

  it('should handle context(n) in context part of rule, v10.0 generation', async function() {
    await run_test_keyboard(kmnCompiler, 'test_context_in_context');
  });

  it('should determine the minimum version correctly with U_xxxx_yyyy touch ids', async function() {
    const filenames = generateTestFilenames('version_u_xxxx_yyyy');

    let result = await kmnCompiler.run(filenames.source, null);
    assert.isNotNull(result);
    assert.isTrue(callbacks.hasMessage(KmwCompilerMessages.INFO_MinimumWebEngineVersion));
    // we expect only 1 of the info messages -- for the .kmx target (not 2)
    assert.equal(callbacks.messages.filter(item => item.code == KmnCompilerMessages.INFO_MinimumCoreEngineVersion).length, 1);

    const data = new TextDecoder('utf-8').decode(result.artifacts.js.data);
    assert.match(data, /KMINVER="15.0"/, `Could not find expected 'KMINVER="15.0"'`);
  });

  it('should give an error if the minimum version specified in the keyboard does not support U_xxxx_yyyy touch ids', async function() {
    const filenames = generateTestFilenames('version_u_xxxx_yyyy_14');

    let result = await kmnCompiler.run(filenames.source, null);
    assert.isNull(result);
    assert.isFalse(callbacks.hasMessage(KmnCompilerMessages.INFO_MinimumCoreEngineVersion));
    assert.isFalse(callbacks.hasMessage(KmwCompilerMessages.INFO_MinimumWebEngineVersion));
    assert.isTrue(callbacks.hasMessage(KmwCompilerMessages.ERROR_TouchLayoutIdentifierRequires15));
  });

  ['caps_lock', 'chiral_modifiers'].forEach((mode) => {
    it(`should give warning WARN_ExtendedShiftFlagsNotSupportedInKeymanWeb for v9 keyboards if ${mode} found`, async function() {
      const filenames = generateTestFilenames(`version_9_${mode}`);

      let result = await kmnCompiler.run(filenames.source, null);
      assert.isNotNull(result); // only a warning, so output is generated
      assert.isFalse(callbacks.hasMessage(KmnCompilerMessages.INFO_MinimumCoreEngineVersion));
      assert.isFalse(callbacks.hasMessage(KmwCompilerMessages.INFO_MinimumWebEngineVersion));
      assert.isTrue(callbacks.hasMessage(KmwCompilerMessages.WARN_ExtendedShiftFlagsNotSupportedInKeymanWeb));
    });

    it(`should select version 10 automatically if ${mode} found`, async function() {
      const filenames = generateTestFilenames(`version_auto_${mode}`);

      let result = await kmnCompiler.run(filenames.source, null);
      assert.isNotNull(result);

      // we expect only 1 of the info messages -- for the .kmx target (not 2)
      assert.equal(callbacks.messages.filter(item => item.code == KmnCompilerMessages.INFO_MinimumCoreEngineVersion).length, 1);
      assert.isTrue(callbacks.hasMessage(KmwCompilerMessages.INFO_MinimumWebEngineVersion));
      assert.isFalse(callbacks.hasMessage(KmwCompilerMessages.WARN_ExtendedShiftFlagsNotSupportedInKeymanWeb));

      const data = new TextDecoder('utf-8').decode(result.artifacts.js.data);
      assert.match(data, /KMINVER="10.0"/, `Could not find expected 'KMINVER="10.0"'`);
    });
  });

  it('should determine the minimum version correctly with `notany`', async function() {
    // Note that the logic being tested here is in kmx compiler.cpp, not kmw compiler
    const filenames = generateTestFilenames('version_notany');

    let result = await kmnCompiler.run(filenames.source, null);
    assert.isNotNull(result);
    assert.isTrue(callbacks.hasMessage(KmwCompilerMessages.INFO_MinimumWebEngineVersion));
    // we expect only 1 of the info messages -- for the .kmx target (not 2)
    assert.equal(callbacks.messages.filter(item => item.code == KmnCompilerMessages.INFO_MinimumCoreEngineVersion).length, 1);

    const data = new TextDecoder('utf-8').decode(result.artifacts.js.data);
    assert.match(data, /KMINVER="14.0"/, `Could not find expected 'KMINVER="14.0"'`);
  });

  it('should give an error if the minimum version specified in the keyboard does not support `notany`', async function() {
    // Note that the logic being tested here is in kmx compiler.cpp, not kmw compiler
    const filenames = generateTestFilenames('version_notany_10');

    let result = await kmnCompiler.run(filenames.source, null);
    assert.isNull(result);
    assert.isFalse(callbacks.hasMessage(KmnCompilerMessages.INFO_MinimumCoreEngineVersion));
    assert.isFalse(callbacks.hasMessage(KmwCompilerMessages.INFO_MinimumWebEngineVersion));
    assert.isTrue(callbacks.hasMessage(KmwCompilerMessages.ERROR_140FeatureOnlyContextAndNotAnyWeb));
  });

  it('should determine the minimum version correctly with special key caps on normal keys', async function() {
    const filenames = generateTestFilenames('version_special_key_caps');

    let result = await kmnCompiler.run(filenames.source, null);
    assert.isNotNull(result);
    assert.isTrue(callbacks.hasMessage(KmwCompilerMessages.INFO_MinimumWebEngineVersion));
    // we expect only 1 of the info messages -- for the .kmx target (not 2)
    assert.equal(callbacks.messages.filter(item => item.code == KmnCompilerMessages.INFO_MinimumCoreEngineVersion).length, 1);

    const data = new TextDecoder('utf-8').decode(result.artifacts.js.data);
    assert.match(data, /KMINVER="14.0"/, `Could not find expected 'KMINVER="14.0"'`);
  });

  it('should give warning WARN_TouchLayoutSpecialLabelOnNormalKey if the minimum version specified in the keyboard does not support special key caps on normal keys', async function() {
    const filenames = generateTestFilenames('version_special_key_caps_14');

    let result = await kmnCompiler.run(filenames.source, null);
    assert.isNotNull(result);
    assert.isFalse(callbacks.hasMessage(KmnCompilerMessages.INFO_MinimumCoreEngineVersion));
    assert.isFalse(callbacks.hasMessage(KmwCompilerMessages.INFO_MinimumWebEngineVersion));
    assert.isTrue(callbacks.hasMessage(KmwCompilerMessages.WARN_TouchLayoutSpecialLabelOnNormalKey));
  });

  it('should determine the minimum version correctly with v17 gestures', async function() {
    const filenames = generateTestFilenames('version_gestures');

    let result = await kmnCompiler.run(filenames.source, null);
    assert.isNotNull(result);
    assert.isTrue(callbacks.hasMessage(KmwCompilerMessages.INFO_MinimumWebEngineVersion));
    // we expect only 1 of the info messages -- for the .kmx target (not 2)
    assert.equal(callbacks.messages.filter(item => item.code == KmnCompilerMessages.INFO_MinimumCoreEngineVersion).length, 1);

    const data = new TextDecoder('utf-8').decode(result.artifacts.js.data);
    assert.match(data, /KMINVER="17.0"/, `Could not find expected 'KMINVER="17.0"'`);
  });

  it('should give warning HINT_TouchLayoutUsesUnsupportedGesturesDownlevel if the minimum version specified in the keyboard does not support special key caps on normal keys', async function() {
    const filenames = generateTestFilenames('version_gestures_16');

    let result = await kmnCompiler.run(filenames.source, null);
    assert.isNotNull(result);
    assert.isFalse(callbacks.hasMessage(KmnCompilerMessages.INFO_MinimumCoreEngineVersion));
    assert.isFalse(callbacks.hasMessage(KmwCompilerMessages.INFO_MinimumWebEngineVersion));
    assert.isTrue(callbacks.hasMessage(KmwCompilerMessages.HINT_TouchLayoutUsesUnsupportedGesturesDownlevel));
  });

  it('should give error ERROR_TouchLayoutInvalidIdentifier if a virtual key is badly formatted e.g. U_1234[_5678]', async function() {
    // #12870
    const filenames = generateTestFilenames('error_touch_layout_invalid_identifier');

    let result = await kmnCompiler.run(filenames.source, null);
    assert.isNull(result);
    assert.isFalse(callbacks.hasMessage(KmnCompilerMessages.INFO_MinimumCoreEngineVersion));
    assert.isFalse(callbacks.hasMessage(KmwCompilerMessages.INFO_MinimumWebEngineVersion));
    assert.isTrue(callbacks.hasMessage(KmwCompilerMessages.ERROR_TouchLayoutInvalidIdentifier));
    assert.isTrue(callbacks.hasMessage(KmwCompilerMessages.ERROR_InvalidTouchLayoutFile));
    assert.lengthOf(callbacks.messages, 2);
  });

  it('should generate correct index offsets (#12980) for context and context(n) when building a KMW keyboard', async function() {
    const filenames = generateTestFilenames('test_index_12980');

    let result = await kmnCompiler.run(filenames.source, null);
    assert.isNotNull(result);
    const data = new TextDecoder('utf-8').decode(result.artifacts.js.data);

    function doMatchKIO(s: string) {
      const regex = new RegExp('k\\.KIO\\(-1,this\\.'+s+',(.+?),t\\);');
      const m = regex.exec(data);
      assert.isNotNull(m, `Could not find match for ${s}`);
      assert.equal(m[1], '1', `Expected '${s}' usage to have an index parameter == 1`);
    }

    doMatchKIO('s_a_6');
    doMatchKIO('s_b_7');

    const matches = [...data.matchAll(/k\.KCXO\(-1,t,1,1\);/g)];
    assert.lengthOf(matches, 2);

    doMatchKIO('s_e_10');
    doMatchKIO('s_f_11');
  });
});

async function run_test_keyboard(kmnCompiler: KmnCompiler, id: string):
  Promise<{ result: KmnCompilerResult, actualCode: string, actual: ETLResult, expectedCode: string, expected: ETLResult }> {
  const filenames = generateTestFilenames(id);

  let result = await kmnCompiler.run(filenames.source, null);
  assert.isNotNull(result);

  let value = {
    result,
    actualCode: new TextDecoder().decode(result.artifacts.js.data),
    expectedCode: fs.readFileSync(filenames.fixture, 'utf8'),
    expected: <ETLResult>null,
    actual: <ETLResult>null,
  };
  value.actual = extractTouchLayout(value.actualCode);
  value.expected = extractTouchLayout(value.expectedCode);

  if(debug) {
    // This is mostly to allow us to verify that extractTouchLayout is doing what we want
    // fs.writeFileSync(filenames.binary + '.strip.js', value.actual.js);
    // fs.writeFileSync(filenames.fixture + '.strip.js', value.expected.js);
    fs.writeFileSync(filenames.binary, value.actualCode);
  }

  assert.deepEqual(value.actual.js.replaceAll(/\r\n/g, '\n'), value.expected.js.replaceAll(/\r\n/g, '\n'));
  assert.deepEqual(JSON.parse(value.actual.touchLayout), JSON.parse(value.expected.touchLayout));

  return value;
}
