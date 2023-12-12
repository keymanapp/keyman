import 'mocha';
import { assert } from 'chai';
import { KmnCompiler } from '../src/main.js';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { makePathToFixture } from './helpers/index.js';
import { KMX, KmxFileReader } from '@keymanapp/common-types';

describe('Keyboard compiler features', function() {
  let compiler: KmnCompiler = null;
  let callbacks: TestCompilerCallbacks = null;

  this.beforeAll(async function() {
    compiler = new KmnCompiler();
    callbacks = new TestCompilerCallbacks();
    assert(await compiler.init(callbacks, {saveDebug: true}));
    assert(compiler.verifyInitialized());
  });

  beforeEach(function() {
    callbacks.clear();
  });

  // Test each Keyman file version target

  const versions = [
    // TODO(lowpri): we should add a test for each version + also test automatic feature detection
    ['16.0', '160', KMX.KMXFile.VERSION_160],
    ['17.0', '170', KMX.KMXFile.VERSION_170],
  ];

  for(const v of versions) {
    it(`should build a version ${v[0]} keyboard`, async function() {
      const fixtureName = makePathToFixture('features', `version_${v[1]}.kmn`);

      const result = await compiler.run(fixtureName, `version_${v[1]}.kmx`);
      if(result === null) callbacks.printMessages();
      assert.isNotNull(result);

      const reader = new KmxFileReader();
      const keyboard = reader.read(result.artifacts.kmx.data);
      assert.equal(keyboard.fileVersion, v[2]);
    });
  }
});
