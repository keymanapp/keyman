import 'mocha';
import { assert } from 'chai';
import { KMX, KmxFileReader } from '@keymanapp/common-types';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { makePathToFixture } from './helpers/index.js';

import { KmnCompiler } from '../src/main.js';

describe('Keyboard compiler features', function() {
  let callbacks: TestCompilerCallbacks = null;

  this.beforeAll(async function() {
    callbacks = new TestCompilerCallbacks();
  });

  this.beforeEach(function() {
    callbacks.clear();

  });

  this.afterEach(function() {
    if(this.currentTest.isFailed()) {
      callbacks.printMessages();
    }
  });



  // Test each Keyman file version target

  const versions = [
    // TODO-EMBED-OSK-IN-KMX: we should add a test for each version + also test automatic feature detection
    //                        and also verify that targetVersion works as expected with that
    {major: '16.0', vstr: '160', vernum: KMX.KMXFile.VERSION_160},
    {major: '17.0', vstr: '170', vernum: KMX.KMXFile.VERSION_170},
    {major: '19.0', vstr: '190', vernum: KMX.KMXFile.VERSION_190},
  ];

  for(const v of versions) {
    it(`should build a version ${v.major} keyboard`, async function() {
      const compiler = new KmnCompiler();
      assert.isTrue(await compiler.init(callbacks, {saveDebug: true}));
      assert.isTrue(compiler.verifyInitialized());

      const fixtureName = makePathToFixture('features', `version_${v.vstr}.kmn`);

      const result = await compiler.run(fixtureName, `version_${v.vstr}.kmx`);
      assert.isNotNull(result);

      const reader = new KmxFileReader();
      const keyboard = reader.read(result.artifacts.kmx.data);
      assert.equal(keyboard.fileVersion, v.vernum);
    });
  }

  [
    {s:'VERSION_60',  t:undefined,               e:KMX.KMXFile.VERSION_60},
    {s:'VERSION_170', t:KMX.KMXFile.VERSION_170, e:KMX.KMXFile.VERSION_170},
    {s:'VERSION_190', t:KMX.KMXFile.VERSION_190, e:KMX.KMXFile.VERSION_190},
  ].forEach(function(v) {
    it(`should build a minimum version keyboard to ${v.s} with targetVersion=${v.t}`, async function() {
        const compiler = new KmnCompiler();
        assert.isTrue(await compiler.init(callbacks, {
          saveDebug: true,
          targetVersion: v.t,
        }));
        assert.isTrue(compiler.verifyInitialized());
        const fixtureName = makePathToFixture('features', `unversioned.kmn`);

        const result = await compiler.run(fixtureName, `unversioned.kmx`);
        assert.isNotNull(result);

        const reader = new KmxFileReader();
        const keyboard = reader.read(result.artifacts.kmx.data);
        assert.equal(keyboard.fileVersion, v.e);
    });
  });
});
