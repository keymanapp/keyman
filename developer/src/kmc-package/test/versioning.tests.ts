import 'mocha';
import { assert } from 'chai';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { KmpJsonFile } from '@keymanapp/common-types';
import { KmpCompiler } from '../src/compiler/kmp-compiler.js';
import { unitTestEndpoints } from '../src/compiler/package-version-validator.js';
import { makePathToFixture } from './helpers/index.js';

// This unit test was translated from a Delphi test
// Keyman.Test.System.CompilePackageVersioningTest, but note the difference in
// test results documented below.

describe('package versioning', function () {

  const cases: [string,string][] = [
    ['test-single-version-1-package', 'test1.kps'],
    ['test-single-version-2-package', 'test2.kps'],

    // The test result for the following two packages has changed in Keyman 17,
    // in commit 92c8c6497e987a0583c62c91515884e6e36ece23, to 'shouldPass':
    //
    // `WARN_KeyboardVersionsDoNotMatchPackageVersion` has been removed, because
    // it did not really make sense; if 'FollowKeyboardVersion' is set, it could
    // not be raised, and otherwise, the author may wish to have separate
    // keyboard + package versions anyway.

    ['test-version-2-1-mismatch', 'test2-1.kps'],
    ['test-version-1-2-mismatch', 'test1-2.kps'],

    ['test-keyboard-1-package-2', 'test-keyboard-1-vs-package-2.kps'],
    ['test-package-1-keyboard-2', 'test-package-1-vs-keyboard-2.kps'],
  ];

  for(const [ caseTitle, filename ] of cases) {
    it(caseTitle, async function () {
      const callbacks = new TestCompilerCallbacks();
      const kmpCompiler = new KmpCompiler();
      assert.isTrue(await kmpCompiler.init(callbacks, null));

      const kpsPath = makePathToFixture('versioning', filename);
      const kmpJson: KmpJsonFile.KmpJsonFile = kmpCompiler.transformKpsToKmpObject(kpsPath);
      assert.isTrue(kmpJson !== null);
    });
  }

  it('should validate recognized version numbers', function() {
    const goodVersions = [
      '1.0',
      '1',
      '1.0.2',
      '2.2.3',
      '9',
      '0',
    ], badVersions = [
      '0001',
      '0.1m',
      '2.3.4.5',
      '1.03.4',
      'test',
      '1.0-beta',
      '',
      null,
      undefined,
      'xxx',
    ];

    for(const v of goodVersions) {
      assert.isTrue(unitTestEndpoints.isValidVersionNumber(v), `'${v}' should be recognized as a valid version number`);
    }

    for(const v of badVersions) {
      assert.isFalse(unitTestEndpoints.isValidVersionNumber(v), `'${v}' should be recognized as an invalid version number`);
    }
  });
});
