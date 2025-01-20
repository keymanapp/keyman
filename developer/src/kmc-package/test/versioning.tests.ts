import 'mocha';
import { assert } from 'chai';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { makePathToFixture } from './helpers/index.js';
import { KmpCompiler } from '../src/compiler/kmp-compiler.js';

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
      const { kmpJsonData } = await kmpCompiler.transformKpsToKmpObject(kpsPath) ?? {};
      assert.isTrue(kmpJsonData !== null);
    });
  }
});
