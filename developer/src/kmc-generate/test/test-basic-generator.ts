/*
 * Keyman is copyright (C) SIL International. MIT License.
 */

import 'mocha';
import { assert } from 'chai';
import { BasicGenerator } from '../src/basic-generator.js';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { options } from './shared-options.js';

describe('BasicGenerator', function () {
  it('should configure default settings', async function() {
    const bg = new BasicGenerator();
    const callbacks = new TestCompilerCallbacks();
    assert(await bg.init(callbacks, options));
    bg.test_preGenerate();

    assert.equal(bg.test_tokenMap['$NAME'], 'Sample Project');
  });

  // TODO-GENERATE: test transform
});

