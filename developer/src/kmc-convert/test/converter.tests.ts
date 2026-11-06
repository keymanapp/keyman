/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import 'mocha';
import { assert } from 'chai';
import { Converter, ConverterOptions } from '../src/main.js';
import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';

describe('converter class', function () {
  it('should throw on failure', async function () {
    const converter = new Converter();
    try {
      await converter.init(null, null);
      assert.fail('Expected exception');
    } catch (e) {
      assert.ok(e);
    }
  });

  it('should start', async function () {
    const converter = new Converter();
    const callbacks = new TestCompilerCallbacks();
    const options: ConverterOptions = { saveDebug: true, shouldAddCompilerVersion: false, target: '.kmn' };
    assert(await converter.init(callbacks, options));
  });

});
