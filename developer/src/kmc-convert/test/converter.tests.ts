import 'mocha';
import { assert } from 'chai';
import { Converter } from '../src/main.js';
import { CompilerOptions, } from "@keymanapp/developer-utils";
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
    const options: CompilerOptions = { saveDebug: true, shouldAddCompilerVersion: false };
    assert(await converter.init(callbacks, options));
  });

});
