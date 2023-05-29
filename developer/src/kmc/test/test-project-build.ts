import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { assert } from 'chai';
import 'mocha';
import { BuildProject } from '../src/commands/build/BuildProject.js';
import { makePathToFixture } from './helpers/index.js';

const callbacks = new TestCompilerCallbacks();

describe('BuildProject', function () {
  it('should build a keyboard project', async function() {
    const builder = new BuildProject();
    const path = makePathToFixture('relative_paths', 'k_000___null_keyboard.kpj');
    let result = await builder.build(path, callbacks, {
      compilerVersion: false,
      compilerWarningsAsErrors: true,
      debug: false,
      warnDeprecatedCode: true,
    });
    // 4 messages == starting build x 2, build successful x 2
    // callbacks.printMessages();
    assert.equal(callbacks.messages.length, 4);
    assert.isTrue(result);
  });
});
