import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { assert } from 'chai';
import 'mocha';
import { BuildProject } from '../src/commands/buildClasses/BuildProject.js';
import { makePathToFixture } from './helpers/index.js';
import { InfrastructureMessages } from '../src/messages/infrastructureMessages.js';
import { clearOptions } from '@keymanapp/developer-utils';

const callbacks = new TestCompilerCallbacks();

describe('BuildProject', function () {
  it('should build a keyboard project', async function() {
    clearOptions();
    const builder = new BuildProject();
    const path = makePathToFixture('relative_paths', 'k_000___null_keyboard.kpj');
    let result = await builder.build(path, null, callbacks, {
      shouldAddCompilerVersion: false,
      compilerWarningsAsErrors: true,
      saveDebug: false,
      warnDeprecatedCode: true,
      logLevel: 'info',
    });
    const messages = [
      InfrastructureMessages.HINT_ProjectIsVersion10,
      InfrastructureMessages.INFO_BuildingFile, // kmn
      InfrastructureMessages.INFO_FileBuiltSuccessfully,
      InfrastructureMessages.INFO_BuildingFile, // kps
      InfrastructureMessages.INFO_FileBuiltSuccessfully,
    ];
    if(callbacks.messages.length != messages.length) {
      callbacks.printMessages();
    }
    assert.equal(callbacks.messages.length, messages.length);
    for(let i = 0; i < messages.length; i++) {
      assert.equal(callbacks.messages[i].code, messages[i]);
    }

    assert.isTrue(result);
  });
});
