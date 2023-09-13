import { TestCompilerCallbacks } from '@keymanapp/developer-test-helpers';
import { assert } from 'chai';
import 'mocha';
import { BuildProject } from '../src/commands/buildClasses/BuildProject.js';
import { makePathToFixture } from './helpers/index.js';
import { InfrastructureMessages } from '../src/messages/infrastructureMessages.js';

const callbacks = new TestCompilerCallbacks();

describe('BuildProject', function () {
  it('should build a keyboard project', async function() {
    const builder = new BuildProject();
    const path = makePathToFixture('relative_paths', 'k_000___null_keyboard.kpj');
    let result = await builder.build(path, callbacks, {
      shouldAddCompilerVersion: false,
      compilerWarningsAsErrors: true,
      saveDebug: false,
      warnDeprecatedCode: true,
      logLevel: 'info'
    });
    if(callbacks.messages.length != 6) {
      callbacks.printMessages();
    }
    assert.equal(callbacks.messages.length, 6);
    const messages = [
      InfrastructureMessages.INFO_BuildingFile, // kmn
      InfrastructureMessages.INFO_FileBuiltSuccessfully,
      InfrastructureMessages.INFO_BuildingFile, // kps
      InfrastructureMessages.INFO_FileBuiltSuccessfully,
      InfrastructureMessages.INFO_BuildingFile, // keyboard_info
      InfrastructureMessages.INFO_FileBuiltSuccessfully,
    ];
    for(let i = 0; i < messages.length; i++) {
      assert.equal(callbacks.messages[i].code, messages[i]);
    }

    assert.isTrue(result);
  });
});
