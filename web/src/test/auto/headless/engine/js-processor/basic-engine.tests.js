import { assert } from 'chai';
import fs from 'fs';


import { createRequire } from 'module';
const require = createRequire(import.meta.url);

import { MinimalKeymanGlobal } from 'keyman/engine/keyboard';
import { JSKeyboardInterface } from 'keyman/engine/js-processor';
import { NodeKeyboardLoader } from 'keyman/test/resources';
import { KeyboardTest, NodeProctor } from '@keymanapp/recorder-core';

import { env } from 'node:process';
import { fileURLToPath } from 'node:url';
import { dirname } from 'node:path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const KEYMAN_ROOT = env.KEYMAN_ROOT ?? (__dirname + '/../../../../../../../');

describe('Engine - Basic Simulation', function() {
  let testJSONtext = fs.readFileSync(require.resolve('@keymanapp/common-test-resources/json/engine_tests/basic_lao_simulation.json'));
  // Common test suite setup.
  let testSuite = new KeyboardTest(JSON.parse(testJSONtext));

  let keyboardWithHarness;

  let device = {
    formFactor: 'desktop',
    OS: 'windows',
    browser: 'native'
  }

  before(async function() {
    // -- START: Standard Recorder-based unit test loading boilerplate --
    let keyboardLoader = new NodeKeyboardLoader(new JSKeyboardInterface({}, MinimalKeymanGlobal));
    let keyboard = await keyboardLoader.loadKeyboardFromPath(KEYMAN_ROOT + '/common/test/' + testSuite.keyboard.filename);
    keyboardWithHarness = keyboardLoader.harness;
    keyboardWithHarness.activeKeyboard = keyboard;

    assert.equal(keyboard.id, "Keyboard_" + testSuite.keyboard.id);
    // --  END:  Standard Recorder-based unit test loading boilerplate --

    // This part provides extra assurance that the keyboard properly loaded.
    assert.equal(keyboard.id, "Keyboard_lao_2008_basic");
  });

  // Converts each test set into its own Mocha-level test.
  for(let set of testSuite.inputTestSets) {
    let proctor = new NodeProctor(keyboardWithHarness, device, assert.equal);

    if(!proctor.compatibleWithSuite(testSuite)) {
      it.skip(set.toTestName() + " - Cannot run this test suite on Node.");
    } else {
      it(set.toTestName(), async function() {
        // Refresh the proctor instance at runtime.
        let proctor = new NodeProctor(keyboardWithHarness, device, assert.equal);
        await set.test(proctor);
      });
    }
  }
});