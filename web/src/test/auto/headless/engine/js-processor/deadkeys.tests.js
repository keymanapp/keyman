import { assert } from 'chai';
import fs from 'fs';

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

import { MinimalKeymanGlobal } from 'keyman/engine/keyboard';
import { KeyboardInterface } from 'keyman/engine/js-processor';
import { NodeKeyboardLoader } from 'keyman/engine/keyboard/node-keyboard-loader';
import { KeyboardTest, NodeProctor } from '@keymanapp/recorder-core';

import { getKeymanRoot } from '../../getKeymanRoot.mjs';
const KEYMAN_ROOT = getKeymanRoot();

describe('Engine - Deadkeys', function() {
  let testJSONtext = fs.readFileSync(require.resolve('@keymanapp/common-test-resources/json/engine_tests/deadkeys.json'));
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
    let keyboardLoader = new NodeKeyboardLoader(new KeyboardInterface({}, MinimalKeymanGlobal));
    let keyboard = await keyboardLoader.loadKeyboardFromPath(KEYMAN_ROOT + '/common/test/' + testSuite.keyboard.filename);
    keyboardWithHarness = keyboardLoader.harness;
    keyboardWithHarness.activeKeyboard = keyboard;

    assert.equal(keyboard.id, "Keyboard_" + testSuite.keyboard.id);
    // --  END:  Standard Recorder-based unit test loading boilerplate --

    // This part provides extra assurance that the keyboard properly loaded.
    assert.equal(keyboard.id, "Keyboard_test_deadkeys");
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