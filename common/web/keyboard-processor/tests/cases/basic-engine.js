import { assert } from 'chai';
import fs from 'fs';
import vm from 'vm';

import { KeyboardProcessor } from '@keymanapp/keyboard-processor';
import { KeyboardTest, NodeProctor } from '@keymanapp/recorder-core';

describe('Engine - Basic Simulation', function() {
  let testJSONtext = fs.readFileSync('../../test/resources/json/engine_tests/basic_lao_simulation.json');
  // Common test suite setup.
  let testSuite = new KeyboardTest(JSON.parse(testJSONtext));

  var keyboard;

  let device = {
    formFactor: 'desktop',
    OS: 'windows',
    browser: 'native'
  }

  before(function() {
    // -- START: Standard Recorder-based unit test loading boilerplate --
    // Load the keyboard.  We'll need a KeyboardProcessor instance as an intermediary.
    let kp = new KeyboardProcessor();

    // These two lines will load a keyboard from its file; headless-mode `registerKeyboard` will
    // automatically set the keyboard as active.
    var script = new vm.Script(fs.readFileSync('../../test/' + testSuite.keyboard.filename));
    script.runInThisContext();

    keyboard = kp.activeKeyboard;
    assert.equal(keyboard.id, "Keyboard_" + testSuite.keyboard.id);
    // --  END:  Standard Recorder-based unit test loading boilerplate --

    // This part provides extra assurance that the keyboard properly loaded.
    assert.equal(keyboard.id, "Keyboard_lao_2008_basic");
  });

  // Converts each test set into its own Mocha-level test.
  for(let set of testSuite.inputTestSets) {
    let proctor = new NodeProctor(keyboard, device, assert.equal);

    if(!proctor.compatibleWithSuite(testSuite)) {
      it.skip(set.toTestName() + " - Cannot run this test suite on Node.");
    } else {
      it(set.toTestName(), function() {
        // Refresh the proctor instance at runtime.
        let proctor = new NodeProctor(keyboard, device, assert.equal);
        set.test(proctor);
      });
    }
  }
});