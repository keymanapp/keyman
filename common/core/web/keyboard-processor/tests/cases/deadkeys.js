var assert = require('chai').assert;
let fs = require('fs');
let vm = require('vm');

let KeyboardProcessor = require('../../dist');
let KMWRecorder = require('../../../tools/recorder/dist/nodeProctor');

// Required initialization setup.
global.com = KeyboardProcessor.com; // exports all keyboard-processor namespacing.

describe('Engine - Deadkeys', function() {
  var keyboard;
  var testSuite;
  let device = {
    formFactor: 'desktop',
    OS: 'windows',
    browser: 'native'
  }

  before(function() {
    // Load the test suite.
    let testJSONtext = fs.readFileSync('../tests/resources/json/engine_tests/deadkeys.json');

    // -- START: Standard Recorder-based unit test loading boilerplate --
    testSuite = new KMWRecorder.KeyboardTest(JSON.parse(testJSONtext));

    // Load the keyboard.  We'll need a KeyboardProcessor instance as an intermediary.
    let kp = new KeyboardProcessor();

    // These two lines will load a keyboard from its file; headless-mode `registerKeyboard` will
    // automatically set the keyboard as active.
    var script = new vm.Script(fs.readFileSync('../tests/' + testSuite.keyboard.filename));
    script.runInThisContext();

    keyboard = kp.activeKeyboard;
    assert.equal(keyboard.id, "Keyboard_" + testSuite.keyboard.id);
    // --  END:  Standard Recorder-based unit test loading boilerplate --

    // This part provides extra assurance that the keyboard properly loaded.
    assert.equal(keyboard.id, "Keyboard_test_deadkeys");
  });

  it('Keyboard simulation', function() {
    let proctor = new KMWRecorder.NodeProctor(keyboard, device, assert.equal);
    testSuite.test(proctor);
  });
});