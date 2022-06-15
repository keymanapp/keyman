var assert = require('chai').assert;
let fs = require('fs');
let vm = require('vm');

let KeyboardProcessor = require('../../../build/index.bundled.js');

// Required initialization setup.
global.com = KeyboardProcessor.com; // exports all keyboard-processor namespacing.

let KMWRecorder = require('../../../../recorder/build/nodeProctor');

describe('Engine - Unmatched Final Groups', function() {
  let testJSONtext = fs.readFileSync('../../test/resources/json/engine_tests/ghp_enter.json');
  // Common test suite setup.
  let testSuite = new KMWRecorder.KeyboardTest(JSON.parse(testJSONtext));

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
    assert.equal(keyboard.id, "Keyboard_galaxie_hebrew_positional");
  });

  it('Emits default enter AND matches rule from early group', function() {
    let proctor = new KMWRecorder.NodeProctor(keyboard, device, assert.equal);
    testSuite.test(proctor);
  });
});