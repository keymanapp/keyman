var assert = require('chai').assert;
let fs = require('fs');
let vm = require('vm');

let KeyboardProcessor = require('../../build/index.bundled.js');
let KMWRecorder = require('../../../recorder/build/nodeProctor');

// Required initialization setup.
global.com = KeyboardProcessor.com; // exports all keyboard-processor namespacing.
let KeyboardInterface = com.keyman.text.KeyboardInterface;
let Codes = com.keyman.text.Codes;

describe('Engine - Chirality', function() {
  let testJSONtext = fs.readFileSync('../../test/resources/json/engine_tests/chirality.json');
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
    assert.equal(keyboard.id, "Keyboard_test_chirality");
  });

  // Converts each test set into its own Mocha-level test.
  for(let set of testSuite.inputTestSets) {
    let proctor = new KMWRecorder.NodeProctor(keyboard, device, assert.equal);

    if(!proctor.compatibleWithSuite(testSuite)) {
      it.skip(set.toTestName() + " - Cannot run this test suite on Node.");
    } else if(set.constraint.target == 'hardware') {
      it(set.toTestName(), function() {
        // Refresh the proctor instance at runtime.
        let proctor = new KMWRecorder.NodeProctor(keyboard, device, assert.equal);
        set.test(proctor);
      });
    } else {
      it.skip(set.toTestName() + " - modifier state simulation for OSK not yet supported in headless KeyboardProcessor");
    }
  }

  describe("Chiral modifier mapping", function() {
    let VIRTUAL_KEY_CODE = Codes.modifierCodes["VIRTUAL_KEY"];

    // An incomplete instance of text.KeyEvent; we only **_really_** need the one field,
    // `Lmodifiers`, though.
    let BASE_KEY_EVENT = {
      Lcode: 64,
      Lstates: 0,
      Lmodifiers: VIRTUAL_KEY_CODE, // The one we'll modify for this set of tests.
      LisVirtualKey: true
    };

    it("does not affect non-chiral KeyEvents - CTRL only", function() {
      let key = Object.assign({}, BASE_KEY_EVENT);
      key.Lmodifiers |= Codes.modifierCodes["CTRL"];

      let targetModifiers = VIRTUAL_KEY_CODE | Codes.modifierCodes["CTRL"];

      // We should get the same results whether or not there actually is a corresponding modifier
      // expected by the rule we're examining.
      let mappedModifiers = KeyboardInterface.getEventRuleModifiers(key, VIRTUAL_KEY_CODE);
      assert.equal(targetModifiers, mappedModifiers);

      mappedModifiers = KeyboardInterface.getEventRuleModifiers(key, VIRTUAL_KEY_CODE | Codes.modifierCodes["CTRL"]);
      assert.equal(targetModifiers, mappedModifiers);

      mappedModifiers = KeyboardInterface.getEventRuleModifiers(key, VIRTUAL_KEY_CODE | Codes.modifierCodes["LCTRL"]);
      assert.equal(targetModifiers, mappedModifiers);

      mappedModifiers = KeyboardInterface.getEventRuleModifiers(key, VIRTUAL_KEY_CODE | Codes.modifierCodes["RCTRL"]);
      assert.equal(targetModifiers, mappedModifiers);
    });

    it("does not affect non-chiral KeyEvents - ALT only", function() {
      let key = Object.assign({}, BASE_KEY_EVENT);
      key.Lmodifiers |= Codes.modifierCodes["ALT"];

      let targetModifiers = VIRTUAL_KEY_CODE | Codes.modifierCodes["ALT"];

      // We should get the same results whether or not there actually is a corresponding modifier
      // expected by the rule we're examining.
      mappedModifiers = KeyboardInterface.getEventRuleModifiers(key, VIRTUAL_KEY_CODE);
      assert.equal(targetModifiers, mappedModifiers);

      mappedModifiers = KeyboardInterface.getEventRuleModifiers(key, VIRTUAL_KEY_CODE | Codes.modifierCodes["ALT"]);
      assert.equal(targetModifiers, mappedModifiers);

      mappedModifiers = KeyboardInterface.getEventRuleModifiers(key, VIRTUAL_KEY_CODE | Codes.modifierCodes["LALT"]);
      assert.equal(targetModifiers, mappedModifiers);

      mappedModifiers = KeyboardInterface.getEventRuleModifiers(key, VIRTUAL_KEY_CODE | Codes.modifierCodes["RALT"]);
      assert.equal(targetModifiers, mappedModifiers);
    });

    it("does not affect non-chiral KeyEvents - ALT + CTRL", function() {
      let key = Object.assign({}, BASE_KEY_EVENT);
      key.Lmodifiers |= Codes.modifierCodes["ALT"] | Codes.modifierCodes["CTRL"];

      let targetModifiers = VIRTUAL_KEY_CODE | Codes.modifierCodes["ALT"] | Codes.modifierCodes["CTRL"];

      // We should get the same results whether or not there actually is a corresponding modifier
      // expected by the rule we're examining.
      mappedModifiers = KeyboardInterface.getEventRuleModifiers(key, VIRTUAL_KEY_CODE);
      assert.equal(targetModifiers, mappedModifiers);

      let ctrlPlusAlt = Codes.modifierCodes["ALT"] | Codes.modifierCodes["CTRL"];
      mappedModifiers = KeyboardInterface.getEventRuleModifiers(key, VIRTUAL_KEY_CODE | ctrlPlusAlt);
      assert.equal(targetModifiers, mappedModifiers);

      let leftVariant = Codes.modifierCodes["LALT"] | Codes.modifierCodes["LCTRL"];
      mappedModifiers = KeyboardInterface.getEventRuleModifiers(key, VIRTUAL_KEY_CODE | leftVariant);
      assert.equal(targetModifiers, mappedModifiers);

      let rightVariant = Codes.modifierCodes["RALT"] | Codes.modifierCodes["RCTRL"];
      mappedModifiers = KeyboardInterface.getEventRuleModifiers(key, VIRTUAL_KEY_CODE | rightVariant);
      assert.equal(targetModifiers, mappedModifiers);

      let mixedVariant1 = Codes.modifierCodes["LALT"] | Codes.modifierCodes["CTRL"];
      mappedModifiers = KeyboardInterface.getEventRuleModifiers(key, VIRTUAL_KEY_CODE | mixedVariant1);
      assert.equal(targetModifiers, mappedModifiers);

      let mixedVariant2 = Codes.modifierCodes["ALT"] | Codes.modifierCodes["RCTRL"];
      mappedModifiers = KeyboardInterface.getEventRuleModifiers(key, VIRTUAL_KEY_CODE | mixedVariant2);
      assert.equal(targetModifiers, mappedModifiers);
    });

    it("maps chiral modifiers when the rule does not expect a matching modifier", function() {
      let ctrlTargetModifiers = VIRTUAL_KEY_CODE | Codes.modifierCodes["CTRL"];

      let ctrlKey1 = Object.assign({}, BASE_KEY_EVENT);
      ctrlKey1.Lmodifiers |= Codes.modifierCodes["LCTRL"];

      // We should get the same results whether or not there actually is a corresponding modifier
      // expected by the rule we're examining.
      let mappedModifiers = KeyboardInterface.getEventRuleModifiers(ctrlKey1, VIRTUAL_KEY_CODE);
      assert.equal(ctrlTargetModifiers, mappedModifiers);

      let ctrlKey2 = Object.assign({}, BASE_KEY_EVENT);
      ctrlKey2.Lmodifiers |= Codes.modifierCodes["RCTRL"];

      // We should get the same results whether or not there actually is a corresponding modifier
      // expected by the rule we're examining.
      mappedModifiers = KeyboardInterface.getEventRuleModifiers(ctrlKey2, VIRTUAL_KEY_CODE);
      assert.equal(ctrlTargetModifiers, mappedModifiers);

      let altTargetModifiers = VIRTUAL_KEY_CODE | Codes.modifierCodes["ALT"];

      let altKey1 = Object.assign({}, BASE_KEY_EVENT);
      altKey1.Lmodifiers |= Codes.modifierCodes["LALT"];

      // We should get the same results whether or not there actually is a corresponding modifier
      // expected by the rule we're examining.
      mappedModifiers = KeyboardInterface.getEventRuleModifiers(altKey1, VIRTUAL_KEY_CODE);
      assert.equal(altTargetModifiers, mappedModifiers);

      let altKey2 = Object.assign({}, BASE_KEY_EVENT);
      altKey2.Lmodifiers |= Codes.modifierCodes["RALT"];

      // We should get the same results whether or not there actually is a corresponding modifier
      // expected by the rule we're examining.
      mappedModifiers = KeyboardInterface.getEventRuleModifiers(altKey2, VIRTUAL_KEY_CODE);
      assert.equal(altTargetModifiers, mappedModifiers);
    });

    it("maps chiral modifiers when the rule expects the non-chiral version", function() {
      let ctrlTargetModifiers = VIRTUAL_KEY_CODE | Codes.modifierCodes["CTRL"];

      let ctrlKey1 = Object.assign({}, BASE_KEY_EVENT);
      ctrlKey1.Lmodifiers |= Codes.modifierCodes["LCTRL"];

      // We should get the same results whether or not there actually is a corresponding modifier
      // expected by the rule we're examining.
      let mappedModifiers = KeyboardInterface.getEventRuleModifiers(ctrlKey1, VIRTUAL_KEY_CODE | Codes.modifierCodes["CTRL"]);
      assert.equal(ctrlTargetModifiers, mappedModifiers);

      let ctrlKey2 = Object.assign({}, BASE_KEY_EVENT);
      ctrlKey2.Lmodifiers |= Codes.modifierCodes["RCTRL"];

      // We should get the same results whether or not there actually is a corresponding modifier
      // expected by the rule we're examining.
      mappedModifiers = KeyboardInterface.getEventRuleModifiers(ctrlKey2, VIRTUAL_KEY_CODE | Codes.modifierCodes["CTRL"]);
      assert.equal(ctrlTargetModifiers, mappedModifiers);

      let altTargetModifiers = VIRTUAL_KEY_CODE | Codes.modifierCodes["ALT"];

      let altKey1 = Object.assign({}, BASE_KEY_EVENT);
      altKey1.Lmodifiers |= Codes.modifierCodes["LALT"];

      // We should get the same results whether or not there actually is a corresponding modifier
      // expected by the rule we're examining.
      mappedModifiers = KeyboardInterface.getEventRuleModifiers(altKey1, VIRTUAL_KEY_CODE | Codes.modifierCodes["ALT"]);
      assert.equal(altTargetModifiers, mappedModifiers);

      let altKey2 = Object.assign({}, BASE_KEY_EVENT);
      altKey2.Lmodifiers |= Codes.modifierCodes["RALT"];

      // We should get the same results whether or not there actually is a corresponding modifier
      // expected by the rule we're examining.
      mappedModifiers = KeyboardInterface.getEventRuleModifiers(altKey2, VIRTUAL_KEY_CODE | Codes.modifierCodes["ALT"]);
      assert.equal(altTargetModifiers, mappedModifiers);
    });

    it("does not map nonchiral modifiers", function() {
      let keyEvent = Object.assign({}, BASE_KEY_EVENT);
      keyEvent.Lmodifiers |= Codes.modifierCodes["ALT"] | Codes.modifierCodes["CTRL"];

      let targetModifiers = VIRTUAL_KEY_CODE | Codes.modifierCodes["ALT"] | Codes.modifierCodes["CTRL"];

      let mappedModifiers = KeyboardInterface.getEventRuleModifiers(keyEvent, VIRTUAL_KEY_CODE | Codes.modifierCodes["LALT"] | Codes.modifierCodes["LCTRL"]);
      assert.equal(targetModifiers, mappedModifiers);

      mappedModifiers = KeyboardInterface.getEventRuleModifiers(keyEvent, VIRTUAL_KEY_CODE | Codes.modifierCodes["RALT"] | Codes.modifierCodes["RCTRL"]);
      assert.equal(targetModifiers, mappedModifiers);
    });

    it("does not map chirals when a chiral version is expected", function() {
      let keyEvent = Object.assign({}, BASE_KEY_EVENT);
      keyEvent.Lmodifiers |= Codes.modifierCodes["LALT"] | Codes.modifierCodes["LCTRL"];

      let targetModifiers = VIRTUAL_KEY_CODE | Codes.modifierCodes["LALT"] | Codes.modifierCodes["LCTRL"];

      let mappedModifiers = KeyboardInterface.getEventRuleModifiers(keyEvent, VIRTUAL_KEY_CODE | Codes.modifierCodes["LALT"] | Codes.modifierCodes["LCTRL"]);
      assert.equal(targetModifiers, mappedModifiers);

      mappedModifiers = KeyboardInterface.getEventRuleModifiers(keyEvent, VIRTUAL_KEY_CODE | Codes.modifierCodes["RALT"] | Codes.modifierCodes["RCTRL"]);
      assert.equal(targetModifiers, mappedModifiers);
    });

    it("handles mixed chiral/nonchiral rules - case 1", function() {
      let keyEvent = Object.assign({}, BASE_KEY_EVENT);
      keyEvent.Lmodifiers |= Codes.modifierCodes["LALT"] | Codes.modifierCodes["LCTRL"];

      let modifierTarget = VIRTUAL_KEY_CODE | Codes.modifierCodes["LALT"] | Codes.modifierCodes["CTRL"];

      let mappedModifiers = KeyboardInterface.getEventRuleModifiers(keyEvent, VIRTUAL_KEY_CODE | Codes.modifierCodes["LALT"] | Codes.modifierCodes["CTRL"]);
      assert.equal(modifierTarget, mappedModifiers);
    });

    it("handles mixed chiral/nonchiral rules - case 2", function() {
      let keyEvent = Object.assign({}, BASE_KEY_EVENT);
      keyEvent.Lmodifiers |= Codes.modifierCodes["LALT"] | Codes.modifierCodes["LCTRL"];

      let modifierTarget = VIRTUAL_KEY_CODE | Codes.modifierCodes["ALT"] | Codes.modifierCodes["LCTRL"];

      let mappedModifiers = KeyboardInterface.getEventRuleModifiers(keyEvent, VIRTUAL_KEY_CODE | Codes.modifierCodes["ALT"] | Codes.modifierCodes["RCTRL"]);
      assert.equal(modifierTarget, mappedModifiers);
    });

    it("handles mixed chiral/nonchiral rules - case 3", function() {
      let keyEvent = Object.assign({}, BASE_KEY_EVENT);
      keyEvent.Lmodifiers |= Codes.modifierCodes["ALT"] | Codes.modifierCodes["LCTRL"] | Codes.modifierCodes["SHIFT"];

      let modifierTarget = VIRTUAL_KEY_CODE | Codes.modifierCodes["ALT"] | Codes.modifierCodes["LCTRL"] | Codes.modifierCodes["SHIFT"];

      mappedModifiers = KeyboardInterface.getEventRuleModifiers(keyEvent, VIRTUAL_KEY_CODE | Codes.modifierCodes["LALT"] | Codes.modifierCodes["RCTRL"]);
      assert.equal(modifierTarget, mappedModifiers);
    });
  });
});