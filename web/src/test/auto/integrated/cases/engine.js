import { assert } from '/node_modules/chai/chai.js';

import {
  loadKeyboardFromJSON,
  runKeyboardTestFromJSON,
  setupKMW,
  teardownKMW
} from "../test_utils.js";
import * as KMWRecorder from '/@keymanapp/keyman/build/tools/testing/recorder/lib/index.mjs';

describe('Engine - Browser Interactions', function() {
  this.timeout(testconfig.timeouts.scriptLoad);

  before(function() {
    fixture.setBase('fixtures');
    return setupKMW(null, testconfig.timeouts.scriptLoad);
  });

  beforeEach(function(done) {
    fixture.load("singleInput.html");

    window.setTimeout(function() {
      done()
    }, testconfig.timeouts.eventDelay);
  });

  after(function() {
    teardownKMW();
  });

  afterEach(function() {
    fixture.cleanup();
  });

  describe('RegisterStub', function() {
    it('RegisterStub on same keyboard twice', async function() {
      this.timeout(testconfig.timeouts.scriptLoad);

      var test_callback = async function() {
        assert.isNotNull(keyman.getKeyboard("lao_2008_basic", "lo"), "Keyboard stub was not registered!");
        assert.equal(keyman.getActiveKeyboard(), "Keyboard_lao_2008_basic", "Keyboard not set automatically!");
        keyman.removeKeyboards('lao_2008_basic');
        await Promise.resolve(); // so that the keyboard can reset.
        assert.equal(keyman.getActiveKeyboard(), '', "Keyboard not removed correctly!");
      }

      let finalPromise = loadKeyboardFromJSON("/keyboards/lao_2008_basic.json", testconfig.timeouts.scriptLoad)
        .then(test_callback);

      var stub = {
        'KI': 'Keyboard_lao_2008_basic',
        'KN': 'Lao 2008 Basic',
        'KLC': 'lo',
        'KL': 'Lao',
        'KF': 'resources/keyboards/lao_2008_basic.js'
      };

      await Promise.resolve();

      assert.equal(KeymanWeb.registerStub(stub), 1, "Registering existing keyboard should return 1!");
      return finalPromise;
    });

  });

  describe('Variable Stores', function() {
    this.timeout(testconfig.timeouts.scriptLoad + testconfig.timeouts.standard);

    beforeEach(function() {
      return loadKeyboardFromJSON("/keyboards/options_with_save.json", testconfig.timeouts.scriptLoad);
    });

    after(function() {
      keyman.removeKeyboards('options_with_save');
      fixture.cleanup();
    });

    it('Backing up and restoring (loadStore/saveStore)', function() {
      // Keyboard's default value is 0, corresponding to "no foo."
      var keyboardID = "options_with_save";
      var prefixedKeyboardID = "Keyboard_" + keyboardID;
      var storeName = "foo";

      return keyman.setActiveKeyboard(keyboardID, 'en').then(function() {
        // Alas, saveStore itself requires the keyboard to be active!
        KeymanWeb.saveStore(storeName, 1);

        // First, ensure that we get the same thing if we load the value immediately.
        var value = KeymanWeb.loadStore(prefixedKeyboardID, storeName, 0);
        assert.equal(value, 1, "loadStore did not see the value saved to initialize the test before resetting keyboard");

        // Reload the keyboard so that we can test its loaded value.
        keyman.removeKeyboards(keyboardID);

        return Promise.resolve();
      }).then(() => {
        return loadKeyboardFromJSON("/keyboards/options_with_save.json", testconfig.timeouts.scriptLoad);
      }).then(() => {
        return keyman.setActiveKeyboard(keyboardID, 'en');
      }).then(() => {
        // This requires proper storage to a cookie, as we'll be on a new instance of the same keyboard.
        var value = KeymanWeb.loadStore(prefixedKeyboardID, storeName, 0);
        assert.equal(value, 1, "Did not properly save and reload variable store setting");
      }).finally(() => {
        KeymanWeb.saveStore(storeName, 0);
      });
    });

    it("Multiple-sequence check", function() {
      this.timeout(testconfig.timeouts.standard + testconfig.timeouts.scriptLoad * 3);
      var keyboardID = "options_with_save";
      var storeName = "foo";

      return keyman.setActiveKeyboard(keyboardID, 'en').then(async function() {
        KeymanWeb.saveStore(storeName, 1);
        keyman.removeKeyboards(keyboardID);

        await Promise.resolve();
      }).then(() => {
        // First test:  expects option to be "on" from cookie-init setting, emitting "foo.", then turning option "off".
        return runKeyboardTestFromJSON('/engine_tests/options_with_save_1.json',
                                      {usingOSK: false},
                                      assert.equal,
                                      testconfig.timeouts.scriptLoad)
      }).then(async () => {
        // Reset the keyboard... again.
        keyman.removeKeyboards(keyboardID);

        await Promise.resolve();

        // Second test:  expects option to still be "off" b/c cookies.
        return runKeyboardTestFromJSON('/engine_tests/options_with_save_2.json',
                                      {usingOSK: false},
                                      assert.equal,
                                      testconfig.timeouts.scriptLoad);
      });
    });
  });

  // Performs basic processing system checks/tests to ensure the sequence testing
  // is based on correct assumptions about the code.
  describe('Integrated Simulation Checks', function() {
    this.timeout(testconfig.timeouts.standard);

    before(function() {
      this.timeout = testconfig.timeouts.scriptLoad;
      return loadKeyboardFromJSON("/keyboards/lao_2008_basic.json", testconfig.timeouts.scriptLoad);
    });

    beforeEach(function() {
      var inputElem = document.getElementById('singleton');
      keyman.setActiveElement(inputElem);
      inputElem.value = "";
    });

    after(function() {
      keyman.removeKeyboards('lao_2008_basic');
      fixture.cleanup();
    });

    /**
     * This can be surprisingly useful for catching browser-specific oddities related to keystroke
     * and keyboard-rule processing!  Keep this test cross-browser at all costs if possible!
     *
     * See #8830.
     */
    it('Simple Keypress', function() {
      var inputElem = document.getElementById('singleton');

      var lao_s_key_json = {"type": "key", "key":"s", "code":"KeyS","keyCode":83,"modifierSet":0,"location":0};
      var lao_s_event = new KMWRecorder.PhysicalInputEventSpec(lao_s_key_json);

      let eventDriver = new KMWRecorder.BrowserDriver(inputElem);
      eventDriver.simulateEvent(lao_s_event);

      if(inputElem['base']) {
        inputElem = inputElem['base'];
      }
      assert.equal(inputElem.value, "ຫ");
    });

    it('Simple OSK click', async function() {
      var inputElem = document.getElementById('singleton');

      var lao_s_osk_json = {"type": "osk", "keyID": 'shift-K_S'};
      var lao_s_event = new KMWRecorder.OSKInputEventSpec(lao_s_osk_json);

      let eventDriver = new KMWRecorder.BrowserDriver(inputElem);
      await eventDriver.simulateEvent(lao_s_event);

      if(inputElem['base']) {
        inputElem = inputElem['base'];
      }
      assert.equal(inputElem.value, ";");
    });
  });

  describe('Sequence Simulation Checks', function() {
    this.timeout(testconfig.timeouts.scriptLoad);

    it('Keyboard simulation', async function() {
      return await runKeyboardTestFromJSON('/engine_tests/basic_lao_simulation.json', {usingOSK: false}, assert.equal, testconfig.timeouts.scriptLoad);
    });

    it('OSK simulation', async function() {
      return await runKeyboardTestFromJSON('/engine_tests/basic_lao_simulation.json', {usingOSK: true}, assert.equal, testconfig.timeouts.scriptLoad);
    })
  });
});

describe('Unmatched Final Groups', function() {
  this.timeout(testconfig.timeouts.scriptLoad);

  before(function() {
    fixture.setBase('fixtures');
    return setupKMW(null, testconfig.timeouts.scriptLoad + testconfig.timeouts.eventDelay);
  });

  beforeEach(function(done) {
    fixture.load("singleTextArea.html");

    window.setTimeout(function() {
      done()
    }, testconfig.timeouts.eventDelay);
  });

  after(function() {
    teardownKMW();
  });

  afterEach(function() {
    fixture.cleanup();
  });

  it('matches rule from early group AND performs default behavior', async function() {
    // While a TAB-oriented version would be nice, it's much harder to write the test
    // to detect change in last input element.
    return await runKeyboardTestFromJSON('/engine_tests/ghp_enter.json', {usingOSK: true}, assert.equal, testconfig.timeouts.scriptLoad);
  });
});

// Kept separate to maintain an extra-clean setup for this test.
describe('Engine - Browser Interactions', function() {
  this.timeout(testconfig.timeouts.scriptLoad);

  before(function() {
    fixture.setBase('fixtures');
  });

  beforeEach(function() {
    fixture.load("singleInput.html");
    return setupKMW(null, testconfig.timeouts.scriptLoad);
  });

  afterEach(function() {
    fixture.cleanup();
    teardownKMW();
  });

  describe('Keyboard Loading', function() {
    it('Local', function() {
      this.timeout(testconfig.timeouts.scriptLoad);

      return loadKeyboardFromJSON("/keyboards/lao_2008_basic.json",
                                  testconfig.timeouts.scriptLoad).then(async function() {
        assert.isNotNull(keyman.getKeyboard("lao_2008_basic", "lo"), "Keyboard stub was not registered!");
        assert.equal(keyman.getActiveKeyboard(), "Keyboard_lao_2008_basic", "Keyboard not set automatically!");
        keyman.removeKeyboards('lao_2008_basic');

        await Promise.resolve();
        assert.equal(keyman.getActiveKeyboard(), '', "Keyboard not removed correctly!");
      });
    });

    it('Automatically sets first available keyboard', function() {
      this.timeout(2 * testconfig.timeouts.scriptLoad);

      return loadKeyboardFromJSON("/keyboards/lao_2008_basic.json",
                                  testconfig.timeouts.scriptLoad,
                                  {passive: true}).then(() => {
        // Because we're loading the keyboard 'passively', KMW's setActiveKeyboard function is auto-called
        // on the stub-add.  That specific call (for first keyboard auto-activation) is outside of KMW's
        // current Promise chain, so we can't _directly_ rely on a KMW Promise to test it.
        return new Promise((resolve) => {
          let hasResolved = false;
          // So, we give KMW the time needed for auto-activation to happen, polling a bit actively so that we don't
          // wait unnecessarily long after it occurs.
          let absoluteTimer = window.setTimeout(() => {
            if(!hasResolved) {
              resolve();
              hasResolved = true;
            }

            window.clearTimeout(intervalTimer);
          }, testconfig.timeouts.scriptLoad);

          let intervalTimer = window.setInterval(() => {
            if(keyman.getActiveKeyboard() != '') {
              window.clearTimeout(intervalTimer);
              window.clearTimeout(absoluteTimer);

              if(!hasResolved) {
                resolve();
                hasResolved = true;
              }
            }
          }, 50);
        });
        // Once this delay-Promise resolves successfully (either way)...
      }).then(async function() {
        assert.isNotNull(keyman.getKeyboard("lao_2008_basic", "lo"), "Keyboard stub was not registered!");
        assert.equal(keyman.getActiveKeyboard(), "Keyboard_lao_2008_basic", "Keyboard not set automatically!");
        keyman.removeKeyboards('lao_2008_basic');

        await Promise.resolve();
        assert.equal(keyman.getActiveKeyboard(), '', "Keyboard not removed correctly!");
      });  // THEN we run our checks.
    });
  });
});