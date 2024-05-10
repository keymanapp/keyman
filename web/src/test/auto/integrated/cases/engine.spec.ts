import { assert } from 'chai';

import {
  loadKeyboardFromJSON,
  runKeyboardTestFromJSON,
  setupKMW,
  teardownKMW
} from "../test_utils.js";
import * as KMWRecorder from '#recorder';

import { type KeymanEngine, type KeyboardInterface } from 'keyman/app/browser';
import { type KeyboardStub } from 'keyman/engine/package-cache';
import { type OSKInputEventSpec } from '#recorder';

/** @type {HTMLInputElement} */
let inputElem;

// The OSK is appended to the body; we need the ability to clear things out without nullifying the OSK.
const host = document.createElement('div');
document.body.appendChild(host);

const baseTimeout = 5000;
const eventTimeout = 500;

describe('Engine - Browser Interactions', function() {
  this.timeout(baseTimeout);

  before(function() {
    return setupKMW(null, baseTimeout);
  });

  beforeEach(function(done) {
    inputElem = document.createElement('input');
    inputElem.id = 'singleton';
    host.appendChild(inputElem);

    window.setTimeout(function() {
      done()
    }, eventTimeout);
  });

  after(function() {
    teardownKMW();
  });

  afterEach(function() {
    host.innerHTML = '';
  });

  describe('RegisterStub', function() {
    it('RegisterStub on same keyboard twice', async function() {
      this.timeout(baseTimeout);
      const keyman: KeymanEngine = window['keyman'];
      const KeymanWeb: KeyboardInterface = window['KeymanWeb'];

      await loadKeyboardFromJSON("resources/json/keyboards/lao_2008_basic.json", baseTimeout);

      assert.isNotNull(keyman.getKeyboard("lao_2008_basic", "lo"), "Keyboard stub was not registered!");
      assert.equal(keyman.getActiveKeyboard(), "Keyboard_lao_2008_basic", "Keyboard not set automatically!");

      let stub = {
        'KI': 'Keyboard_lao_2008_basic',
        'KN': 'Lao 2008 Basic',
        'KLC': 'lo',
        'KL': 'Lao',
        'KF': 'resources/keyboards/lao_2008_basic.js'
      } as KeyboardStub;

      await Promise.resolve();

      assert.equal(KeymanWeb.registerStub(stub), 1, "Registering existing keyboard should return 1!");
    });
  });

  describe('Variable Stores', function() {
    this.timeout(baseTimeout + baseTimeout);

    beforeEach(function() {
      return loadKeyboardFromJSON("resources/json/keyboards/options_with_save.json", baseTimeout);
    });

    after(function() {
      const keyman: KeymanEngine = window['keyman'];
      keyman.removeKeyboards('options_with_save');
    });

    it('Backing up and restoring (loadStore/saveStore)', function() {
      const keyman: KeymanEngine = window['keyman'];
      const KeymanWeb: KeyboardInterface = window['KeymanWeb'];

      // Keyboard's default value is 0, corresponding to "no foo."
      var keyboardID = "options_with_save";
      var prefixedKeyboardID = "Keyboard_" + keyboardID;
      var storeName = "foo";

      return keyman.setActiveKeyboard(keyboardID, 'en').then(function() {
        // Alas, saveStore itself requires the keyboard to be active!
        KeymanWeb.saveStore(storeName, '1');

        // First, ensure that we get the same thing if we load the value immediately.
        var value = KeymanWeb.loadStore(prefixedKeyboardID, storeName, '0');
        assert.equal(value, '1', "loadStore did not see the value saved to initialize the test before resetting keyboard");

        // Reload the keyboard so that we can test its loaded value.
        keyman.removeKeyboards(keyboardID);

        return Promise.resolve();
      }).then(() => {
        return loadKeyboardFromJSON("resources/json/keyboards/options_with_save.json", baseTimeout);
      }).then(() => {
        return keyman.setActiveKeyboard(keyboardID, 'en');
      }).then(() => {
        // This requires proper storage to a cookie, as we'll be on a new instance of the same keyboard.
        var value = KeymanWeb.loadStore(prefixedKeyboardID, storeName, '0');
        assert.equal(value, '1', "Did not properly save and reload variable store setting");
      }).finally(() => {
        KeymanWeb.saveStore(storeName, '0');
      });
    });

    it("Multiple-sequence check", function() {
      this.timeout(baseTimeout + baseTimeout * 3);
      const keyman: KeymanEngine = window['keyman'];
      const KeymanWeb: KeyboardInterface = window['KeymanWeb'];

      var keyboardID = "options_with_save";
      var storeName = "foo";

      return keyman.setActiveKeyboard(keyboardID, 'en').then(async function() {
        KeymanWeb.saveStore(storeName, '1');
        keyman.removeKeyboards(keyboardID);

        await Promise.resolve();
      }).then(() => {
        // First test:  expects option to be "on" from cookie-init setting, emitting "foo.", then turning option "off".
        return runKeyboardTestFromJSON('resources/json/engine_tests/options_with_save_1.json',
                                      {usingOSK: false},
                                      assert.equal,
                                      baseTimeout)
      }).then(async () => {
        // Reset the keyboard... again.
        keyman.removeKeyboards(keyboardID);

        await Promise.resolve();

        // Second test:  expects option to still be "off" b/c cookies.
        return runKeyboardTestFromJSON('resources/json/engine_tests/options_with_save_2.json',
                                      {usingOSK: false},
                                      assert.equal,
                                      baseTimeout);
      });
    });
  });

  // Performs basic processing system checks/tests to ensure the sequence testing
  // is based on correct assumptions about the code.
  describe('Integrated Simulation Checks', function() {
    this.timeout(baseTimeout);

    before(function() {
      this.timeout(baseTimeout);
      return loadKeyboardFromJSON("resources/json/keyboards/lao_2008_basic.json", baseTimeout);
    });

    beforeEach(function() {
      const keyman: KeymanEngine = window['keyman'];
      keyman.setActiveElement(inputElem);
      inputElem.value = "";
    });

    after(function() {
      const keyman: KeymanEngine = window['keyman'];
      keyman.removeKeyboards('lao_2008_basic');
    });

    /**
     * This can be surprisingly useful for catching browser-specific oddities related to keystroke
     * and keyboard-rule processing!  Keep this test cross-browser at all costs if possible!
     *
     * See #8830.
     */
    it('Simple Keypress', function() {
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
      var lao_s_osk_json = {"type": "osk", "keyID": 'shift-K_S'};
      var lao_s_event = new KMWRecorder.OSKInputEventSpec(lao_s_osk_json as OSKInputEventSpec);

      let eventDriver = new KMWRecorder.BrowserDriver(inputElem);
      await eventDriver.simulateEvent(lao_s_event);

      if(inputElem['base']) {
        inputElem = inputElem['base'];
      }
      assert.equal(inputElem.value, ";");
    });
  });

  describe('Sequence Simulation Checks', function() {
    this.timeout(baseTimeout);

    it('Keyboard simulation', async function() {
      return await runKeyboardTestFromJSON('resources/json/engine_tests/basic_lao_simulation.json', {usingOSK: false}, assert.equal, baseTimeout);
    });

    it('OSK simulation', async function() {
      return await runKeyboardTestFromJSON('resources/json/engine_tests/basic_lao_simulation.json', {usingOSK: true}, assert.equal, baseTimeout);
    })
  });
});

describe('Unmatched Final Groups', function() {
  this.timeout(baseTimeout);

  before(function() {
    return setupKMW(null, baseTimeout + eventTimeout);
  });

  beforeEach(function(done) {
    // Do NOT use an <input> here, as that has special return-char handling that
    // complicates matters.
    const textArea = document.createElement('textarea');
    textArea.id = 'singleton';
    host.appendChild(textArea);

    window.setTimeout(function() {
      done()
    }, eventTimeout);
  });

  after(function() {
    teardownKMW();
  });

  afterEach(function() {
    host.innerHTML = '';
  });

  it('matches rule from early group AND performs default behavior', async function() {
    // While a TAB-oriented version would be nice, it's much harder to write the test
    // to detect change in last input element.
    return await runKeyboardTestFromJSON('resources/json/engine_tests/ghp_enter.json', {usingOSK: true}, assert.equal, baseTimeout);
  });
});

// Kept separate to maintain an extra-clean setup for this test.
describe('Engine - Browser Interactions', function() {
  this.timeout(baseTimeout);

  beforeEach(function() {
    inputElem = document.createElement('input');
    inputElem.id = 'singleton';
    host.appendChild(inputElem);

    return setupKMW(null, baseTimeout);
  });

  afterEach(function() {
    host.innerHTML = '';
    teardownKMW();
  });

  describe('Keyboard Loading', function() {
    it('Local', function() {
      this.timeout(baseTimeout);
      const keyman: KeymanEngine = window['keyman'];
      const KeymanWeb: KeyboardInterface = window['KeymanWeb'];

      return loadKeyboardFromJSON("resources/json/keyboards/lao_2008_basic.json",
                                  baseTimeout).then(async function() {
        assert.isNotNull(keyman.getKeyboard("lao_2008_basic", "lo"), "Keyboard stub was not registered!");
        assert.equal(keyman.getActiveKeyboard(), "Keyboard_lao_2008_basic", "Keyboard not set automatically!");
        keyman.removeKeyboards('lao_2008_basic');

        await Promise.resolve();
        assert.equal(keyman.getActiveKeyboard(), '', "Keyboard not removed correctly!");
      });
    });

    it('Automatically sets first available keyboard', function() {
      this.timeout(2 * baseTimeout);
      const keyman: KeymanEngine = window['keyman'];
      const KeymanWeb: KeyboardInterface = window['KeymanWeb'];

      return loadKeyboardFromJSON("resources/json/keyboards/lao_2008_basic.json",
                                  baseTimeout,
                                  {passive: true}).then(() => {
        // Because we're loading the keyboard 'passively', KMW's setActiveKeyboard function is auto-called
        // on the stub-add.  That specific call (for first keyboard auto-activation) is outside of KMW's
        // current Promise chain, so we can't _directly_ rely on a KMW Promise to test it.
        return new Promise<void>((resolve) => {
          let hasResolved = false;
          // So, we give KMW the time needed for auto-activation to happen, polling a bit actively so that we don't
          // wait unnecessarily long after it occurs.
          let absoluteTimer = window.setTimeout(() => {
            if(!hasResolved) {
              resolve();
              hasResolved = true;
            }

            window.clearTimeout(intervalTimer);
          }, baseTimeout);

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