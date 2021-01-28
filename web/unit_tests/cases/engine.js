var assert = chai.assert;

describe('Engine - Browser Interactions', function() {
  this.timeout(kmwconfig.timeouts.scriptLoad);

  before(function(done) {
    fixture.setBase('fixtures');
    setupKMW(null, done, kmwconfig.timeouts.scriptLoad);
  });

  beforeEach(function(done) {
    fixture.load("singleInput.html");
    
    window.setTimeout(function() {
      done()
    }, kmwconfig.timeouts.eventDelay);
  });
  
  after(function() {
    teardownKMW();
  });

  afterEach(function() {
    fixture.cleanup();
  });
  
  describe('Keyboard Loading', function() {
    it('Local', function(done) {
      this.timeout(kmwconfig.timeouts.scriptLoad);

      var test_callback = function() {
        assert.isNotNull(keyman.getKeyboard("lao_2008_basic", "lo"), "Keyboard stub was not registered!");
        assert.equal(keyman.getActiveKeyboard(), "Keyboard_lao_2008_basic", "Keyboard not set automatically!");
        keyman.removeKeyboards('lao_2008_basic');
        assert.equal(keyman.getActiveKeyboard(), '', "Keyboard not removed correctly!");
        done();
      }

      loadKeyboardFromJSON("/keyboards/lao_2008_basic.json", test_callback, kmwconfig.timeouts.scriptLoad, {passive: true});
    });
  });

  describe('Variable Stores', function() {
    this.timeout(kmwconfig.timeouts.scriptLoad + kmwconfig.timeouts.standard);

    beforeEach(function(done) {
      loadKeyboardFromJSON("/keyboards/options_with_save.json", done, kmwconfig.timeouts.scriptLoad);
    });

    after(function() {
      keyman.removeKeyboards('options_with_save');
      fixture.cleanup();
    });

    it('Backing up and restoring (loadStore/saveStore)', function(done) {
      // Keyboard's default value is 0, corresponding to "no foo."
      var keyboardID = "options_with_save";
      var prefixedKeyboardID = "Keyboard_" + keyboardID;
      var storeName = "foo";

      keyman.setActiveKeyboard(keyboardID, 'en').then(function() {
        // Alas, saveStore itself requires the keyboard to be active!
        KeymanWeb.saveStore(storeName, 1);

        // First, ensure that we get the same thing if we load the value immediately.
        var value = KeymanWeb.loadStore(prefixedKeyboardID, storeName, 0);
        assert.equal(value, 1, "loadStore did not see the value saved to initialize the test before resetting keyboard");

        // Reload the keyboard so that we can test its loaded value.
        keyman.removeKeyboards(keyboardID, true);

        // Now we can reload the keyboard and run the test.
        var remainderOfTest = function() {
          // This requires proper storage to a cookie, as we'll be on a new instance of the same keyboard.
          var value = KeymanWeb.loadStore(prefixedKeyboardID, storeName, 0);
          assert.equal(value, 1, "Did not properly save and reload variable store setting");

          KeymanWeb.saveStore(storeName, 0);
          

          done();
        }

        loadKeyboardFromJSON("/keyboards/options_with_save.json", function() {
          keyman.setActiveKeyboard(keyboardID, 'en').then(remainderOfTest);
        }, kmwconfig.timeouts.scriptLoad);
      });
    });

    it("Multiple-sequence check", function(done) {
      this.timeout(kmwconfig.timeouts.standard + kmwconfig.timeouts.scriptLoad * 3);
      var keyboardID = "options_with_save";
      var storeName = "foo";

      keyman.setActiveKeyboard(keyboardID, 'en').then(function() {
        KeymanWeb.saveStore(storeName, 1);
        keyman.removeKeyboards(keyboardID, true);

        var finalCheck = function() {
          // Reset the keyboard... again.
          keyman.removeKeyboards(keyboardID, true);

          // Second test:  expects option to still be "off" b/c cookies.
          runKeyboardTestFromJSON('/engine_tests/options_with_save_2.json', {usingOSK: false}, done, assert.equal, kmwconfig.timeouts.scriptLoad);
        };

        // First test:  expects option to be "on" from cookie-init setting, emitting "foo.", then turning option "off".
        runKeyboardTestFromJSON('/engine_tests/options_with_save_1.json', {usingOSK: false}, finalCheck, assert.equal, kmwconfig.timeouts.scriptLoad);
      });
    });
  });

  // Performs basic processing system checks/tests to ensure the sequence testing
  // is based on correct assumptions about the code.
  describe('Integrated Simulation Checks', function() {
    this.timeout(kmwconfig.timeouts.standard);

    before(function(done){
      this.timeout = kmwconfig.timeouts.scriptLoad;
      loadKeyboardFromJSON("/keyboards/lao_2008_basic.json", done, kmwconfig.timeouts.scriptLoad);
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

    it('Simple Keypress', function() {
      var inputElem = document.getElementById('singleton');
      if(inputElem['kmw_ip']) {
        inputElem = inputElem['kmw_ip'];
      }

      var lao_s_key_json = {"type": "key", "key":"s", "code":"KeyS","keyCode":83,"modifierSet":0,"location":0};
      var lao_s_event = new KMWRecorder.PhysicalInputEventSpec(lao_s_key_json);

      let eventDriver = new KMWRecorder.BrowserDriver(inputElem);
      eventDriver.simulateEvent(lao_s_event);

      if(inputElem['base']) {
        inputElem = inputElem['base'];
      }
      assert.equal(inputElem.value, "ຫ");
    });

    it('Simple OSK click', function() {
      var inputElem = document.getElementById('singleton');
      if(inputElem['kmw_ip']) {
        inputElem = inputElem['kmw_ip'];
      }

      var lao_s_osk_json = {"type": "osk", "keyID": 'shift-K_S'};
      var lao_s_event = new KMWRecorder.OSKInputEventSpec(lao_s_osk_json);

      let eventDriver = new KMWRecorder.BrowserDriver(inputElem);
      eventDriver.simulateEvent(lao_s_event);

      if(inputElem['base']) {
        inputElem = inputElem['base'];
      }
      assert.equal(inputElem.value, ";");
    });
  })

  describe('Sequence Simulation Checks', function() {
    this.timeout(kmwconfig.timeouts.scriptLoad);

    it('Keyboard simulation', function(done) {
      runKeyboardTestFromJSON('/engine_tests/basic_lao_simulation.json', {usingOSK: false}, done, assert.equal, kmwconfig.timeouts.scriptLoad);
    });

    it('OSK simulation', function(done) {
      runKeyboardTestFromJSON('/engine_tests/basic_lao_simulation.json', {usingOSK: true}, done, assert.equal, kmwconfig.timeouts.scriptLoad);
    })
  });
});

describe('Unmatched Final Groups', function() {
  this.timeout(kmwconfig.timeouts.scriptLoad);

  before(function(done) {
    fixture.setBase('fixtures');
    setupKMW(null, done, kmwconfig.timeouts.scriptLoad + kmwconfig.timeouts.eventDelay);
  });

  beforeEach(function(done) {
    fixture.load("singleTextArea.html");
    
    window.setTimeout(function() {
      done()
    }, kmwconfig.timeouts.eventDelay);
  });
  
  after(function() {
    teardownKMW();
  });

  afterEach(function() {
    fixture.cleanup();
  });

  it.only('matches rule from early group AND performs default behavior', function(done) {
    // While a TAB-oriented version would be nice, it's much harder to write the test
    // to detect change in last input element.
    runKeyboardTestFromJSON('/engine_tests/ghp_enter.json', {usingOSK: true}, done, assert.equal, kmwconfig.timeouts.scriptLoad);
  });
});