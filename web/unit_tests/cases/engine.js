var assert = chai.assert;

describe('Engine', function() {

  before(function(done) {
    this.timeout(10000);

    fixture.setBase('unit_tests/fixtures');
    setupKMW(null, done, 10000);
  });

  beforeEach(function(done) {
    fixture.load("singleInput.html");
    
    window.setTimeout(function() {
      done()
    }, 50);
  });
  
  after(function() {
    teardownKMW();
  });

  afterEach(function() {
    fixture.cleanup();
  });
  
  describe('Keyboard Loading', function() {
    it('Local', function(done) {
      this.timeout(10000);

      var test_callback = function() {
        assert.equal(keyman.getActiveKeyboard(), "Keyboard_lao_2008_basic", "Keyboard not set correctly!");
        keyman.removeKeyboards('lao_2008_basic');
        assert.equal(keyman.getActiveKeyboard(), '', "Keyboard not removed correctly!");
        done();
      }

      loadKeyboardFromJSON("/keyboards/lao_2008_basic.json", test_callback, 10000);
    });
  });

  // Performs basic processing system checks/tests to ensure the sequence testing
  // is based on correct assumptions about the code.
  describe('Processing', function() {
    before(function(done){
      this.timeout = 10000;
      loadKeyboardFromJSON("/keyboards/lao_2008_basic.json", done, 10000);
    });

    beforeEach(function() {
      var inputElem = document.getElementById('singleton');
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
      var lao_s_event = new KMWRecorder.PhysicalInputEvent(lao_s_key_json);

      lao_s_event.simulateEventOn(inputElem);

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
      var lao_s_event = new KMWRecorder.OSKInputEvent(lao_s_osk_json);

      lao_s_event.simulateEventOn(inputElem);

      if(inputElem['base']) {
        inputElem = inputElem['base'];
      }
      assert.equal(inputElem.value, ";");
    });
    // TODO:  add a 'resetContext' test!
  })

  describe('Sequence Testing', function() {
    before(function(done){
      this.timeout = 10000;
      loadKeyboardFromJSON("/keyboards/lao_2008_basic.json", done, 10000);
    });

    after(function() {
      keyman.removeKeyboards('lao_2008_basic');
      fixture.cleanup();
    });

    it('Keyboard simulation', function() {
      var inputElem = document.getElementById('singleton');
      if(inputElem['kmw_ip']) {
        inputElem = inputElem['kmw_ip'];
      }

      var lao_s_key_json = {
        "inputs": [{"type":"key", "key":"s", "code":"KeyS","keyCode":83,"modifierSet":0,"location":0}],
        "output": "ຫ"
      };
      var lao_s_test = new KMWRecorder.InputTestSequence(lao_s_key_json);
      lao_s_test.simulateSequenceOn(inputElem, assert.equal);
    });

    it('OSK simulation', function() {
      var inputElem = document.getElementById('singleton');
      if(inputElem['kmw_ip']) {
        inputElem = inputElem['kmw_ip'];
      }

      var lao_s_osk_json = {
        "inputs": [{"type":"osk", "keyID": 'shift-K_S'}],
        "output": ";"
      };
      var lao_s_test = new KMWRecorder.InputTestSequence(lao_s_osk_json);
      lao_s_test.simulateSequenceOn(inputElem, assert.equal);
    });
  })
});