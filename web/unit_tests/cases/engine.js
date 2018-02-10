var assert = chai.assert;

describe('Engine', function() {

  before(function(done) {
    this.timeout(10000);

    fixture.setBase('unit_tests/fixtures');
    setupKMW();

    // Pass the initTimer method our 'done' callback so it can handle our initialization delays for us.
    initTimer(done);
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
      var laoStub = fixture.load("/keyboards/lao_2008_basic.json", true);

      keyman.addKeyboards(laoStub);
      keyman.setActiveKeyboard("Keyboard_lao_2008_basic", "lao");

      window.setTimeout(function() {
        assert.equal(keyman.getActiveKeyboard(), "Keyboard_lao_2008_basic");

        keyman.removeKeyboards('lao_2008_basic');
        done();
      }, 500);
    });
  });

  describe('Processing', function() {
    before(function(done){
      var laoStub = fixture.load("/keyboards/lao_2008_basic.json", true);

      keyman.addKeyboards(laoStub);
      keyman.setActiveKeyboard("Keyboard_lao_2008_basic", "lao");

      window.setTimeout(function() {
        done();
      }, 500);
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

      var lao_s_key_json = {"key":"s", "code":"KeyS","keyCode":83,"modifierSet":0,"location":0};
      var lao_s_event = new KMWRecorder.PhysicalInputEvent(lao_s_key_json);

      lao_s_event.simulateEvent(inputElem);

      assert.equal(inputElem.value, "ຫ");
    });

    it('Simple OSK click', function() {
      var inputElem = document.getElementById('singleton');

      var lao_s_osk_json = {"keyID": 'shift-K_S'};
      var lao_s_event = new KMWRecorder.OSKInputEvent(lao_s_osk_json);

      lao_s_event.simulateEvent(inputElem);

      assert.equal(inputElem.value, ";");
    });
  })
});