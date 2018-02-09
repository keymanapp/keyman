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
      inputElem.focus();

      // Yep, not KeyboardEvent.  "keyCode" is nasty-bugged in Chrome and unusable if initializing through KeyboardEvent.
      var event;
      if(typeof Event == 'function') {
        event = new Event("keydown", {"key":"s", "code":"KeyS", "keyCode":83, "which":83});
        event.keyCode = 83;
        event.getModifierState = function() {
          return false;
        };
      } else { // Yeah, so IE can't use the above at all, and requires its own trick.
        event = document.createEvent("KeyboardEvent");
        event.initKeyboardEvent("keydown", false, true, null, String.fromCharCode(83), 0, 0, "", 0);
      }
      inputElem.dispatchEvent(event);

      assert.equal(inputElem.value, "ຫ");
    });

    it('Simple OSK click', function(done) {
      var inputElem = document.getElementById('singleton');

      /* We hack KMW a little bit because the .focus method is insufficient;
       * it won't trigger if the tested browser doesn't have focus.
       * Only one can have focus when testing locally.
       */
      DOMEventHandlers.states.lastActiveElement = inputElem;

      // Let the focus() method do its thing.
      window.setTimeout(function() {
        var osk_S = document.getElementById('default-K_S');

        // Yep, not KeyboardEvent.  "keyCode" is nasty-bugged in Chrome and unusable if initializing through KeyboardEvent.
        var downEvent;
        var upEvent;
        if(typeof Event == 'function') {
          downEvent = new Event("mousedown", {"relatedTarget": osk_S});
          upEvent = new Event("mouseup", {"relatedTarget": osk_S});
        } else { // Yeah, so IE can't use the above at all, and requires its own trick.
          downEvent = document.createEvent("MouseEvent");
          downEvent.initMouseEvent("mousedown", false, true, null,
            null, 0, 0, 0, 0,
            false, false, false, false,
            0, osk_S);

          upEvent = document.createEvent("MouseEvent");
          upEvent.initMouseEvent("mouseup", false, true, null,
            null, 0, 0, 0, 0,
            false, false, false, false,
            0, osk_S);
        }
        osk_S.dispatchEvent(downEvent);
        osk_S.dispatchEvent(upEvent);

        assert.equal(inputElem.value, "ຫ");
        done();
      }, 25);
    });
  })
});