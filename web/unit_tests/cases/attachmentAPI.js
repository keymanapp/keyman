var assert = chai.assert;

describe('Attachment API', function() {
  this.timeout(testconfig.timeouts.standard);

  before(function() {
    assert.isFalse(com.keyman.karma.DEVICE_DETECT_FAILURE, "Cannot run due to device detection failure.");
    fixture.setBase('fixtures');

    this.timeout(testconfig.timeouts.scriptLoad * 3);
    return setupKMW({ attachType:'manual' }, testconfig.timeouts.scriptLoad).then(() => {
      const kbd1 = loadKeyboardFromJSON("/keyboards/lao_2008_basic.json", testconfig.timeouts.scriptLoad, { passive: true });
      const kbd2 = loadKeyboardFromJSON("/keyboards/khmer_angkor.json",   testconfig.timeouts.scriptLoad, { passive: true });
      return Promise.all([kbd1, kbd2]).then(() => {
        return keyman.setActiveKeyboard("lao_2008_basic");
      });
    });
  });

  after(function() {
    keyman.removeKeyboards('lao_2008_basic');
    keyman.removeKeyboards('khmer_angkor');
    teardownKMW();
  });

  beforeEach(function() {
    fixture.load("robustAttachment.html");
  });

  afterEach(function(done) {
    fixture.cleanup();
    window.setTimeout(function(){
      done();
    }, testconfig.timeouts.eventDelay);
  });

  it("Attachment/Detachment", function(done) {
    // Since we're in 'manual', we start detached.
    var ele = document.getElementById(DynamicElements.addInput());

    window.setTimeout(function() {
      // Ensure we didn't auto-attach.
      DynamicElements.assertDetached(ele);
      let eventDriver = new KMWRecorder.BrowserDriver(ele);
      eventDriver.simulateEvent(DynamicElements.keyCommand);

      var val = ele.value;
      ele.value = "";
      assert.equal(val, DynamicElements.disabledOutput, "'Detached' element performed keystroke processing!");

      keyman.attachToControl(ele);
      DynamicElements.assertAttached(ele); // Happens in-line, since we directly request the attachment.

      eventDriver = new KMWRecorder.BrowserDriver(ele);
      eventDriver.simulateEvent(DynamicElements.keyCommand);

      val = retrieveAndReset(ele);

      assert.equal(val, DynamicElements.enabledLaoOutput, "'Attached' element did not perform keystroke processing!");

      done();
    }, testconfig.timeouts.eventDelay);
  });

  it("Enablement/Disablement", function(done) {
    // Since we're in 'manual', we start detached.
    var ele = document.getElementById(DynamicElements.addInput());
    window.setTimeout(function() {
      keyman.attachToControl(ele);
      keyman.disableControl(ele);

      // It appears that mobile devices do not instantly trigger the MutationObserver, so we need a small timeout
      // for the change to take effect.
      window.setTimeout(function() {
        DynamicElements.assertAttached(ele);
        let eventDriver = new KMWRecorder.BrowserDriver(ele);
        eventDriver.simulateEvent(DynamicElements.keyCommand);
        val = retrieveAndReset(ele);
        assert.equal(val, DynamicElements.disabledOutput, "'Disabled' element performed keystroke processing!");

        keyman.enableControl(ele);
        window.setTimeout(function() {
          DynamicElements.assertAttached(ele); // Happens in-line, since we directly request the attachment.
          let eventDriver = new KMWRecorder.BrowserDriver(ele);
          eventDriver.simulateEvent(DynamicElements.keyCommand);
          val = retrieveAndReset(ele);
          assert.equal(val, DynamicElements.enabledLaoOutput, "'Enabled' element did not perform keystroke processing!");
          done();
        }, testconfig.timeouts.eventDelay);
      }, testconfig.timeouts.eventDelay);
    }, testconfig.timeouts.eventDelay);
  });

  it("Keyboard Management (active control)", function() {
    // It appears that event generation + inline event dispatching is a bit time-intensive on some browsers.
    this.timeout(testconfig.timeouts.standard * 2);

    var input = document.getElementById(DynamicElements.addInput());
    var textarea = document.getElementById(DynamicElements.addText());

    keyman.attachToControl(input);
    keyman.attachToControl(textarea);

    keyman.setActiveElement(input);
    // We assume from the other tests that running on the Lao keyboard will give proper output.
    // It'd be a redundant check.

    // Set control with independent keyboard.
    keyman.setKeyboardForControl(input, "khmer_angkor", "km");
    var eventDriver = new KMWRecorder.BrowserDriver(input);
    eventDriver.simulateEvent(DynamicElements.keyCommand);
    val = retrieveAndReset(input);
    assert.equal(val, DynamicElements.enabledKhmerOutput, "KMW did not use control's keyboard settings!");

    // Swap to a global-linked control...
    keyman.setActiveElement(textarea);
    eventDriver = new KMWRecorder.BrowserDriver(textarea);
    eventDriver.simulateEvent(DynamicElements.keyCommand);
    val = retrieveAndReset(textarea);
    assert.equal(val, DynamicElements.enabledLaoOutput, "KMW did not use manage keyboard settings correctly for global-linked control!");

    // Swap back and check that the settings persist.
    keyman.setActiveElement(input);
    eventDriver = new KMWRecorder.BrowserDriver(input);
    eventDriver.simulateEvent(DynamicElements.keyCommand);
    val = retrieveAndReset(input);
    assert.equal(val, DynamicElements.enabledKhmerOutput, "KMW forgot control's independent keyboard settings!");

    // Finally, clear the independent setting.
    keyman.setKeyboardForControl(input, null, null);
    eventDriver.simulateEvent(DynamicElements.keyCommand);
    val = retrieveAndReset(input);
    assert.equal(val, DynamicElements.enabledLaoOutput, "KMW did not properly clear control's independent keyboard settings!");
  });

  it("Keyboard Management (inactive control)", function() {
    // It appears that event generation + inline event dispatching is a bit time-intensive on some browsers.
    this.timeout(testconfig.timeouts.standard * 2);

    var input = document.getElementById(DynamicElements.addInput());
    var textarea = document.getElementById(DynamicElements.addText());

    keyman.attachToControl(input);
    keyman.attachToControl(textarea);

    // We assume from the other tests that running on the Lao keyboard will give proper output.
    // It'd be a redundant check.

    // We are testing that setting a specific keyboard for an inactive control does not affect
    // the currently active control. The textarea control will be manually set to Khmer,
    // and the input control will get the document default of Lao.

    keyman.setActiveElement(input);
    // Set textarea control with independent keyboard khmer_angkor.
    keyman.setKeyboardForControl(textarea, "khmer_angkor", "km");
    var eventDriver = new KMWRecorder.BrowserDriver(input);
    eventDriver.simulateEvent(DynamicElements.keyCommand);
    val = retrieveAndReset(input);
    assert.equal(val, DynamicElements.enabledLaoOutput, "KMW set independent keyboard for the incorrect control!");

    // Swap to the textarea control with its overridden khmer_angkor keyboard...
    keyman.setActiveElement(textarea);
    eventDriver = new KMWRecorder.BrowserDriver(textarea);
    eventDriver.simulateEvent(DynamicElements.keyCommand);
    val = retrieveAndReset(textarea);
    assert.equal(val, DynamicElements.enabledKhmerOutput, "KMW did not properly store keyboard for the previously-inactive control!");

    // Swap back to the input control and check that the settings persist.
    keyman.setActiveElement(input);
    keyman.setKeyboardForControl(textarea, null, null);

    eventDriver = new KMWRecorder.BrowserDriver(input);
    eventDriver.simulateEvent(DynamicElements.keyCommand);
    val = retrieveAndReset(input);
    assert.equal(val, DynamicElements.enabledLaoOutput, "KMW made a strange error when clearing an inactive control's keyboard setting!");

    keyman.setActiveElement(textarea);
    // Finally, after clearing the independent setting, check that we are back to Lao output as expected for the textarea
    eventDriver = new KMWRecorder.BrowserDriver(textarea);
    eventDriver.simulateEvent(DynamicElements.keyCommand);
    val = retrieveAndReset(textarea);
    assert.equal(val, DynamicElements.enabledLaoOutput, "KMW did not properly clear control's independent keyboard settings!");
  });
});

Modernizr.on('touchevents', function(result) {
  if(result) {
    describe('Device-specific Attachment Checks (Touch, \'auto\')', function() {

      this.timeout(testconfig.timeouts.standard);

      before(function() {
        this.timeout(testconfig.timeouts.scriptLoad);

        fixture.setBase('fixtures');
        return setupKMW({ attachType:'auto' }, testconfig.timeouts.scriptLoad);
      });

      beforeEach(function() {
        fixture.load("robustAttachment.html");
      });

      after(function() {
        teardownKMW();
      });

      afterEach(function(done) {
        fixture.cleanup();
        window.setTimeout(function(){
          done();
        }, testconfig.timeouts.eventDelay);
      });

      describe('Element Type', function() {
        it('<input>', function(done) {
          var ID = DynamicElements.addInput();
          var ele = document.getElementById(ID);

          DynamicElements.assertAttached(ele, done);
        });

        it('<textarea>', function(done) {
          var ID = DynamicElements.addText();
          var ele = document.getElementById(ID);

          DynamicElements.assertAttached(ele, done);
        });

        it.skip('<iframe>', function(done) {
          this.timeout(testconfig.timeouts.scriptLoad * 2);  // Just in case, for iframe loading time.

          var ID = DynamicElements.addIFrame(function() {
            var ele = document.getElementById(ID);
            var innerEle = ele.contentDocument.getElementById('iframe_input');

            assert.isFalse(keyman.isAttached(ele));
            assert.isNotNull(innerEle);
            assert.isFalse(keyman.isAttached(innerEle));

            window.setTimeout(function() {
              done();
            }, testconfig.timeouts.eventDelay);
          });
        });

        it('contentEditable=true', function(done) {
          var ID = DynamicElements.addEditable();
          var ele = document.getElementById(ID);

          assert.isFalse(keyman.isAttached(ele));
          done();
        });
      });
    });
  } else {
    describe('Device-specific Attachment Checks (Desktop, \'auto\')', function() {

      this.timeout(testconfig.timeouts.standard);

      before(function() {
        this.timeout(testconfig.timeouts.scriptLoad);

        fixture.setBase('fixtures');
        return setupKMW({ attachType:'auto' }, testconfig.timeouts.scriptLoad);
      });

      beforeEach(function() {
        fixture.load("robustAttachment.html");
      });

      after(function() {
        teardownKMW();
      });

      afterEach(function(done) {
        fixture.cleanup();
        window.setTimeout(function(){
          done();
        }, testconfig.timeouts.eventDelay);
      })

      describe('Element Type', function() {
        it('<input>', function(done) {
          var ID = DynamicElements.addInput();
          var ele = document.getElementById(ID);

          DynamicElements.assertAttached(ele, done);
        });

        it('<textarea>', function(done) {
          var ID = DynamicElements.addText();
          var ele = document.getElementById(ID);

          DynamicElements.assertAttached(ele, done);
        });

        it.skip('<iframe>', function(done) {
          this.timeout(testconfig.timeouts.scriptLoad * 2);  // Just in case, for iframe loading time.

          var ID = DynamicElements.addIFrame(function() {
            var ele = document.getElementById(ID);
            var innerEle = ele.contentDocument.getElementById('iframe_input');

            // No need to track data on the iframe itself.
            assert.isFalse(keyman.isAttached(ele));
            assert.isNotNull(innerEle);
            assert.isTrue(keyman.isAttached(innerEle));
            keyman.detachFromControl(ele);

            window.setTimeout(function() {
              done();
            }, testconfig.timeouts.eventDelay);
          });
        });

        it('contentEditable=true', function(done) {
          var ID = DynamicElements.addEditable();
          var ele = document.getElementById(ID);

          DynamicElements.assertAttached(ele, done);
        });
      });
    });

  }
});