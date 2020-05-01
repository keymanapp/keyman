var assert = chai.assert;

describe('Attachment API', function() {
  this.timeout(kmwconfig.timeouts.standard);

  before(function(done) {
    assert.isFalse(com.keyman.karma.DEVICE_DETECT_FAILURE, "Cannot run due to device detection failure.");
    fixture.setBase('fixtures');

    this.timeout(kmwconfig.timeouts.scriptLoad * 3);
    setupKMW({ attachType:'manual' }, function() {
      loadKeyboardFromJSON("/keyboards/lao_2008_basic.json", function() {
        // Sequential so we don't have to worry about race conditions and such
        // to signal completion with done().

        loadKeyboardFromJSON("/keyboards/khmer_angkor.json", function() {
          keyman.setActiveKeyboard("lao_2008_basic");
          done();
        }, kmwconfig.timeouts.scriptLoad);
      }, kmwconfig.timeouts.scriptLoad);
    }, kmwconfig.timeouts.scriptLoad);
  });

  after(function() {
    keyman.removeKeyboards('lao_2008_basic');
    teardownKMW();
  });

  beforeEach(function() {
    fixture.load("robustAttachment.html");
  });
  
  afterEach(function(done) {
    fixture.cleanup();
    window.setTimeout(function(){
      done();
    }, kmwconfig.timeouts.eventDelay);
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

      // A keystroke must target the input-receiving element.  For touch, that's the alias.
      eventDriver = new KMWRecorder.BrowserDriver(ele['kmw_ip'] ? ele['kmw_ip'] : ele);
      eventDriver.simulateEvent(DynamicElements.keyCommand);

      val = retrieveAndReset(ele);

      assert.equal(val, DynamicElements.enabledLaoOutput, "'Attached' element did not perform keystroke processing!");

      done();
    }, kmwconfig.timeouts.eventDelay);
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
        let eventDriver = new KMWRecorder.BrowserDriver(ele['kmw_ip'] ? ele['kmw_ip'] : ele);
        eventDriver.simulateEvent(DynamicElements.keyCommand);
        val = retrieveAndReset(ele);  
        assert.equal(val, DynamicElements.disabledOutput, "'Disabled' element performed keystroke processing!");

        keyman.enableControl(ele);
        window.setTimeout(function() {
          DynamicElements.assertAttached(ele); // Happens in-line, since we directly request the attachment.
          let eventDriver = new KMWRecorder.BrowserDriver(ele['kmw_ip'] ? ele['kmw_ip'] : ele);
          eventDriver.simulateEvent(DynamicElements.keyCommand);
          val = retrieveAndReset(ele);
          assert.equal(val, DynamicElements.enabledLaoOutput, "'Enabled' element did not perform keystroke processing!");
          done();
        }, kmwconfig.timeouts.eventDelay);
      }, kmwconfig.timeouts.eventDelay);
    }, kmwconfig.timeouts.eventDelay);
  });

  it("Keyboard Management (active control)", function() {
    // It appears that event generation + inline event dispatching is a bit time-intensive on some browsers.
    this.timeout(kmwconfig.timeouts.standard * 2);

    var input = document.getElementById(DynamicElements.addInput());
    var textarea = document.getElementById(DynamicElements.addText());

    keyman.attachToControl(input);
    keyman.attachToControl(textarea);

    keyman.setActiveElement(input);
    // We assume from the other tests that running on the Lao keyboard will give proper output.
    // It'd be a redundant check.

    // Set control with independent keyboard.
    keyman.setKeyboardForControl(input, "khmer_angkor", "km");
    var eventDriver = new KMWRecorder.BrowserDriver(input['kmw_ip'] ? input['kmw_ip'] : input);
    eventDriver.simulateEvent(DynamicElements.keyCommand);
    val = retrieveAndReset(input);
    assert.equal(val, DynamicElements.enabledKhmerOutput, "KMW did not use control's keyboard settings!");

    // Swap to a global-linked control...
    keyman.setActiveElement(textarea);
    eventDriver = new KMWRecorder.BrowserDriver(textarea['kmw_ip'] ? textarea['kmw_ip'] : textarea);
    eventDriver.simulateEvent(DynamicElements.keyCommand);
    val = retrieveAndReset(textarea);
    assert.equal(val, DynamicElements.enabledLaoOutput, "KMW did not use manage keyboard settings correctly for global-linked control!");

    // Swap back and check that the settings persist.
    keyman.setActiveElement(input);
    eventDriver = new KMWRecorder.BrowserDriver(input['kmw_ip'] ? input['kmw_ip'] : input);
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
    this.timeout(kmwconfig.timeouts.standard * 2);

    var input = document.getElementById(DynamicElements.addInput());
    var textarea = document.getElementById(DynamicElements.addText());

    keyman.attachToControl(input);
    keyman.attachToControl(textarea);

    keyman.setActiveElement(input);
    // We assume from the other tests that running on the Lao keyboard will give proper output.
    // It'd be a redundant check.

    // Set control with independent keyboard.
    keyman.setKeyboardForControl(textarea, "khmer_angkor", "km");
    var eventDriver = new KMWRecorder.BrowserDriver(input['kmw_ip'] ? input['kmw_ip'] : input);
    eventDriver.simulateEvent(DynamicElements.keyCommand);
    val = retrieveAndReset(input);
    assert.equal(val, DynamicElements.enabledLaoOutput, "KMW set independent keyboard for the incorrect control!");

    // Swap to a global-linked control...
    keyman.setActiveElement(textarea);
    eventDriver = new KMWRecorder.BrowserDriver(textarea['kmw_ip'] ? textarea['kmw_ip'] : textarea);
    eventDriver.simulateEvent(DynamicElements.keyCommand);
    val = retrieveAndReset(textarea);
    assert.equal(val, DynamicElements.enabledKhmerOutput, "KMW did not properly store keyboard for the previously-inactive control!");

    // Swap back and check that the settings persist.
    keyman.setActiveElement(input);
    keyman.setKeyboardForControl(textarea, null, null);

    eventDriver = new KMWRecorder.BrowserDriver(input['kmw_ip'] ? input['kmw_ip'] : input);
    eventDriver.simulateEvent(DynamicElements.keyCommand);
    val = retrieveAndReset(input);
    assert.equal(val, DynamicElements.enabledLaoOutput, "KMW made a strange error when clearing an inactive control's keyboard setting!");

    keyman.setActiveElement(textarea);
    // Finally, clear the independent setting.
    eventDriver.simulateEvent(DynamicElements.keyCommand);
    val = retrieveAndReset(input);
    assert.equal(val, DynamicElements.enabledLaoOutput, "KMW did not properly clear control's independent keyboard settings!");
  });
});

Modernizr.on('touchevents', function(result) {
  if(result) {
    describe('Device-specific Attachment Checks (Touch, \'auto\')', function() {

      this.timeout(kmwconfig.timeouts.standard);

      before(function(done) {
        this.timeout(kmwconfig.timeouts.scriptLoad);

        fixture.setBase('fixtures');
        setupKMW({ attachType:'auto' }, done, kmwconfig.timeouts.scriptLoad);
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
        }, kmwconfig.timeouts.eventDelay);
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
          this.timeout(kmwconfig.timeouts.scriptLoad * 2);  // Just in case, for iframe loading time.
          
          var ID = DynamicElements.addIFrame(function() {
            var ele = document.getElementById(ID);
            var innerEle = ele.contentDocument.getElementById('iframe_input');

            assert.isFalse(keyman.isAttached(ele));
            assert.isNotNull(innerEle);
            assert.isFalse(keyman.isAttached(innerEle));

            window.setTimeout(function() {
              done();
            }, kmwconfig.timeouts.eventDelay);
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

      this.timeout(kmwconfig.timeouts.standard);

      before(function(done) {
        this.timeout(kmwconfig.timeouts.scriptLoad);

        fixture.setBase('fixtures');
        setupKMW({ attachType:'auto' }, done, kmwconfig.timeouts.scriptLoad);
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
        }, kmwconfig.timeouts.eventDelay);
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
          this.timeout(kmwconfig.timeouts.scriptLoad * 2);  // Just in case, for iframe loading time.

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
            }, kmwconfig.timeouts.eventDelay);
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