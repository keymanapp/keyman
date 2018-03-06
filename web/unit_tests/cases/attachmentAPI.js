var assert = chai.assert;

var DynamicElements;
var inputCounter = 0;

if(typeof(DynamicElements) == 'undefined') {
  DynamicElements = {};

  DynamicElements.addInput = function() {
    var masterDiv = document.getElementById('DynamicElements');
    var newInput = document.createElement("input");
    var i = inputCounter++;
    
    newInput.id = 'input' + i;
    newInput.className = 'test';
    newInput.placeholder = "Dynamic area #" + i + "!";
    
    masterDiv.appendChild(newInput);
    return newInput.id;
  }
  
  DynamicElements.addText = function () {
    var masterDiv = document.getElementById('DynamicElements');
    var newTextArea = document.createElement("textarea");
    var i = inputCounter++;
    
    newTextArea.id = 'textarea' + i;
    newTextArea.className = 'test';
    newTextArea.placeholder = "Dynamic area #" + i + "!";
    
    masterDiv.appendChild(newTextArea);
    return newTextArea.id;
  }
  
  DynamicElements.addIFrame = function(loadCallback) {
    var masterDiv = document.getElementById('DynamicElements');
    var frame = document.createElement("iframe");
    var i = inputCounter++;
    
    frame.height = "100";
    frame.id = 'iframe' + i;
    if(loadCallback) {
      frame.addEventListener('load', function() {
        // Give KMW's attachment events a chance to run first.
        window.setTimeout(loadCallback, 100);
      });
    }
    frame.setAttribute("src", "resources/html/iframe.html");
      
    masterDiv.appendChild(frame);
    return frame.id;
  }
  
  DynamicElements.addEditable = function() {
    var masterDiv = document.getElementById('DynamicElements');
    var editable = document.createElement("div");
    var i = inputCounter++;
    
    editable.contentEditable = true;
    editable.textContent = "Edit me!";
    editable.id = 'editable' + i;
    editable.style.width="500px";
    
    masterDiv.appendChild(editable);
    return editable.id;
  }

  DynamicElements.assertAttached = function(ele, done) {
    var assertion = function() {
      assert.isTrue(keyman.isAttached(ele), "Element tag '" + ele.tagName + "', id '" + ele.id + "' was not attached!");
    }
    if(done) {
      window.setTimeout(function() {
        assertion();
        done();
      }, 50);
    } else {
      assertion();
    }
  }

  DynamicElements.assertDetached = function(ele, done) {
    var assertion = function() {
      assert.isFalse(keyman.isAttached(ele), "Element tag '" + ele.tagName + "', id '" + ele.id + "' was not detached!");
    }
    if(done) {
      window.setTimeout(function() {
        assertion();
        done();
      }, 50);
    } else {
      assertion();
    }
  }

  DynamicElements.init = function() {
    var lao_s_key_json = {"type": "key", "key":"s", "code":"KeyS","keyCode":83,"modifierSet":0,"location":0};
    DynamicElements.keyCommand = new KMWRecorder.PhysicalInputEvent(lao_s_key_json);

    DynamicElements.enabledOutput = "ຫ";
    // Simulated JavaScript events do not produce text output.
    DynamicElements.disabledOutput = "";
  }

  DynamicElements.init();
}

describe('Attachment API', function() {
  this.timeout(5000);

  before(function() {
    fixture.setBase('unit_tests/fixtures');
  });

  describe("Enablement/Disablement", function() {
    before(function(done) {
      this.timeout(20000);
      setupKMW({ attachType:'manual' }, function() {
        loadKeyboardFromJSON("/keyboards/lao_2008_basic.json", done, 10000);
        
        // At present, keyboard settings are managed/saved on blur/focus events.
        // Since we can't rely on those automatically happening in automated testing,
        // we force-set the values here for now.
        keyman.globalKeyboard = "Keyboard_lao_2008_basic";
        keyman.globalLanguageCode = "lo";
      }, 10000);
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
      }, 50);
    });

    it("Attach/Detach", function(done) {
      // Since we're in 'manual', we start detached.
      var ele = document.getElementById(DynamicElements.addInput());
      window.setTimeout(function() {

        // Ensure we didn't auto-attach.
        DynamicElements.assertDetached(ele);
        DynamicElements.keyCommand.simulateEventOn(ele);

        var val = ele.value;
        ele.value = "";
        assert.equal(val, DynamicElements.disabledOutput, "'Detached' element performed keystroke processing!");

        keyman.attachToControl(ele);
        DynamicElements.assertAttached(ele); // Happens in-line, since we directly request the attachment.

        // A keystroke must target the input-receiving element.  For touch, that's the alias.
        DynamicElements.keyCommand.simulateEventOn(ele['kmw_ip'] ? ele['kmw_ip'] : ele);

        val = retrieveAndReset(ele);

        assert.equal(val, DynamicElements.enabledOutput, "'Attached' element did not perform keystroke processing!");

        done();
      }, 5);
    });

    it("Enable/Disable", function(done) {
      // Since we're in 'manual', we start detached.
      var ele = document.getElementById(DynamicElements.addInput());
      window.setTimeout(function() {
        // Ensure we didn't auto-attach.
        keyman.attachToControl(ele);
        // Disablement uses MutationObservers to function properly, so we need a minor timeout.
        keyman.disableControl(ele);
        window.setTimeout(function() {
          DynamicElements.assertAttached(ele);
          DynamicElements.keyCommand.simulateEventOn(ele['kmw_ip'] ? ele['kmw_ip'] : ele);
          val = retrieveAndReset(ele);  
          assert.equal(val, DynamicElements.disabledOutput, "'Disabled' element performed keystroke processing!");
  
          keyman.enableControl(ele);
          window.setTimeout(function() {
            DynamicElements.assertAttached(ele); // Happens in-line, since we directly request the attachment.
            DynamicElements.keyCommand.simulateEventOn(ele['kmw_ip'] ? ele['kmw_ip'] : ele);
            val = retrieveAndReset(ele);
            assert.equal(val, DynamicElements.enabledOutput, "'Enabled' element did not perform keystroke processing!");
    
            done();
          }, 5);
        }, 5);
      }, 5);
    });
  });
});

Modernizr.on('touchevents', function(result) {
  if(result) {
    describe('Device-specific Attachment Checks (Touch, \'auto\')', function() {

      this.timeout(5000);

      before(function(done) {
        this.timeout(10000);

        fixture.setBase('unit_tests/fixtures');
        setupKMW({ attachType:'auto' }, done, 10000);
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
        }, 500);
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

        it('<iframe>', function(done) {
          var ID = DynamicElements.addIFrame(function() {
            var ele = document.getElementById(ID);
            var innerEle = ele.contentDocument.getElementById('iframe_input');

            assert.isFalse(keyman.isAttached(ele));
            assert.isNotNull(innerEle);
            assert.isFalse(keyman.isAttached(innerEle));

            window.setTimeout(function() {
              done();
            }, 50);
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

      this.timeout(5000);

      before(function(done) {
        this.timeout(10000);

        fixture.setBase('unit_tests/fixtures');
        setupKMW({ attachType:'auto' }, done, 10000);
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
        }, 500);
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

        it('<iframe>', function(done) {
          var ID = DynamicElements.addIFrame(function() {
            var ele = document.getElementById(ID);
            var innerEle = ele.contentDocument.getElementById('iframe_input');

            assert.isTrue(keyman.isAttached(ele));
            assert.isNotNull(innerEle);
            assert.isTrue(keyman.isAttached(innerEle));
            keyman.detachFromControl(ele);

            window.setTimeout(function() {
              done();
            }, 50);
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