var assert = chai.assert;

describe('TouchAliasElement', function() {
  this.timeout(kmwconfig.timeouts.standard);

  before(function() {
    fixture.setBase('fixtures');
  });

  beforeEach(function() {
    fixture.load("robustAttachment.html");

    // Default:  KMW's SMP extensions shouldn't be enabled.
    // Even if not used, this also ensures certain key functions are available; this
    // initializes KMW's specialized String methods are available.
    String.kmwEnableSupplementaryPlane(false);
  });

  afterEach(function() {
    fixture.cleanup();
  });

  describe("Without ['base']", function() {
    describe("White-box", function() {
      it('correctly reports the state of its content and caret (no SMP)', function() {
        var ta = document.getElementById(DynamicElements.addTouchAlias());
        String.kmwEnableSupplementaryPlane(false);
        
        // The "White-box" part of the test - we directly assign text to its internal objects here.
        ta.__preCaret.textContent = '12';
        ta.__postCaret.textContent = '345';
        // End "White-box" part of test.

        assert.equal(ta.getText(), '12345', "does not properly return its text content");
        assert.equal(ta.getTextCaret(), 2, "does not properly report its simulated-caret position");
        assert.equal(ta.getTextBeforeCaret(), '12', "does not properly return only pre-caret text");

        ta.setTextCaret(4);
        assert.equal(ta.getTextBeforeCaret(), '1234', "does not properly relocate the caret");

        // One more "White-box" part of test
        assert.equal(ta.__postCaret.textContent, '5', "does not properly manage text on caret relocation");
        ta.setText("1234567");
        assert.equal(ta.getTextCaret(), 7, "does not relocate caret to end by default after a 'setText' call");
        // End "White-box" part of test.
      });

      it('correctly reports the state of its content and caret (with SMP)', function() {
        var ta = document.getElementById(DynamicElements.addTouchAlias());
        String.kmwEnableSupplementaryPlane(true);
        
        // Makes a nice Unicode shortcut.
        var u = toSupplementaryPairString;
        var apple = 'a'+u(0x1d5c9)+'p'+'l'+u(0x1d5be) // Mixed-SMP 'apple', where the first 'p' and the 'e' are SMP.

        // The "White-box" part of the test - we directly assign text to its internal objects here.
        ta.__preCaret.textContent = 'a'+u(0x1d5c9);
        ta.__postCaret.textContent = 'p'+'l'+u(0x1d5be);
        // End "White-box" part of test.

        assert.equal(ta.getText(), apple, "does not properly return its text content");
        assert.equal(ta.getTextCaret(), 2, "does not properly report its simulated-caret position");
        assert.equal(ta.getTextBeforeCaret(), 'a'+u(0x1d5c9), "does not properly return only pre-caret text");

        ta.setTextCaret(4);
        assert.equal(ta.getTextBeforeCaret(), 'a'+u(0x1d5c9)+'p'+'l', "does not properly relocate the caret");

        // One more "White-box" part of test
        assert.equal(ta.__postCaret.textContent, u(0x1d5be), "does not properly manage text on caret relocation");
        ta.setText("1234567");
        assert.equal(ta.getTextCaret(), 7, "does not relocate caret to end by default after a 'setText' call");
        // End "White-box" part of test.
      });
    });

    it('manipulates text correctly', function() {
      var ta = document.getElementById(DynamicElements.addTouchAlias());
      // Should be empty by default for base-less elements.
      assert.equal("", ta.getText());

      // Set up initial conditions
      ta.setTextBeforeCaret("apples and oranges");

      // Check that setTextCaret works properly as part of our setup for the rest of this case.
      ta.setTextCaret(6);
      assert.equal(ta.getTextBeforeCaret(), "apples");

      // Now for the interesting part - do public text & caret handling methods work correctly?
      ta.setTextBeforeCaret("bananas");
      assert.equal(ta.getTextCaret(), "bananas".length, "Did not properly move text caret after inserting text");
      assert.equal(ta.getText(), "bananas and oranges", "Did not properly manage text after insertions");
    });
  });

  describe("With ['base']", function() {
    before(function() {
      // These tests require use of KMW's device-detection functionality.
      assert.isFalse(com.keyman.karma.DEVICE_DETECT_FAILURE, "Cannot run due to device detection failure.");
    })
    
    it('correctly aliases upon construction', function(done) {
      var input = document.getElementById(DynamicElements.addInput());
      input.value = "apples";

      var ta = document.getElementById(DynamicElements.addTouchAlias(input));
      assert.equal(ta.getText(), "apples", "Did not correctly initialize text from ['base']");

      var inputStyle = window.getComputedStyle(input);
      var aliasStyle = window.getComputedStyle(ta);

      // TouchAliasElements require a 1ms timeout post-creation to properly overlay their base element.
      window.setTimeout(function() {
        // inputStyle will report 'auto' for .top and .left... we need to rely on com.keyman.dom.Utils for a proper check here.
        // Unfortunately, even this doesn't always perfectly match - macOS Safari will fail on these.
        //assert.equal(aliasStyle.top, com.keyman.dom.Utils.getAbsoluteY(input) + 'px', "Positioning:  CSS 'top' does not match base");
        //assert.equal(aliasStyle.left, com.keyman.dom.Utils.getAbsoluteX(input) + 'px', "Positioning:  CSS 'left' does not match base");
        // Exact calculations for width and height are hard without relying on internal logic calculations.
        // assert.equal(aliasStyle.width, inputStyle.width, "Positioning:  CSS 'width' does not match base");
        // assert.equal(aliasStyle.height, inputStyle.height, "Positioning:  CSS 'top' does not match base");

        // Tell Mocha we're done with the test.
        done();
      }, 50);
    });

    it('manipulates base-element text correctly', function() {
      var input = document.getElementById(DynamicElements.addInput());
      var ta = document.getElementById(DynamicElements.addTouchAlias(input));
      // Should be empty by default for input elements without pre-set text.
      assert.equal("", ta.getText());

      // Assignment when no pre-existing text is present
      ta.setTextBeforeCaret("apples and oranges");
      assert.equal(input.value, "apples and oranges", "did not copy text to base element when previously empty");

      // Now for the interesting part - do public text & caret handling methods work correctly?
      ta.setTextCaret(6);
      ta.setTextBeforeCaret("bananas");
      assert.equal(input.value, "bananas and oranges", "did not correctly set text to base element after text modification");
    });
  });
});