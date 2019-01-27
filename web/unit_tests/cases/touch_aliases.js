var assert = chai.assert;

describe('TouchAliasElement', function() {
  this.timeout(kmwconfig.timeouts.standard);

  before(function() {
    fixture.setBase('unit_tests/fixtures');
  });

  beforeEach(function() {
    fixture.load("robustAttachment.html");
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

        String.kmwEnableSupplementaryPlane(false);
      });
    });
  });
});