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
      });
    });
  });
});