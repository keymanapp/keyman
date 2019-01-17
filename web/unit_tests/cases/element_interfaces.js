var assert = chai.assert;

var InterfaceTests;

// Define common interface testing functions that can be run upon the EditableElement interface.
if(typeof InterfaceTests == 'undefined') {
  InterfaceTests = {};

  (function(){
    // Makes a nice Unicode shortcut.
    var u = toSupplementaryPairString;

    InterfaceTests.Strings = {};

    InterfaceTests.Strings.Apple = {};
    InterfaceTests.Strings.Apple.normal = 'apple';
    // Built in-line via function.  Looks functionally equivalent to "apple", but with SMP characters.
    InterfaceTests.Strings.Apple.smp = u(0x1d5ba)+u(0x1d5c9)+u(0x1d5c9)+u(0x1d5c5)+u(0x1d5be);
    InterfaceTests.Strings.Apple.mixed = 'a'+u(0x1d5c9)+'p'+'l'+u(0x1d5be); 

    //#region Defines helpers related to HTMLInputElement / Input test setup.
    InterfaceTests.Input = {};

    InterfaceTests.Input.setupElement = function() {
      var id = DynamicElements.addInput();
      var elem = document.getElementById(id);
      var wrapper = new com.keyman.dom.Input(elem);

      return {elem: elem, wrapper: wrapper};
    }

    InterfaceTests.Input.resetWithText = function(pair, string) {
      pair.elem.value = string;
      pair.elem.setSelectionRange(0, 0);
    }

    // Implemented for completeness and generality with other tests.
    InterfaceTests.Input.setCaret = function(pair, index) {
      pair.elem.setSelectionRange(index, index);
    }

    // Implemented for completeness and generality with other tests.
    InterfaceTests.Input.getCaret = function(pair) {
      return pair.elem.selectionStart;
    }

    InterfaceTests.Input.setSelectionRange = function(pair, start, end) {
      // For HTMLInputElements, `start` must be before `end` to create a proper range.
      var dir = 'forward';

      if(end < start) {
        dir = 'backward';
        var temp = end;
        end = start;
        start = temp;
      }

      pair.elem.setSelectionRange(start, end, dir);
    }

    InterfaceTests.Input.setText = function(pair, text) {
      pair.elem.value = text;
    }
    //#endregion

    //#region Defines helpers related to HTMLTextAreaElement / TextArea test setup.
    InterfaceTests.TextArea = {};

    InterfaceTests.TextArea.setupElement = function() {
      var id = DynamicElements.addText();
      var elem = document.getElementById(id);
      var wrapper = new com.keyman.dom.TextArea(elem);

      return {elem: elem, wrapper: wrapper};
    }

    InterfaceTests.TextArea.resetWithText = function(pair, string) {
      pair.elem.value = string;
      pair.elem.setSelectionRange(0, 0);
    }

    // Implemented for completeness and generality with other tests.
    InterfaceTests.TextArea.setCaret = function(pair, index) {
      pair.elem.setSelectionRange(index, index);
    }

    // Implemented for completeness and generality with other tests.
    InterfaceTests.TextArea.getCaret = function(pair) {
      return pair.elem.selectionStart;
    }

    InterfaceTests.TextArea.setSelectionRange = function(pair, start, end) {
      // For HTMLInputElements, `start` must be before `end` to create a proper range.
      var dir = 'forward';

      if(end < start) {
        dir = 'backward';
        var temp = end;
        end = start;
        start = temp;
      }

      pair.elem.setSelectionRange(start, end, dir);
    }

    InterfaceTests.TextArea.setText = function(pair, text) {
      pair.elem.value = text;
    }
    //#endregion

    //#region Defines common test patterns across element tests
    InterfaceTests.Tests = {};

    InterfaceTests.Tests.getCaretNoSelection = function(testObj, pair) {
      var Apple = InterfaceTests.Strings.Apple;

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      testObj.setCaret(pair, 4);
      assert.equal(pair.wrapper.getCaret(), 4, "Failed to correctly read the caret's position within a simple string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.smp);
      testObj.setCaret(pair, 8);
      assert.equal(pair.wrapper.getCaret(), 4, "Failed to correctly read the caret's position within an SMP string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.mixed);
      testObj.setCaret(pair, 5);
      assert.equal(pair.wrapper.getCaret(), 4, "Failed to correctly read the caret's position within a mixed SMP string");
      String.kmwEnableSupplementaryPlane(false);
    }

    InterfaceTests.Tests.getCaretWithSelection = function(testObj, pair) {
      var Apple = InterfaceTests.Strings.Apple;

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      testObj.setSelectionRange(pair, 3, 5);
      assert.equal(pair.wrapper.getCaret(), 3, "Failed to choose the caret's initial position for forward-selections within a simple string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      testObj.setSelectionRange(pair, 5, 3);
      assert.equal(pair.wrapper.getCaret(), 3, "Failed to choose the caret's initial position for backward-selections within a simple string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.smp);
      testObj.setSelectionRange(pair, 6, 10);
      assert.equal(pair.wrapper.getCaret(), 3, "Failed to caret's initial position for forward-selections within an SMP string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.smp);
      testObj.setSelectionRange(pair, 10, 6);
      assert.equal(pair.wrapper.getCaret(), 3, "Failed to caret's initial position for backward-selections within an SMP string");
      String.kmwEnableSupplementaryPlane(false);
    }

    InterfaceTests.Tests.setCaretNoSelection = function(testObj, pair) {
      var Apple = InterfaceTests.Strings.Apple;

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      pair.wrapper.setCaret(4);
      assert.equal(testObj.getCaret(pair), 4, "Failed to correctly set the caret within a simple string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      testObj.resetWithText(pair, Apple.smp);
      String.kmwEnableSupplementaryPlane(true);
      pair.wrapper.setCaret(4);
      assert.equal(testObj.getCaret(pair), 8, "Failed to correctly set the caret within an SMP string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      testObj.resetWithText(pair, Apple.mixed);
      String.kmwEnableSupplementaryPlane(true);
      pair.wrapper.setCaret(4);
      assert.equal(testObj.getCaret(pair), 5, "Failed to correctly set the caret within a mixed SMP string");
      String.kmwEnableSupplementaryPlane(false);
    }

    InterfaceTests.Tests.setCaretWithSelection = function(testObj, pair) {
      var Apple = InterfaceTests.Strings.Apple;

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      testObj.setSelectionRange(pair, 3, 5);
      pair.wrapper.setCaret(4);
      assert.equal(testObj.getCaret(pair), 4, "Failed to correctly set the caret within a simple string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      testObj.setSelectionRange(pair, 5, 3);
      pair.wrapper.setCaret(4);
      assert.equal(testObj.getCaret(pair), 4, "Failed to correctly set the caret within a simple string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.smp);
      testObj.setSelectionRange(pair, 6, 10);
      pair.wrapper.setCaret(4);
      assert.equal(testObj.getCaret(pair), 8, "Failed to correctly set the caret within an SMP string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.smp);
      testObj.setSelectionRange(pair, 10, 6);
      pair.wrapper.setCaret(4);
      assert.equal(testObj.getCaret(pair), 8, "Failed to correctly set the caret within an SMP string");
      String.kmwEnableSupplementaryPlane(false);
    }

    InterfaceTests.Tests.getTextNoSelection = function(testObj, pair) {
      var Apple = InterfaceTests.Strings.Apple;

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      assert.equal(pair.wrapper.getText(), Apple.normal, "Failed to properly return its stored value:  a simple string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.smp);
      assert.equal(pair.wrapper.getText(), Apple.smp, "Failed to properly return its stored value:  a string of SMP characters");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.mixed);
      assert.equal(pair.wrapper.getText(), Apple.mixed, "Failed to properly return its stored value:  a string with some SMP characters");
      String.kmwEnableSupplementaryPlane(false);
    }

    InterfaceTests.Tests.getTextWithSelection = function(testObj, pair) {
      var Apple = InterfaceTests.Strings.Apple;

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      testObj.setSelectionRange(pair, 3, 5);
      assert.equal(pair.wrapper.getText(), Apple.normal, "Failed to properly return its stored value:  a simple string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.smp);
      testObj.setSelectionRange(pair, 6, 10);
      assert.equal(pair.wrapper.getText(), Apple.smp, "Failed to properly return its stored value:  a string of SMP characters");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.mixed);
      testObj.setSelectionRange(pair, 4, 7);
      assert.equal(pair.wrapper.getText(), Apple.mixed, "Failed to properly return its stored value:  a string with some SMP characters");
      String.kmwEnableSupplementaryPlane(false);
    }

    InterfaceTests.Tests.getTextBeforeCaretNoSelection = function(testObj, pair) {
      var Apple = InterfaceTests.Strings.Apple;

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      pair.wrapper.setCaret(4);
      assert.equal(pair.wrapper.getTextBeforeCaret(), Apple.normal.substr(0, 4), "Failed to properly return correct substring for a simple string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.smp);
      pair.wrapper.setCaret(4);
      assert.equal(pair.wrapper.getTextBeforeCaret(), Apple.smp.substr(0, 8), "Failed to properly return correct substring for an SMP string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.mixed);
      pair.wrapper.setCaret(4);
      assert.equal(pair.wrapper.getTextBeforeCaret(), Apple.mixed.substr(0, 5), "Failed to properly return correct substring for a mixed SMP string");
      String.kmwEnableSupplementaryPlane(false);
    }

    InterfaceTests.Tests.getTextBeforeCaretWithSelection = function(testObj, pair) {
      var Apple = InterfaceTests.Strings.Apple;

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      testObj.setSelectionRange(pair, 3, 5);
      assert.equal(pair.wrapper.getTextBeforeCaret(), Apple.normal.substr(0, 3), "Failed simple string, normal-order selection");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      testObj.setSelectionRange(pair, 5, 3);
      assert.equal(pair.wrapper.getTextBeforeCaret(), Apple.normal.substr(0, 3), "Failed simple string, reverse-order selection");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.smp);
      testObj.setSelectionRange(pair, 6, 10);
      assert.equal(pair.wrapper.getTextBeforeCaret(), Apple.smp.substr(0, 6), "Failed SMP string, normal-order selection");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.smp);
      testObj.setSelectionRange(pair, 10, 6);
      assert.equal(pair.wrapper.getTextBeforeCaret(), Apple.smp.substr(0, 6), "Failed SMP string, reverse-order selection");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.mixed);
      testObj.setSelectionRange(pair, 4, 7);
      assert.equal(pair.wrapper.getTextBeforeCaret(), Apple.mixed.substr(0, 4), "Failed mixed SMP string, forward-order selection");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();
      
      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.mixed);
      testObj.setSelectionRange(pair, 7, 4);
      assert.equal(pair.wrapper.getTextBeforeCaret(), Apple.mixed.substr(0, 4), "Failed mixed SMP string, reverse-order selection");
      String.kmwEnableSupplementaryPlane(false);
    }

    InterfaceTests.Tests.getTextAfterCaretNoSelection = function(testObj, pair) {
      var Apple = InterfaceTests.Strings.Apple;

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      pair.wrapper.setCaret(4);
      assert.equal(pair.wrapper.getTextAfterCaret(), Apple.normal.substr(4), "Failed to properly return correct substring for a simple string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.smp);
      pair.wrapper.setCaret(4);
      assert.equal(pair.wrapper.getTextAfterCaret(), Apple.smp.substr(8), "Failed to properly return correct substring for an SMP string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.mixed);
      pair.wrapper.setCaret(4);
      assert.equal(pair.wrapper.getTextAfterCaret(), Apple.mixed.substr(5), "Failed to properly return correct substring for a mixed SMP string");
      String.kmwEnableSupplementaryPlane(false);
    }

    InterfaceTests.Tests.getTextAfterCaretWithSelection = function(testObj, pair) {
      var Apple = InterfaceTests.Strings.Apple;

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      testObj.setSelectionRange(pair, 2, 4);
      assert.equal(pair.wrapper.getTextAfterCaret(), Apple.normal.substr(4), "Failed simple string, normal-order selection");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      testObj.setSelectionRange(pair, 4, 2);
      assert.equal(pair.wrapper.getTextAfterCaret(), Apple.normal.substr(4), "Failed simple string, reverse-order selection");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.smp);
      testObj.setSelectionRange(pair, 4, 8);
      assert.equal(pair.wrapper.getTextAfterCaret(), Apple.smp.substr(8), "Failed SMP string, normal-order selection");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.smp);
      testObj.setSelectionRange(pair, 8, 4);
      assert.equal(pair.wrapper.getTextAfterCaret(), Apple.smp.substr(8), "Failed SMP string, reverse-order selection");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.mixed);
      testObj.setSelectionRange(pair, 3, 5);
      assert.equal(pair.wrapper.getTextAfterCaret(), Apple.mixed.substr(5), "Failed mixed SMP string, forward-order selection");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();
      
      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.mixed);
      testObj.setSelectionRange(pair, 5, 3);
      assert.equal(pair.wrapper.getTextAfterCaret(), Apple.mixed.substr(5), "Failed mixed SMP string, reverse-order selection");
      String.kmwEnableSupplementaryPlane(false);
    }

    InterfaceTests.Tests.clearSelection = function(testObj, pair) {
      var Apple = InterfaceTests.Strings.Apple;

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.mixed);
      testObj.setSelectionRange(pair, 3, 5);
      assert.equal(pair.wrapper.getText(), Apple.mixed, "String should not be modified on selection: forward-order selection");
      pair.wrapper.clearSelection();
      assert.equal(pair.wrapper.getText(), Apple.mixed.substr(0, 3) + Apple.mixed.substr(5), "Selected text improperly removed: forward-order selection");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.mixed);
      testObj.setSelectionRange(pair, 5, 3);
      assert.equal(pair.wrapper.getText(), Apple.mixed, "String should not be modified on selection: reverse-order selection");
      pair.wrapper.clearSelection();
      assert.equal(pair.wrapper.getText(), Apple.mixed.substr(0, 3) + Apple.mixed.substr(5), "Selected text improperly removed: backward-order selection");
      String.kmwEnableSupplementaryPlane(false);
    }

    InterfaceTests.Tests.setTextBeforeCaretNoSelection = function(testObj, pair) {
      var Apple = InterfaceTests.Strings.Apple;

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      pair.wrapper.setCaret(4);
      pair.wrapper.setTextBeforeCaret(Apple.normal.substr(0, 2))
      assert.equal(pair.wrapper.getText(), Apple.normal.substr(0, 2) + Apple.normal.substr(4), "Error replacing text in a simple string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.smp);
      pair.wrapper.setCaret(4);
      pair.wrapper.setTextBeforeCaret(Apple.smp.substr(0, 4));
      assert.equal(pair.wrapper.getText(), Apple.smp.substr(0, 4) + Apple.smp.substr(8), "Error replacing text in an SMP string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.mixed);
      pair.wrapper.setCaret(4);
      pair.wrapper.setTextBeforeCaret(Apple.smp.substr(0, 4))
      assert.equal(pair.wrapper.getText(), Apple.smp.substr(0, 4) + Apple.mixed.substr(5), "Error replacing text in a mixed SMP string");
      String.kmwEnableSupplementaryPlane(false);
    }

    InterfaceTests.Tests.setTextBeforeCaretWithSelection = function(testObj, pair) {
      var Apple = InterfaceTests.Strings.Apple;

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.mixed);
      testObj.setSelectionRange(pair, 3, 5);
      pair.wrapper.setTextBeforeCaret(Apple.mixed.substr(0, 3));
      assert.equal(pair.wrapper.getText(), Apple.mixed, "Actively-selected text was erroneously removed");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.mixed);
      testObj.setSelectionRange(pair, 3, 5);
      pair.wrapper.setTextBeforeCaret(Apple.smp.substr(0, 4));
      assert.equal(pair.wrapper.getText(), Apple.smp.substr(0, 4) + Apple.mixed.substr(3), "Error with text replacement: forward-order selection");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.mixed);
      testObj.setSelectionRange(pair, 5, 3);
      pair.wrapper.setTextBeforeCaret(Apple.smp.substr(0, 4));
      assert.equal(pair.wrapper.getText(), Apple.smp.substr(0, 4) + Apple.mixed.substr(3), "Error with text replacement:  backward-order selection");
      String.kmwEnableSupplementaryPlane(false);
    }
    //#endregion

  })();
}

describe('Element Input/Output Interfacing', function() {
  this.timeout(kmwconfig.timeouts.standard);

  before(function() {
    fixture.setBase('unit_tests/fixtures');
    
    // Make sure the basic SMP extension hooks exist to prevent errors later.
    String.kmwEnableSupplementaryPlane(false);
  });

  beforeEach(function() {
    fixture.load("robustAttachment.html");
  })

  afterEach(function() {
    fixture.cleanup();
  });

  describe('Wrapper: HTMLInputElement', function() {
    /**
     * The design of these tests is to ensure that all caret handling works correctly, 
     * independently of other methods.  Other tests will then rely upon these methods
     * to simplify their code.
     */
    describe('Caret Handling', function() {
      describe('setCaret', function() {
        it('correctly places the caret (no prior selection)', function() {
          var TestObj = InterfaceTests.Input;
          var input = TestObj.setupElement();

          InterfaceTests.Tests.setCaretNoSelection(TestObj, input);
        });

        it('correctly places the caret (prior selection)', function() {
          var TestObj = InterfaceTests.Input;
          var input = TestObj.setupElement();

          InterfaceTests.Tests.setCaretWithSelection(TestObj, input);
        });
      });

      describe('getCaret', function() {
        it('correctly reports the position of the caret (no selection)', function() {
          var TestObj = InterfaceTests.Input;
          var input = TestObj.setupElement();

          InterfaceTests.Tests.getCaretNoSelection(TestObj, input);
        });

        it('correctly reports the position of the caret (active selection)', function() {
          var TestObj = InterfaceTests.Input;
          var input = TestObj.setupElement();

          InterfaceTests.Tests.getCaretWithSelection(TestObj, input);
        });
      });
    });

    describe('Text Retrieval', function(){
      describe('getText', function() {
        it('correctly returns text (no active selection)', function() {
          var TestObj = InterfaceTests.Input;
          var input = TestObj.setupElement();

          InterfaceTests.Tests.getTextNoSelection(TestObj, input);
        });

        it('correctly returns text (with active selection)', function() {
          var TestObj = InterfaceTests.Input;
          var input = TestObj.setupElement();

          InterfaceTests.Tests.getTextWithSelection(TestObj, input);
        });
      });

      describe('getTextBeforeCaret', function() {
        it('correctly returns text (no active selection)', function() {
          var TestObj = InterfaceTests.Input;
          var input = TestObj.setupElement();

          InterfaceTests.Tests.getTextBeforeCaretNoSelection(TestObj, input);
        });

        it('correctly returns text (with active selection)', function() {
          var TestObj = InterfaceTests.Input;
          var input = TestObj.setupElement();

          InterfaceTests.Tests.getTextBeforeCaretWithSelection(TestObj, input);
        });
      });

      describe('getTextAfterCaret', function() {
        it('correctly returns text (no active selection)', function() {
          var TestObj = InterfaceTests.Input;
          var input = TestObj.setupElement();

          InterfaceTests.Tests.getTextAfterCaretNoSelection(TestObj, input);
        });

        it('correctly returns text (with active selection)', function() {
          var TestObj = InterfaceTests.Input;
          var input = TestObj.setupElement();

          InterfaceTests.Tests.getTextAfterCaretWithSelection(TestObj, input);
        });
      });
    });

    describe('Text Mutation', function() {
      describe('clearSelection', function() {
        it('properly deletes selected text', function() {
          var TestObj = InterfaceTests.Input;
          var input = TestObj.setupElement();

          InterfaceTests.Tests.clearSelection(TestObj, input);
        });
      });

      describe('setTextBeforeCaret', function() {
        it("correctly replaces the element's 'context' (no active selection)", function() {
          var TestObj = InterfaceTests.Input;
          var input = TestObj.setupElement();

          InterfaceTests.Tests.setTextBeforeCaretNoSelection(TestObj, input);
        });

        it("correctly replaces the element's 'context' (with active selection)", function() {
          var TestObj = InterfaceTests.Input;
          var input = TestObj.setupElement();

          InterfaceTests.Tests.setTextBeforeCaretWithSelection(TestObj, input);
        });
      });
    });
  });

  describe('Wrapper: HTMLTextAreaElement', function() {
    /**
     * The design of these tests is to ensure that all caret handling works correctly, 
     * independently of other methods.  Other tests will then rely upon these methods
     * to simplify their code.
     */
    describe('Caret Handling', function() {
      describe('setCaret', function() {
        it('correctly places the caret (no prior selection)', function() {
          var TestObj = InterfaceTests.TextArea;
          var textArea = TestObj.setupElement();

          InterfaceTests.Tests.setCaretNoSelection(TestObj, textArea);
        });

        it('correctly places the caret (prior selection)', function() {
          var TestObj = InterfaceTests.TextArea;
          var textArea = TestObj.setupElement();

          InterfaceTests.Tests.setCaretWithSelection(TestObj, textArea);
        });
      });

      describe('getCaret', function() {
        it('correctly reports the position of the caret (no selection)', function() {
          var TestObj = InterfaceTests.TextArea;
          var textArea = TestObj.setupElement();

          InterfaceTests.Tests.getCaretNoSelection(TestObj, textArea);
        });

        it('correctly reports the position of the caret (active selection)', function() {
          var TestObj = InterfaceTests.TextArea;
          var textArea = TestObj.setupElement();

          InterfaceTests.Tests.getCaretWithSelection(TestObj, textArea);
        });
      });
    });

    describe('Text Retrieval', function(){
      describe('getText', function() {
        it('correctly returns text (no active selection)', function() {
          var TestObj = InterfaceTests.TextArea;
          var textArea = TestObj.setupElement();

          InterfaceTests.Tests.getTextNoSelection(TestObj, textArea);
        });

        it('correctly returns text (with active selection)', function() {
          var TestObj = InterfaceTests.TextArea;
          var textArea = TestObj.setupElement();

          InterfaceTests.Tests.getTextWithSelection(TestObj, textArea);
        });
      });

      describe('getTextBeforeCaret', function() {
        it('correctly returns text (no active selection)', function() {
          var TestObj = InterfaceTests.TextArea;
          var textArea = TestObj.setupElement();

          InterfaceTests.Tests.getTextBeforeCaretNoSelection(TestObj, textArea);
        });

        it('correctly returns text (with active selection)', function() {
          var TestObj = InterfaceTests.TextArea;
          var textArea = TestObj.setupElement();

          InterfaceTests.Tests.getTextBeforeCaretWithSelection(TestObj, textArea);
        });
      });

      describe('getTextAfterCaret', function() {
        it('correctly returns text (no active selection)', function() {
          var TestObj = InterfaceTests.TextArea;
          var textArea = TestObj.setupElement();

          InterfaceTests.Tests.getTextAfterCaretNoSelection(TestObj, textArea);
        });

        it('correctly returns text (with active selection)', function() {
          var TestObj = InterfaceTests.TextArea;
          var textArea = TestObj.setupElement();

          InterfaceTests.Tests.getTextAfterCaretWithSelection(TestObj, textArea);
        });
      });
    });

    describe('Text Mutation', function() {
      describe('clearSelection', function() {
        it('properly deletes selected text', function() {
          var TestObj = InterfaceTests.TextArea;
          var textArea = TestObj.setupElement();

          InterfaceTests.Tests.clearSelection(TestObj, textArea);
        });
      });

      describe('setTextBeforeCaret', function() {
        it("correctly replaces the element's 'context' (no active selection)", function() {
          var TestObj = InterfaceTests.TextArea;
          var textArea = TestObj.setupElement();

          InterfaceTests.Tests.setTextBeforeCaretNoSelection(TestObj, textArea);
        });

        it("correctly replaces the element's 'context' (with active selection)", function() {
          var TestObj = InterfaceTests.TextArea;
          var textArea = TestObj.setupElement();

          InterfaceTests.Tests.setTextBeforeCaretWithSelection(TestObj, textArea);
        });
      });
    });
  });
});