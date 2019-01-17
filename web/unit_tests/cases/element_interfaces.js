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

    // Corresponds to a helper method for certain classes.
    InterfaceTests.Tests.getCaretNoSelection = function(testObj) {
      var Apple = InterfaceTests.Strings.Apple;
      var pair = testObj.setupElement();

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

    // Corresponds to a helper method for certain classes.
    InterfaceTests.Tests.getCaretWithSelection = function(testObj) {
      var Apple = InterfaceTests.Strings.Apple;
      var pair = testObj.setupElement();

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

    // Corresponds to a helper method for certain classes.
    InterfaceTests.Tests.setCaretNoSelection = function(testObj) {
      var Apple = InterfaceTests.Strings.Apple;
      var pair = testObj.setupElement();

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

    // Corresponds to a helper method for certain classes.
    InterfaceTests.Tests.setCaretWithSelection = function(testObj) {
      var Apple = InterfaceTests.Strings.Apple;
      var pair = testObj.setupElement();

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

    InterfaceTests.Tests.getTextNoSelection = function(testObj) {
      var Apple = InterfaceTests.Strings.Apple;
      var pair = testObj.setupElement();

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

    InterfaceTests.Tests.getTextWithSelection = function(testObj) {
      var Apple = InterfaceTests.Strings.Apple;
      var pair = testObj.setupElement();

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

    InterfaceTests.Tests.getTextBeforeCaretNoSelection = function(testObj) {
      var Apple = InterfaceTests.Strings.Apple;
      var pair = testObj.setupElement();

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      testObj.setCaret(pair, 4);
      assert.equal(pair.wrapper.getTextBeforeCaret(), Apple.normal.substr(0, 4), "Failed to properly return correct substring for a simple string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.smp);
      testObj.setCaret(pair, 8);
      assert.equal(pair.wrapper.getTextBeforeCaret(), Apple.smp.substr(0, 8), "Failed to properly return correct substring for an SMP string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.mixed);
      testObj.setCaret(pair, 5);
      assert.equal(pair.wrapper.getTextBeforeCaret(), Apple.mixed.substr(0, 5), "Failed to properly return correct substring for a mixed SMP string");
      String.kmwEnableSupplementaryPlane(false);
    }

    InterfaceTests.Tests.getTextBeforeCaretWithSelection = function(testObj) {
      var Apple = InterfaceTests.Strings.Apple;
      var pair = testObj.setupElement();

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

    InterfaceTests.Tests.getTextAfterCaretNoSelection = function(testObj) {
      var Apple = InterfaceTests.Strings.Apple;
      var pair = testObj.setupElement();

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      testObj.setCaret(pair, 4);
      assert.equal(pair.wrapper.getTextAfterCaret(), Apple.normal.substr(4), "Failed to properly return correct substring for a simple string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.smp);
      testObj.setCaret(pair, 8);
      assert.equal(pair.wrapper.getTextAfterCaret(), Apple.smp.substr(8), "Failed to properly return correct substring for an SMP string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.mixed);
      testObj.setCaret(pair, 5);
      assert.equal(pair.wrapper.getTextAfterCaret(), Apple.mixed.substr(5), "Failed to properly return correct substring for a mixed SMP string");
      String.kmwEnableSupplementaryPlane(false);
    }

    InterfaceTests.Tests.getTextAfterCaretWithSelection = function(testObj) {
      var Apple = InterfaceTests.Strings.Apple;
      var pair = testObj.setupElement();

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

    InterfaceTests.Tests.clearSelection = function(testObj) {
      var Apple = InterfaceTests.Strings.Apple;
      var pair = testObj.setupElement();

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

    // Corresponds to a helper method for certain subclasses.
    InterfaceTests.Tests.setTextBeforeCaretNoSelection = function(testObj) {
      var Apple = InterfaceTests.Strings.Apple;
      var pair = testObj.setupElement();

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      testObj.setCaret(pair, 4);
      pair.wrapper.setTextBeforeCaret(Apple.normal.substr(0, 2))
      assert.equal(pair.wrapper.getText(), Apple.normal.substr(0, 2) + Apple.normal.substr(4), "Error replacing text in a simple string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.smp);
      testObj.setCaret(pair, 8);
      pair.wrapper.setTextBeforeCaret(Apple.smp.substr(0, 4));
      assert.equal(pair.wrapper.getText(), Apple.smp.substr(0, 4) + Apple.smp.substr(8), "Error replacing text in an SMP string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.mixed);
      testObj.setCaret(pair, 5);
      pair.wrapper.setTextBeforeCaret(Apple.smp.substr(0, 4))
      assert.equal(pair.wrapper.getText(), Apple.smp.substr(0, 4) + Apple.mixed.substr(5), "Error replacing text in a mixed SMP string");
      String.kmwEnableSupplementaryPlane(false);
    }

    InterfaceTests.Tests.setTextBeforeCaretWithSelection = function(testObj) {
      var Apple = InterfaceTests.Strings.Apple;
      var pair = testObj.setupElement();

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

    InterfaceTests.Tests.deleteCharsBeforeCaretNoSelection = function(testObj) {
      var Apple = InterfaceTests.Strings.Apple;
      var pair = testObj.setupElement();

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      testObj.setCaret(pair, 4);
      pair.wrapper.deleteCharsBeforeCaret(1);
      assert.equal(pair.wrapper.getText(), Apple.normal.substr(0, 3) + Apple.normal.substr(4), "Error deleting context chars from a simple string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.smp);
      testObj.setCaret(pair, 8);
      pair.wrapper.deleteCharsBeforeCaret(1);
      assert.equal(pair.wrapper.getText(), Apple.smp.substr(0, 6) + Apple.smp.substr(8), "Error deleting context chars from an SMP string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.mixed);
      testObj.setCaret(pair, 4);
      pair.wrapper.deleteCharsBeforeCaret(2);
      assert.equal(pair.wrapper.getText(), Apple.mixed.substr(0, 1) + Apple.mixed.substr(4), "Error deleting context chars from a mixed SMP string");
      String.kmwEnableSupplementaryPlane(false);
    }

    InterfaceTests.Tests.deleteCharsBeforeCaretWithSelection = function(testObj) {
      var Apple = InterfaceTests.Strings.Apple;
      var pair = testObj.setupElement();

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      testObj.setSelectionRange(pair, 2, 4);
      pair.wrapper.deleteCharsBeforeCaret(1);
      assert.equal(pair.wrapper.getText(), Apple.normal.substr(0, 1) + Apple.normal.substr(2), "Selected text erroneously deleted from selection");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();
    }

    InterfaceTests.Tests.insertTextBeforeCaretNoSelection = function(testObj) {
      var Apple = InterfaceTests.Strings.Apple;
      var pair = testObj.setupElement();

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      testObj.setCaret(pair, 4);
      pair.wrapper.insertTextBeforeCaret(Apple.normal.substr(0, 2))
      assert.equal(pair.wrapper.getText(), Apple.normal.substr(0, 4) + Apple.normal.substr(0, 2) + Apple.normal.substr(4), "Error inserting text in a simple string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.smp);
      testObj.setCaret(pair, 8);
      pair.wrapper.insertTextBeforeCaret(Apple.smp.substr(0, 4));
      assert.equal(pair.wrapper.getText(), Apple.smp.substr(0, 8) + Apple.smp.substr(0, 4) + Apple.smp.substr(8), "Error inserting text in an SMP string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.mixed);
      testObj.setCaret(pair, 5);
      pair.wrapper.insertTextBeforeCaret(Apple.smp.substr(0, 4));
      assert.equal(pair.wrapper.getText(), Apple.mixed.substr(0, 5) + Apple.smp.substr(0, 4) + Apple.mixed.substr(5), "Error inserting text in a mixed SMP string");
      String.kmwEnableSupplementaryPlane(false);
    }

    InterfaceTests.Tests.insertTextBeforeCaretWithSelection = function(testObj) {
      var Apple = InterfaceTests.Strings.Apple;
      var pair = testObj.setupElement();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.mixed);
      testObj.setSelectionRange(pair, 3, 5);
      pair.wrapper.insertTextBeforeCaret("");
      assert.equal(pair.wrapper.getText(), Apple.mixed, "Actively-selected text was erroneously removed");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.mixed);
      testObj.setSelectionRange(pair, 3, 5);
      pair.wrapper.insertTextBeforeCaret(Apple.smp.substr(0, 4));
      assert.equal(pair.wrapper.getText(), Apple.mixed.substr(0, 3) + Apple.smp.substr(0, 4) + Apple.mixed.substr(3), "Error with text replacement: forward-order selection");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.mixed);
      testObj.setSelectionRange(pair, 5, 3);
      pair.wrapper.insertTextBeforeCaret(Apple.smp.substr(0, 4));
      assert.equal(pair.wrapper.getText(), Apple.mixed.substr(0, 3) + Apple.smp.substr(0, 4) + Apple.mixed.substr(3), "Error with text replacement:  backward-order selection");
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
          InterfaceTests.Tests.setCaretNoSelection(InterfaceTests.Input);
        });

        it('correctly places the caret (prior selection)', function() {
          InterfaceTests.Tests.setCaretWithSelection(InterfaceTests.Input);
        });
      });

      describe('getCaret', function() {
        it('correctly reports the position of the caret (no selection)', function() {
          InterfaceTests.Tests.getCaretNoSelection(InterfaceTests.Input);
        });

        it('correctly reports the position of the caret (active selection)', function() {
          InterfaceTests.Tests.getCaretWithSelection(InterfaceTests.Input);
        });
      });
    });

    describe('Text Retrieval', function(){
      describe('getText', function() {
        it('correctly returns text (no active selection)', function() {
          InterfaceTests.Tests.getTextNoSelection(InterfaceTests.Input);
        });

        it('correctly returns text (with active selection)', function() {
          InterfaceTests.Tests.getTextWithSelection(InterfaceTests.Input);
        });
      });

      describe('getTextBeforeCaret', function() {
        it('correctly returns text (no active selection)', function() {
          InterfaceTests.Tests.getTextBeforeCaretNoSelection(InterfaceTests.Input);
        });

        it('correctly returns text (with active selection)', function() {
          InterfaceTests.Tests.getTextBeforeCaretWithSelection(InterfaceTests.Input);
        });
      });

      describe('getTextAfterCaret', function() {
        it('correctly returns text (no active selection)', function() {
          InterfaceTests.Tests.getTextAfterCaretNoSelection(InterfaceTests.Input);
        });

        it('correctly returns text (with active selection)', function() {
          InterfaceTests.Tests.getTextAfterCaretWithSelection(InterfaceTests.Input);
        });
      });
    });

    describe('Text Mutation', function() {
      describe('clearSelection', function() {
        it('properly deletes selected text', function() {
          InterfaceTests.Tests.clearSelection(InterfaceTests.Input);
        });
      });

      describe('deleteCharsBeforeCaret', function() {
        it("correctly deletes characters from 'context' (no active selection)", function() {
          InterfaceTests.Tests.deleteCharsBeforeCaretNoSelection(InterfaceTests.Input);
        });

        it("correctly deletes characters from 'context' (with active selection)", function() {
          InterfaceTests.Tests.deleteCharsBeforeCaretWithSelection(InterfaceTests.Input);
        });
      });

      describe('insertTextBeforeCaret', function() {
        it("correctly replaces the element's 'context' (no active selection)", function() {
          InterfaceTests.Tests.insertTextBeforeCaretNoSelection(InterfaceTests.Input);
        });

        it("correctly replaces the element's 'context' (with active selection)", function() {
          InterfaceTests.Tests.insertTextBeforeCaretWithSelection(InterfaceTests.Input);
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
          InterfaceTests.Tests.setCaretNoSelection(InterfaceTests.TextArea);
        });

        it('correctly places the caret (prior selection)', function() {
          InterfaceTests.Tests.setCaretWithSelection(InterfaceTests.TextArea);
        });
      });

      describe('getCaret', function() {
        it('correctly reports the position of the caret (no selection)', function() {
          InterfaceTests.Tests.getCaretNoSelection(InterfaceTests.TextArea);
        });

        it('correctly reports the position of the caret (active selection)', function() {
          InterfaceTests.Tests.getCaretWithSelection(InterfaceTests.TextArea);
        });
      });
    });

    describe('Text Retrieval', function(){
      describe('getText', function() {
        it('correctly returns text (no active selection)', function() {
          InterfaceTests.Tests.getTextNoSelection(InterfaceTests.TextArea);
        });

        it('correctly returns text (with active selection)', function() {
          InterfaceTests.Tests.getTextWithSelection(InterfaceTests.TextArea);
        });
      });

      describe('getTextBeforeCaret', function() {
        it('correctly returns text (no active selection)', function() {
          InterfaceTests.Tests.getTextBeforeCaretNoSelection(InterfaceTests.TextArea);
        });

        it('correctly returns text (with active selection)', function() {
          InterfaceTests.Tests.getTextBeforeCaretWithSelection(InterfaceTests.TextArea);
        });
      });

      describe('getTextAfterCaret', function() {
        it('correctly returns text (no active selection)', function() {
          InterfaceTests.Tests.getTextAfterCaretNoSelection(InterfaceTests.TextArea);
        });

        it('correctly returns text (with active selection)', function() {
          InterfaceTests.Tests.getTextAfterCaretWithSelection(InterfaceTests.TextArea);
        });
      });
    });

    describe('Text Mutation', function() {
      describe('clearSelection', function() {
        it('properly deletes selected text', function() {
          InterfaceTests.Tests.clearSelection(InterfaceTests.TextArea);
        });
      });

      describe('deleteCharsBeforeCaret', function() {
        it("correctly deletes characters from 'context' (no active selection)", function() {
          InterfaceTests.Tests.deleteCharsBeforeCaretNoSelection(InterfaceTests.TextArea);
        });

        it("correctly deletes characters from 'context' (with active selection)", function() {
          InterfaceTests.Tests.deleteCharsBeforeCaretWithSelection(InterfaceTests.TextArea);
        });
      });

      describe('insertTextBeforeCaret', function() {
        it("correctly replaces the element's 'context' (no active selection)", function() {
          InterfaceTests.Tests.insertTextBeforeCaretNoSelection(InterfaceTests.TextArea);
        });

        it("correctly replaces the element's 'context' (with active selection)", function() {
          InterfaceTests.Tests.insertTextBeforeCaretWithSelection(InterfaceTests.TextArea);
        });
      });
    });
  });
});