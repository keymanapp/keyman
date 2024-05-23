import { assert } from 'chai';

import { extendString, Mock } from '@keymanapp/keyboard-processor';
import * as wrappers from 'keyman/engine/element-wrappers';

import { DynamicElements } from '../../test_utils.js';

extendString();

const host = document.createElement('div');
host.id = 'DynamicElements';
document.body.appendChild(host);

var InterfaceTests;

// Define common interface testing functions that can be run upon the OutputTarget interface.
if(typeof InterfaceTests == 'undefined') {
  InterfaceTests = {};

  (function(){
    // Makes a nice Unicode shortcut.
    var u = (code) => String.fromCodePoint(code);

    InterfaceTests.Strings = {};

    InterfaceTests.Strings.Apple = {};
    InterfaceTests.Strings.Apple.normal = '12345'; // 'apple' was a bad choice due to repeated 'p' masking failures!
    // Built in-line via function.  Looks functionally equivalent to "apple", but with SMP characters.
    InterfaceTests.Strings.Apple.smp = u(0x1d5ba)+u(0x1d5ca)+u(0x1d5c9)+u(0x1d5c5)+u(0x1d5be); // Note first 'p' is different to avoid repetition
    InterfaceTests.Strings.Apple.mixed = 'a'+u(0x1d5c9)+'p'+'l'+u(0x1d5be);

    //#region Defines helpers related to HTMLInputElement / Input test setup.
    InterfaceTests.Input = {};

    InterfaceTests.Input.setupElement = function() {
      var id = DynamicElements.addInput();
      var elem = document.getElementById(id) as HTMLInputElement;
      var wrapper = new wrappers.Input(elem);

      return {elem: elem, wrapper: wrapper};
    }

    InterfaceTests.Input.resetWithText = function(pair, string) {
      pair.elem.value = string;
      pair.wrapper.invalidateSelection();
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
      var elem = document.getElementById(id) as HTMLTextAreaElement;
      var wrapper = new wrappers.TextArea(elem);

      return {elem: elem, wrapper: wrapper};
    }

    InterfaceTests.TextArea.resetWithText = function(pair, string) {
      pair.elem.value = string;
      pair.wrapper.invalidateSelection();
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

    //#region Defines helpers related to ContentEditable element test setup.

    // These functions simply make the basic (within a single text node) tests
    // compatible with the more advanced element types; more complex tests may
    // be in order.  They can probably be shared with design-mode IFrames.
    InterfaceTests.ContentEditable = {};

    InterfaceTests.ContentEditable.setupElement = function() {
      var id = DynamicElements.addEditable();
      var elem = document.getElementById(id);
      var wrapper = new wrappers.ContentEditable(elem);

      return {elem: elem, wrapper: wrapper, node: null};
    }

    InterfaceTests.ContentEditable.setupDummyElement = function() {
      var id = DynamicElements.addEditable();
      var elem = document.getElementById(id);
      var wrapper = new wrappers.ContentEditable(elem);

      return {elem: elem, wrapper: wrapper, node: null};
    }

    InterfaceTests.ContentEditable.resetWithText = function(pair, string) {
      this.setText(pair, string);
      this.setSelectionRange(pair, 0, 0);
    }

    // Implemented for completeness and generality with other tests.
    InterfaceTests.ContentEditable.setCaret = function(pair, index) {
      this.setSelectionRange(pair, index, index);
    }

    // Implemented for completeness and generality with other tests.
    InterfaceTests.ContentEditable.getCaret = function(pair) {
      var sel = document.getSelection();

      if(sel.focusNode.compareDocumentPosition(pair.elem) == 16) { // Contained by
        return sel.focusOffset;
      } else {
        console.warn("Selection during test is in unexpected configuration!");
      }
    }

    InterfaceTests.ContentEditable.setSelectionRange = function(pair, start, end) {
      var node = pair.elem.childNodes[0];
      var sel = document.getSelection();

      var setIESelection = function(node, sel, start, end) {
        if(start > end) {
          // The Range API doesn't allow 'backward' configurations.
          var temp = end;
          end = start;
          start = temp;
        }

        var range = document.createRange();
        range.setStart(node, start);
        range.setEnd(node, end);

        sel.removeAllRanges();
        sel.addRange(range);
      }

      if(node.nodeType == 3) {
        sel.removeAllRanges();
        // Does not work on IE!
        try {
          sel.setPosition(node, start);
          sel.extend(node, end);
        } catch (e) {
          // Sometimes fails in Firefox during CI.  Not sure why.
          console.warn("Error occurred while setting Selection via setPosition/extend: " + e.toString());
          setIESelection(node, sel, start, end);
        }
      } else {
        console.warn("Problem detected when setting up a selection range for content-editables!");
        var range = document.createRange();
        range.setStart(node, start);
        range.setEnd(node, start);
        sel.removeAllRanges();
        sel.addRange(range);
      }
    }

    InterfaceTests.ContentEditable.setText = function(pair, text) {
      pair.elem.innerText = text;
    }
    //#endregion

    //#region Defines helpers related to design-mode IFrame test setup.

    // These functions simply make the basic (within a single text node) tests
    // compatible with the more advanced element types; more complex tests may
    // be in order.  They can probably be shared with ContentEditables.
    InterfaceTests.DesignIFrame = {};

    InterfaceTests.DesignIFrame.InitAsyncElements = function(done) {
      // DynamicElements.addDesignIFrame takes an async callback
      // triggered upon the IFrame's load.
      var obj = this;

      var id1 = DynamicElements.addDesignIFrame(function() {
        var id2 = DynamicElements.addDesignIFrame(function() {
          let elem1 = document.getElementById(id1) as HTMLIFrameElement;
          let elem2 = document.getElementById(id2) as HTMLIFrameElement;

          obj.mainPair =  {elem: elem1, wrapper: new wrappers.DesignIFrame(elem1), document: elem1.contentWindow.document};
          obj.dummyPair = {elem: elem2, wrapper: new wrappers.DesignIFrame(elem2), document: elem1.contentWindow.document};

          done();
        });
      });
    }

    InterfaceTests.DesignIFrame.setupElement = function() {
      return this.mainPair;
    }

    InterfaceTests.DesignIFrame.setupDummyElement = function() {
      return this.dummyPair;
    }

    InterfaceTests.DesignIFrame.resetWithText = function(pair, string) {
      this.setText(pair, string);
      this.setSelectionRange(pair, 0, 0);
    }

    // Implemented for completeness and generality with other tests.
    InterfaceTests.DesignIFrame.setCaret = function(pair, index) {
      this.setSelectionRange(pair, index, index);
    }

    // Implemented for completeness and generality with other tests.
    InterfaceTests.DesignIFrame.getCaret = function(pair) {
      var sel = pair.document.getSelection();

      if(sel.focusNode.compareDocumentPosition(pair.elem) == 16) { // Contained by
        return sel.focusOffset;
      } else {
        console.warn("Selection during test is in unexpected configuration!");
      }
    }

    InterfaceTests.DesignIFrame.setSelectionRange = function(pair, start, end) {
      var node = pair.document.documentElement.childNodes[0];
      var sel = pair.document.getSelection();
      var range;

      var setIESelection = function(node, sel, start, end) {
        if(start > end) {
          // The Range API doesn't allow 'backward' configurations.
          var temp = end;
          end = start;
          start = temp;
        }

        range = pair.document.createRange();
        range.setStart(node, start);
        range.setEnd(node, end);

        sel.removeAllRanges();
        sel.addRange(range);
      }

      if(node.nodeType == 3) {
        sel.removeAllRanges();
        // Does not work on IE!
        try {
          sel.setPosition(node, start);
          sel.extend(node, end);
        } catch (e) {
          // Sometimes fails in Firefox during CI.  Not sure why.
          console.warn("Error occurred while setting Selection via setPosition/extend: " + e.toString());
          setIESelection(node, sel, start, end);
        }
      } else {
        console.warn("Problem detected when setting up a selection range!");
        range = pair.document.createRange();
        range.setStart(node, start);
        range.setEnd(node, start);
        sel.removeAllRanges();
        sel.addRange(range);
      }
    }

    InterfaceTests.DesignIFrame.setText = function(pair, text) {
      pair.document.documentElement.innerText = text;
    }
    //#endregion

    //#region Defines helpers related to Mock test setup.
    InterfaceTests.Mock = {};

    InterfaceTests.Mock.setupElement = function() {
      return {wrapper: new Mock()};
    }

    InterfaceTests.Mock.resetWithText = function(pair, string) {
      pair.wrapper.text = string;
      pair.wrapper.selStart = 0;
      pair.wrapper.selEnd = 0;
    }

    // Implemented for completeness and generality with other tests.
    InterfaceTests.Mock.setCaret = function(pair, index) {
      InterfaceTests.Mock.setSelectionRange(pair, index, index);
    }

    // Implemented for completeness and generality with other tests.
    InterfaceTests.Mock.getCaret = function(pair) {
      return pair.wrapper.getDeadkeyCaret();
    }

    InterfaceTests.Mock.setSelectionRange = function(pair, start, end) {
      let convert = (index) => pair.wrapper.text.kmwCodeUnitToCodePoint(index);
      pair.wrapper.setSelection(convert(start), convert(end));
    }

    InterfaceTests.Mock.setText = function(pair, text) {
      pair.wrapper.text = text;
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
      assert.equal(pair.wrapper.getCaret(), 5, "Failed to choose the caret's initial position for forward-selections within a simple string");
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
      assert.equal(pair.wrapper.getCaret(), 5, "Failed to caret's initial position for forward-selections within an SMP string");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.smp);
      testObj.setSelectionRange(pair, 10, 6);
      assert.equal(pair.wrapper.getCaret(), 3, "Failed to caret's initial position for backward-selections within an SMP string");
      String.kmwEnableSupplementaryPlane(false);
    }

    InterfaceTests.Tests.getSelectedText = function(testObj) {
      var Apple = InterfaceTests.Strings.Apple;
      var pair = testObj.setupElement();

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      testObj.setSelectionRange(pair, 3, 5);
      assert.equal(pair.wrapper.getSelectedText(), '45', "Failed to properly retrieve selected text");
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      testObj.setSelectionRange(pair, 5, 3);
      assert.equal(pair.wrapper.getSelectedText(), '45', "Failed to properly retrieve reverse-direction selected text");
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      testObj.setSelectionRange(pair, 3, 3);
      assert.equal(pair.wrapper.getSelectedText(), '', "No text was selected, but some was returned");
      pair.wrapper.invalidateSelection();

      String.kmwEnableSupplementaryPlane(true);
      testObj.resetWithText(pair, Apple.smp);
      testObj.setSelectionRange(pair, 6, 10);
      assert.equal(pair.wrapper.getSelectedText(), Apple.smp.substring(6, 10), "Failed to properly retrieve selected SMP text");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();
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

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      testObj.setCaret(pair, 4);
      pair.wrapper.deleteCharsBeforeCaret(7);
      assert.equal(pair.wrapper.getText(), "5", "Bounds-check on deletion range failed; context improperly deleted.");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();
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

    InterfaceTests.Tests.getSelectionOwned = function(testObj) {
      var Apple = InterfaceTests.Strings.Apple;
      var pair = testObj.setupElement();

      // All we need is some basic sample text to get started.
      testObj.resetWithText(pair, Apple.mixed);

      testObj.setSelectionRange(pair, 0, 7);
      assert.isTrue(pair.wrapper.hasSelection(), "Failed to recognize ownership of full, forward-order selection.");

      testObj.setSelectionRange(pair, 7, 0);
      assert.isTrue(pair.wrapper.hasSelection(), "Failed to recognize ownership of full, backward-order selection.")

      testObj.setSelectionRange(pair, 1, 3);
      assert.isTrue(pair.wrapper.hasSelection(), "Failed to recognize ownership of partial, forward-order selection.");

      testObj.setSelectionRange(pair, 3, 1);
      assert.isTrue(pair.wrapper.hasSelection(), "Failed to recognize ownership of partial, backward-order selection.")
    }

    InterfaceTests.Tests.getSelectionUnowned = function(testObj) {
      var Apple = InterfaceTests.Strings.Apple;
      var pair = testObj.setupElement();
      var dummy = testObj.setupDummyElement();

      // All we need is some basic sample text to get started.
      testObj.resetWithText(pair, Apple.mixed);
      testObj.resetWithText(dummy, Apple.mixed);

      testObj.setSelectionRange(dummy, 0, 7);
      assert.isFalse(pair.wrapper.hasSelection(), "Falsely claimed ownership of full, forward-order selection.");

      testObj.setSelectionRange(dummy, 7, 0);
      assert.isFalse(pair.wrapper.hasSelection(), "Falsely claimed ownership of full, backward-order selection.")

      testObj.setSelectionRange(dummy, 1, 3);
      assert.isFalse(pair.wrapper.hasSelection(), "Falsely claimed ownership of partial, forward-order selection.");

      testObj.setSelectionRange(dummy, 3, 1);
      assert.isFalse(pair.wrapper.hasSelection(), "Falsely claimed ownership of partial, backward-order selection.")
    }

    InterfaceTests.Tests.deadkeyMaintenance = function(testObj) {
      // Rather than messing with keystroke rules, we can directly test the deadkey maintenance logic.
      var Apple = InterfaceTests.Strings.Apple;
      var pair = testObj.setupElement();

      String.kmwEnableSupplementaryPlane(true);

      testObj.resetWithText(pair, Apple.mixed);
      testObj.setCaret(pair, 1);
      pair.wrapper.insertDeadkeyBeforeCaret(1); // unmatched
      pair.wrapper.insertDeadkeyBeforeCaret(2); // matched (by next line)
      pair.wrapper.deadkeys().isMatch(1, 0, 2);
      testObj.setCaret(pair, 5); // True caret point:  4 (one SMP character before this)
      pair.wrapper.insertDeadkeyBeforeCaret(3); // unmatched

      testObj.setCaret(pair, 3); // After SMP character 1.

      pair.wrapper.deadkeys().deleteMatched();
      assert.equal(pair.wrapper.deadkeys().count(), 2, "Failed to delete 'matched' deadkey");

      // Failure here likely reflects incorrect logic in .getDeadkeyCaret()!
      assert.isTrue(pair.wrapper.deadkeys().isMatch(4, 0, 3), "Failed to correctly note position of deadkey after an SMP character!");
      pair.wrapper.deadkeys().resetMatched();

      pair.wrapper.deleteCharsBeforeCaret(1);
      assert.equal(pair.wrapper.deadkeys().count(), 2, "Erroneously deleted deadkey after pre-caret deletions");
      assert.isTrue(pair.wrapper.deadkeys().isMatch(1, 0, 1), "Failed to leave deadkey before caret unmoved after pre-caret deletions");
      assert.isTrue(pair.wrapper.deadkeys().isMatch(3, 0, 3), "Failed to properly adjust position of post-caret deadkey after pre-caret deletions");

      pair.wrapper.deadkeys().resetMatched();
      pair.wrapper.insertTextBeforeCaret(Apple.mixed);
      assert.isTrue(pair.wrapper.deadkeys().isMatch(1, 0, 1), "Failed to leave deadkey before caret unmoved after text insertion");
      assert.isTrue(pair.wrapper.deadkeys().isMatch(8, 0, 3), "Failed to properly adjust position of post-caret deadkey after text insertion");

      String.kmwEnableSupplementaryPlane(false);
    }

    //#endregion

  })();
}

describe('Element Input/Output Interfacing', function() {
  this.timeout(5000);

  before(function() {
    // Make sure the basic SMP extension hooks exist to prevent errors later.
    String.kmwEnableSupplementaryPlane(false);
  });
  afterEach(function() {
    host.innerHTML = '';
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

      it('getSelectedText', function() {
        InterfaceTests.Tests.getSelectedText(InterfaceTests.Input);
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

      it('correctly maintains deadkeys', function() {
        InterfaceTests.Tests.deadkeyMaintenance(InterfaceTests.Input);
      });
    });
  });

  // TODO:  (if possible) Implement support for scroll-restoration checks.
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

      it('getSelectedText', function() {
        InterfaceTests.Tests.getSelectedText(InterfaceTests.Input);
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

      it('correctly maintains deadkeys', function() {
        InterfaceTests.Tests.deadkeyMaintenance(InterfaceTests.TextArea);
      });
    });
  });

  /**
   * TODO:  Design and implement some 'complex', cross-Node selection tests.
   */
  describe('Wrapper: Content-Editable Elements (using DIVs)', function() {
    describe('Caret Handling', function() {
      describe('hasSelection', function() {
        it('correctly recognizes Selection ownership', function () {
          InterfaceTests.Tests.getSelectionOwned(InterfaceTests.ContentEditable);
        });

        it('correctly rejects lack of Selection ownership', function () {
          InterfaceTests.Tests.getSelectionUnowned(InterfaceTests.ContentEditable);
        });

        // Need to design a test (and necessary helpers!) for a 'partial ownership rejection' test.
      });
    });

    describe('Text Retrieval', function(){
      describe('getText', function() {
        it('correctly returns text (no active selection)', function() {
          InterfaceTests.Tests.getTextNoSelection(InterfaceTests.ContentEditable);
        });

        it('correctly returns text (with active selection)', function() {
          InterfaceTests.Tests.getTextWithSelection(InterfaceTests.ContentEditable);
        });
      });

      describe('getTextBeforeCaret', function() {
        it('correctly returns text (no active selection)', function() {
          InterfaceTests.Tests.getTextBeforeCaretNoSelection(InterfaceTests.ContentEditable);
        });

        it('correctly returns text (with active selection)', function() {
          InterfaceTests.Tests.getTextBeforeCaretWithSelection(InterfaceTests.ContentEditable);
        });
      });

      it.skip('getSelectedText', function() { // Not yet properly supported.
        InterfaceTests.Tests.getSelectedText(InterfaceTests.Input);
      });

      describe('getTextAfterCaret', function() {
        it('correctly returns text (no active selection)', function() {
          InterfaceTests.Tests.getTextAfterCaretNoSelection(InterfaceTests.ContentEditable);
        });

        it('correctly returns text (with active selection)', function() {
          InterfaceTests.Tests.getTextAfterCaretWithSelection(InterfaceTests.ContentEditable);
        });
      });
    });

    describe('Text Mutation', function() {
      describe('clearSelection', function() {
        it('properly deletes selected text', function() {
          InterfaceTests.Tests.clearSelection(InterfaceTests.ContentEditable);
        });
      });

      describe('deleteCharsBeforeCaret', function() {
        it("correctly deletes characters from 'context' (no active selection)", function() {
          InterfaceTests.Tests.deleteCharsBeforeCaretNoSelection(InterfaceTests.ContentEditable);
        });

        it("correctly deletes characters from 'context' (with active selection)", function() {
          InterfaceTests.Tests.deleteCharsBeforeCaretWithSelection(InterfaceTests.ContentEditable);
        });
      });

      describe('insertTextBeforeCaret', function() {
        it("correctly replaces the element's 'context' (no active selection)", function() {
          InterfaceTests.Tests.insertTextBeforeCaretNoSelection(InterfaceTests.ContentEditable);
        });

        it("correctly replaces the element's 'context' (with active selection)", function() {
          InterfaceTests.Tests.insertTextBeforeCaretWithSelection(InterfaceTests.ContentEditable);
        });
      });

      it('correctly maintains deadkeys', function() {
        InterfaceTests.Tests.deadkeyMaintenance(InterfaceTests.ContentEditable);
      });
    });
  });

  /**
   * TODO:  Design and implement some 'complex', cross-doc selection tests if possible.
   */
  describe('Wrapper: Design-Mode IFrames', function() {
    // We're asynchronously loading IFrames, and sequentially at that.
    // We'll need a larger timeout.
    this.timeout(5000);

    beforeEach(function(done) {
      // Per-test creation of reg. pair and dummy elements, since IFrames are async.
      // Relies on the main-level's renewal of the overall fixture to be processed first.
      InterfaceTests.DesignIFrame.InitAsyncElements(done);
    });

    /**
     * Sadly, JavaScript appears to disallow programmatically setting an outer document's selection
     * to Nodes inside an iframe's document, which would be needed to properly emulate actual use-case
     * selections we'd want to test with these methods.  :(
     */
    // describe.skip('Caret Handling', function() {
    //   describe('hasSelection', function() {
    //     it('correctly recognizes Selection ownership', function () {
    //       InterfaceTests.Tests.getSelectionOwned(InterfaceTests.DesignIFrame);
    //     });

    //     it('correctly rejects lack of Selection ownership', function () {
    //       InterfaceTests.Tests.getSelectionUnowned(InterfaceTests.DesignIFrame);
    //     });

    //     // Need to design a test (and necessary helpers!) for a 'partial ownership rejection' test.
    //   });
    // });

    describe('Text Retrieval', function(){
      describe('getText', function() {
        it('correctly returns text (no active selection)', function() {
          InterfaceTests.Tests.getTextNoSelection(InterfaceTests.DesignIFrame);
        });

        it('correctly returns text (with active selection)', function() {
          InterfaceTests.Tests.getTextWithSelection(InterfaceTests.DesignIFrame);
        });
      });

      describe('getTextBeforeCaret', function() {
        it('correctly returns text (no active selection)', function() {
          InterfaceTests.Tests.getTextBeforeCaretNoSelection(InterfaceTests.DesignIFrame);
        });

        it('correctly returns text (with active selection)', function() {
          InterfaceTests.Tests.getTextBeforeCaretWithSelection(InterfaceTests.DesignIFrame);
        });
      });

      it.skip('getSelectedText', function() { // Not yet properly supported.
        InterfaceTests.Tests.getSelectedText(InterfaceTests.Input);
      });

      describe('getTextAfterCaret', function() {
        it('correctly returns text (no active selection)', function() {
          InterfaceTests.Tests.getTextAfterCaretNoSelection(InterfaceTests.DesignIFrame);
        });

        it('correctly returns text (with active selection)', function() {
          InterfaceTests.Tests.getTextAfterCaretWithSelection(InterfaceTests.DesignIFrame);
        });
      });
    });

    describe('Text Mutation', function() {
      describe('clearSelection', function() {
        it('properly deletes selected text', function() {
          InterfaceTests.Tests.clearSelection(InterfaceTests.DesignIFrame);
        });
      });

      describe('deleteCharsBeforeCaret', function() {
        it("correctly deletes characters from 'context' (no active selection)", function() {
          InterfaceTests.Tests.deleteCharsBeforeCaretNoSelection(InterfaceTests.DesignIFrame);
        });

        it("correctly deletes characters from 'context' (with active selection)", function() {
          InterfaceTests.Tests.deleteCharsBeforeCaretWithSelection(InterfaceTests.DesignIFrame);
        });
      });

      describe('insertTextBeforeCaret', function() {
        it("correctly replaces the element's 'context' (no active selection)", function() {
          InterfaceTests.Tests.insertTextBeforeCaretNoSelection(InterfaceTests.DesignIFrame);
        });

        it("correctly replaces the element's 'context' (with active selection)", function() {
          InterfaceTests.Tests.insertTextBeforeCaretWithSelection(InterfaceTests.DesignIFrame);
        });
      });

      it('correctly maintains deadkeys', function() {
        InterfaceTests.Tests.deadkeyMaintenance(InterfaceTests.DesignIFrame);
      });
    });
  });

  describe('The "Mock" output target', function() {
    // Unique to the Mock type - element interface cloning tests.  Is element state properly copied?
    // As those require a very different setup, they're in the target_mocks.js test case file instead.

    // Basic text-retrieval unit tests are now done headlessly in @keymanapp/keyboard-processor.

    describe('Text Mutation', function() {
      describe('deleteCharsBeforeCaret', function() {
        it("correctly deletes characters from 'context' (no active selection)", function() {
          InterfaceTests.Tests.deleteCharsBeforeCaretNoSelection(InterfaceTests.Mock);
        });

        it("correctly deletes characters from 'context' (with active selection)", function() {
          InterfaceTests.Tests.deleteCharsBeforeCaretWithSelection(InterfaceTests.Mock);
        });
      });

      describe('insertTextBeforeCaret', function() {
        it("correctly replaces the element's 'context' (no active selection)", function() {
          InterfaceTests.Tests.insertTextBeforeCaretNoSelection(InterfaceTests.Mock);
        });

        it("correctly replaces the element's 'context' (with active selection)", function() {
          InterfaceTests.Tests.insertTextBeforeCaretWithSelection(InterfaceTests.Mock);
        });
      });

      it('correctly maintains deadkeys', function() {
        InterfaceTests.Tests.deadkeyMaintenance(InterfaceTests.Mock);
      });
    });
  });
});