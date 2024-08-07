import { assert } from 'chai';

import { extendString, Mock } from 'keyman/engine/keyboard';
import * as wrappers from 'keyman/engine/element-wrappers';

import { DynamicElements } from '../../test_utils.js';
import { DEFAULT_BROWSER_TIMEOUT } from '@keymanapp/common-test-resources/test-timeouts.mjs';

extendString();

const host = document.createElement('div');
host.id = 'DynamicElements';
document.body.appendChild(host);

const u = (code: number) =>  String.fromCodePoint(code);

type ElementPair<T> = {
  elem: T,
  wrapper: any,
  node?: ChildNode,
  document?: Document,
};

type HTMLElementPair = ElementPair<HTMLElement>;
type InputElementPair = ElementPair<HTMLInputElement>;
type TextElementPair = ElementPair<HTMLTextAreaElement>;
type IFrameElementPair = ElementPair<HTMLIFrameElement>;

interface TestHelper {
  setupElement(): ElementPair<any>;
  resetWithText(pair: ElementPair<any>, string: string): void;
  setCaret(pair: ElementPair<any>, index: number): void;
  getCaret(pair: ElementPair<any>): number;
  setSelectionRange(pair: ElementPair<any>, start: number, end: number): void;
  setText(pair: ElementPair<any>, text: string): void;
}

//#region Defines helpers related to HTMLInputElement / Input test setup.
class InputTestHelper implements TestHelper {
  setupElement(): InputElementPair {
    const id = DynamicElements.addInput();
    const elem = document.getElementById(id) as HTMLInputElement;
    const wrapper = new wrappers.Input(elem);

    return { elem: elem, wrapper: wrapper };
  }

  resetWithText(pair: InputElementPair, string: string) {
    pair.elem.value = string;
    pair.wrapper.invalidateSelection();
    pair.elem.setSelectionRange(0, 0);
  }

  // Implemented for completeness and generality with other tests.
  setCaret(pair: InputElementPair, index: number) {
    pair.elem.setSelectionRange(index, index);
  }

  // Implemented for completeness and generality with other tests.
  getCaret(pair: InputElementPair) {
    return pair.elem.selectionStart;
  }

  setSelectionRange(pair: InputElementPair, start: number, end: number) {
    // For HTMLInputElements, `start` must be before `end` to create a proper range.
    let dir: 'forward' | 'backward' = 'forward';

    if (end < start) {
      dir = 'backward';
      const temp = end;
      end = start;
      start = temp;
    }

    pair.elem.setSelectionRange(start, end, dir);
  }

  setText(pair: InputElementPair, text: string) {
    pair.elem.value = text;
  }
}
//#endregion

//#region Defines helpers related to HTMLTextAreaElement / TextArea test setup.
class TextAreaTestHelper implements TestHelper {
  setupElement(): TextElementPair {
    const id = DynamicElements.addText();
    const elem = document.getElementById(id) as HTMLTextAreaElement;
    const wrapper = new wrappers.TextArea(elem);

    return { elem: elem, wrapper: wrapper };
  }

  resetWithText(pair: TextElementPair, string: string) {
    pair.elem.value = string;
    pair.wrapper.invalidateSelection();
    pair.elem.setSelectionRange(0, 0);
  }

  // Implemented for completeness and generality with other tests.
  setCaret(pair: TextElementPair, index: number) {
    pair.elem.setSelectionRange(index, index);
  }

  // Implemented for completeness and generality with other tests.
  getCaret(pair: TextElementPair) {
    return pair.elem.selectionStart;
  }

  setSelectionRange(pair: TextElementPair, start: number, end: number) {
    // For HTMLInputElements, `start` must be before `end` to create a proper range.
    let dir: 'forward' | 'backward' = 'forward';

    if (end < start) {
      dir = 'backward';
      const temp = end;
      end = start;
      start = temp;
    }

    pair.elem.setSelectionRange(start, end, dir);
  }

  setText(pair: TextElementPair, text: string) {
    pair.elem.value = text;
  }
}
//#endregion

//#region Defines helpers related to ContentEditable element test setup.

// These functions simply make the basic (within a single text node) tests
// compatible with the more advanced element types; more complex tests may
// be in order.  They can probably be shared with design-mode IFrames.
class ContentEditableTestHelper implements TestHelper {
  setupElement(): HTMLElementPair {
    const id = DynamicElements.addEditable();
    const elem = document.getElementById(id);
    const wrapper = new wrappers.ContentEditable(elem);

    return { elem: elem, wrapper: wrapper, node: null };
  }

  setupDummyElement(): HTMLElementPair {
    const id = DynamicElements.addEditable();
    const elem = document.getElementById(id);
    const wrapper = new wrappers.ContentEditable(elem);

    return { elem: elem, wrapper: wrapper, node: null };
  }

  resetWithText(pair: HTMLElementPair, string: string) {
    this.setText(pair, string);
    this.setSelectionRange(pair, 0, 0);
  }

  // Implemented for completeness and generality with other tests.
  setCaret(pair: HTMLElementPair, index: number) {
    this.setSelectionRange(pair, index, index);
  }

  // Implemented for completeness and generality with other tests.
  getCaret(pair: HTMLElementPair): number {
    const sel = document.getSelection();

    if (sel.focusNode.compareDocumentPosition(pair.elem) == 16) { // Contained by
      return sel.focusOffset;
    }

    console.warn("Selection during test is in unexpected configuration!");
    return 0;
  }

  setSelectionRange(pair: HTMLElementPair, start: number, end: number) {
    const node = pair.elem.childNodes[0];
    const sel = document.getSelection();

    const setIESelection = (node: ChildNode, sel: Selection, start: number, end: number) => {
      if (start > end) {
        // The Range API doesn't allow 'backward' configurations.
        const temp = end;
        end = start;
        start = temp;
      }

      const range = document.createRange();
      range.setStart(node, start);
      range.setEnd(node, end);

      sel.removeAllRanges();
      sel.addRange(range);
    }

    if (node.nodeType == 3) {
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
      const range = document.createRange();
      range.setStart(node, start);
      range.setEnd(node, start);
      sel.removeAllRanges();
      sel.addRange(range);
    }
  }

  setText(pair: HTMLElementPair, text: string) {
    pair.elem.innerText = text;
  }
}
//#endregion

//#region Defines helpers related to design-mode IFrame test setup.

// These functions simply make the basic (within a single text node) tests
// compatible with the more advanced element types; more complex tests may
// be in order.  They can probably be shared with ContentEditables.
class DesignIFrameTestHelper implements TestHelper {
  mainPair: IFrameElementPair;
  dummyPair: IFrameElementPair;

  InitAsyncElements(done: any) {
    // DynamicElements.addDesignIFrame takes an async callback
    // triggered upon the IFrame's load.
    const obj = this;

    const id1 = DynamicElements.addDesignIFrame(() => {
      const id2 = DynamicElements.addDesignIFrame(() => {
        const elem1 = document.getElementById(id1) as HTMLIFrameElement;
        const elem2 = document.getElementById(id2) as HTMLIFrameElement;

        obj.mainPair = { elem: elem1, wrapper: new wrappers.DesignIFrame(elem1), document: elem1.contentWindow.document };
        obj.dummyPair = { elem: elem2, wrapper: new wrappers.DesignIFrame(elem2), document: elem1.contentWindow.document };

        done();
      });
    });
  }

  setupElement(): IFrameElementPair {
    return this.mainPair;
  }

  setupDummyElement(): IFrameElementPair {
    return this.dummyPair;
  }

  resetWithText(pair: IFrameElementPair, string: string) {
    this.setText(pair, string);
    this.setSelectionRange(pair, 0, 0);
  }

  // Implemented for completeness and generality with other tests.
  setCaret(pair: IFrameElementPair, index: number) {
    this.setSelectionRange(pair, index, index);
  }

  // Implemented for completeness and generality with other tests.
  getCaret(pair: IFrameElementPair): number {
    const sel = pair.document.getSelection();

    if (sel.focusNode.compareDocumentPosition(pair.elem) == 16) { // Contained by
      return sel.focusOffset;
    }
    console.warn("Selection during test is in unexpected configuration!");
    return 0;
  }

  setSelectionRange(pair: IFrameElementPair, start: number, end: number) {
    const node = pair.document.documentElement.childNodes[0];
    const sel = pair.document.getSelection();
    let range;

    const setIESelection = (node: ChildNode, sel: Selection, start: number, end: number) => {
      if (start > end) {
        // The Range API doesn't allow 'backward' configurations.
        const temp = end;
        end = start;
        start = temp;
      }

      range = pair.document.createRange();
      range.setStart(node, start);
      range.setEnd(node, end);

      sel.removeAllRanges();
      sel.addRange(range);
    }

    if (node.nodeType == 3) {
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

  setText(pair: IFrameElementPair, text: string) {
    pair.document.documentElement.innerText = text;
  }
}
//#endregion

//#region Defines helpers related to Mock test setup.
class MockTestHelper implements TestHelper {
  setupElement(): ElementPair<any> {
    return { elem: null, wrapper: new Mock() };
  }

  resetWithText(pair: ElementPair<any>, string: string) {
    pair.wrapper.text = string;
    pair.wrapper.selStart = 0;
    pair.wrapper.selEnd = 0;
  }

  // Implemented for completeness and generality with other tests.
  setCaret(pair: ElementPair<any>, index: number) {
    this.setSelectionRange(pair, index, index);
  }

  // Implemented for completeness and generality with other tests.
  getCaret(pair: ElementPair<any>) {
    return pair.wrapper.getDeadkeyCaret();
  }

  setSelectionRange(pair: ElementPair<any>, start: number, end: number) {
    const convert = (index: number) => pair.wrapper.text.kmwCodeUnitToCodePoint(index);
    pair.wrapper.setSelection(convert(start), convert(end));
  }

  setText(pair: ElementPair<any>, text: string) {
    pair.wrapper.text = text;
  }
}
//#endregion

class InterfaceTests {
  public static Strings = {
    Apple: {
      normal: '12345',  // 'apple' was a bad choice due to repeated 'p' masking failures!
      // Built in-line via function.  Looks functionally equivalent to "apple", but with SMP characters.
      smp: u(0x1d5ba) + u(0x1d5ca) + u(0x1d5c9) + u(0x1d5c5) + u(0x1d5be), // Note first 'p' is different to avoid repetition
      mixed: 'a' + u(0x1d5c9) + 'p' + 'l' + u(0x1d5be),
    }
  };

  public static Input = new InputTestHelper();

  public static TextArea = new TextAreaTestHelper();

  public static ContentEditable = new ContentEditableTestHelper();

  public static DesignIFrame = new DesignIFrameTestHelper();

  public static Mock = new MockTestHelper();

  //#region Defines common test patterns across element tests
  public static Tests = class {
    // Corresponds to a helper method for certain classes.
    public static getCaretNoSelection(testObj: TestHelper) {
      const Apple = InterfaceTests.Strings.Apple;
      const pair = testObj.setupElement();

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
    public static getCaretWithSelection(testObj: TestHelper) {
      const Apple = InterfaceTests.Strings.Apple;
      const pair = testObj.setupElement();

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

    public static getSelectedText(testObj: TestHelper) {
      const Apple = InterfaceTests.Strings.Apple;
      const pair = testObj.setupElement();

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
    public static setCaretNoSelection(testObj: TestHelper) {
      const Apple = InterfaceTests.Strings.Apple;
      const pair = testObj.setupElement();

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
    public static setCaretWithSelection(testObj: TestHelper) {
      const Apple = InterfaceTests.Strings.Apple;
      const pair = testObj.setupElement();

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

    public static getTextNoSelection(testObj: TestHelper) {
      const Apple = InterfaceTests.Strings.Apple;
      const pair = testObj.setupElement();

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

    public static getTextWithSelection(testObj: TestHelper) {
      const Apple = InterfaceTests.Strings.Apple;
      const pair = testObj.setupElement();

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

    public static getTextBeforeCaretNoSelection(testObj: TestHelper) {
      const Apple = InterfaceTests.Strings.Apple;
      const pair = testObj.setupElement();

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

    public static getTextBeforeCaretWithSelection(testObj: TestHelper) {
      const Apple = InterfaceTests.Strings.Apple;
      const pair = testObj.setupElement();

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

    public static getTextAfterCaretNoSelection(testObj: TestHelper) {
      const Apple = InterfaceTests.Strings.Apple;
      const pair = testObj.setupElement();

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

    public static getTextAfterCaretWithSelection(testObj: TestHelper) {
      const Apple = InterfaceTests.Strings.Apple;
      const pair = testObj.setupElement();

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

    public static clearSelection(testObj: TestHelper) {
      const Apple = InterfaceTests.Strings.Apple;
      const pair = testObj.setupElement();

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
    public static setTextBeforeCaretNoSelection(testObj: TestHelper) {
      const Apple = InterfaceTests.Strings.Apple;
      const pair = testObj.setupElement();

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

    public static setTextBeforeCaretWithSelection(testObj: TestHelper) {
      const Apple = InterfaceTests.Strings.Apple;
      const pair = testObj.setupElement();

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

    public static deleteCharsBeforeCaretNoSelection(testObj: TestHelper) {
      const Apple = InterfaceTests.Strings.Apple;
      const pair = testObj.setupElement();

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

    public static deleteCharsBeforeCaretWithSelection(testObj: TestHelper) {
      const Apple = InterfaceTests.Strings.Apple;
      const pair = testObj.setupElement();

      String.kmwEnableSupplementaryPlane(false);
      testObj.resetWithText(pair, Apple.normal);
      testObj.setSelectionRange(pair, 2, 4);
      pair.wrapper.deleteCharsBeforeCaret(1);
      assert.equal(pair.wrapper.getText(), Apple.normal.substr(0, 1) + Apple.normal.substr(2), "Selected text erroneously deleted from selection");
      String.kmwEnableSupplementaryPlane(false);
      pair.wrapper.invalidateSelection();
    }

    public static insertTextBeforeCaretNoSelection(testObj: TestHelper) {
      const Apple = InterfaceTests.Strings.Apple;
      const pair = testObj.setupElement();

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

    public static insertTextBeforeCaretWithSelection(testObj: TestHelper) {
      const Apple = InterfaceTests.Strings.Apple;
      const pair = testObj.setupElement();

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

    public static getSelectionOwned(testObj: ContentEditableTestHelper) {
      const Apple = InterfaceTests.Strings.Apple;
      const pair = testObj.setupElement();

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

    public static getSelectionUnowned(testObj: ContentEditableTestHelper) {
      const Apple = InterfaceTests.Strings.Apple;
      const pair = testObj.setupElement();
      const dummy = testObj.setupDummyElement();

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

    public static deadkeyMaintenance(testObj: TestHelper) {
      // Rather than messing with keystroke rules, we can directly test the deadkey maintenance logic.
      const Apple = InterfaceTests.Strings.Apple;
      const pair = testObj.setupElement();

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

  }
}

describe('Element Input/Output Interfacing', function () {
  this.timeout(DEFAULT_BROWSER_TIMEOUT);

  before(function () {
    // Make sure the basic SMP extension hooks exist to prevent errors later.
    String.kmwEnableSupplementaryPlane(false);
  });
  afterEach(function () {
    host.innerHTML = '';
  });

  describe('Wrapper: HTMLInputElement', function () {
    /**
     * The design of these tests is to ensure that all caret handling works correctly,
     * independently of other methods.  Other tests will then rely upon these methods
     * to simplify their code.
     */
    describe('Caret Handling', function () {
      describe('setCaret', function () {
        it('correctly places the caret (no prior selection)', function () {
          InterfaceTests.Tests.setCaretNoSelection(InterfaceTests.Input);
        });

        it('correctly places the caret (prior selection)', function () {
          InterfaceTests.Tests.setCaretWithSelection(InterfaceTests.Input);
        });
      });

      describe('getCaret', function () {
        it('correctly reports the position of the caret (no selection)', function () {
          InterfaceTests.Tests.getCaretNoSelection(InterfaceTests.Input);
        });

        it('correctly reports the position of the caret (active selection)', function () {
          InterfaceTests.Tests.getCaretWithSelection(InterfaceTests.Input);
        });
      });
    });

    describe('Text Retrieval', function () {
      describe('getText', function () {
        it('correctly returns text (no active selection)', function () {
          InterfaceTests.Tests.getTextNoSelection(InterfaceTests.Input);
        });

        it('correctly returns text (with active selection)', function () {
          InterfaceTests.Tests.getTextWithSelection(InterfaceTests.Input);
        });
      });

      describe('getTextBeforeCaret', function () {
        it('correctly returns text (no active selection)', function () {
          InterfaceTests.Tests.getTextBeforeCaretNoSelection(InterfaceTests.Input);
        });

        it('correctly returns text (with active selection)', function () {
          InterfaceTests.Tests.getTextBeforeCaretWithSelection(InterfaceTests.Input);
        });
      });

      it('getSelectedText', function () {
        InterfaceTests.Tests.getSelectedText(InterfaceTests.Input);
      });

      describe('getTextAfterCaret', function () {
        it('correctly returns text (no active selection)', function () {
          InterfaceTests.Tests.getTextAfterCaretNoSelection(InterfaceTests.Input);
        });

        it('correctly returns text (with active selection)', function () {
          InterfaceTests.Tests.getTextAfterCaretWithSelection(InterfaceTests.Input);
        });
      });
    });

    describe('Text Mutation', function () {
      describe('clearSelection', function () {
        it('properly deletes selected text', function () {
          InterfaceTests.Tests.clearSelection(InterfaceTests.Input);
        });
      });

      describe('deleteCharsBeforeCaret', function () {
        it("correctly deletes characters from 'context' (no active selection)", function () {
          InterfaceTests.Tests.deleteCharsBeforeCaretNoSelection(InterfaceTests.Input);
        });

        it("correctly deletes characters from 'context' (with active selection)", function () {
          InterfaceTests.Tests.deleteCharsBeforeCaretWithSelection(InterfaceTests.Input);
        });
      });

      describe('insertTextBeforeCaret', function () {
        it("correctly replaces the element's 'context' (no active selection)", function () {
          InterfaceTests.Tests.insertTextBeforeCaretNoSelection(InterfaceTests.Input);
        });

        it("correctly replaces the element's 'context' (with active selection)", function () {
          InterfaceTests.Tests.insertTextBeforeCaretWithSelection(InterfaceTests.Input);
        });
      });

      it('correctly maintains deadkeys', function () {
        InterfaceTests.Tests.deadkeyMaintenance(InterfaceTests.Input);
      });
    });
  });

  // TODO:  (if possible) Implement support for scroll-restoration checks.
  describe('Wrapper: HTMLTextAreaElement', function () {
    /**
     * The design of these tests is to ensure that all caret handling works correctly,
     * independently of other methods.  Other tests will then rely upon these methods
     * to simplify their code.
     */
    describe('Caret Handling', function () {
      describe('setCaret', function () {
        it('correctly places the caret (no prior selection)', function () {
          InterfaceTests.Tests.setCaretNoSelection(InterfaceTests.TextArea);
        });

        it('correctly places the caret (prior selection)', function () {
          InterfaceTests.Tests.setCaretWithSelection(InterfaceTests.TextArea);
        });
      });

      describe('getCaret', function () {
        it('correctly reports the position of the caret (no selection)', function () {
          InterfaceTests.Tests.getCaretNoSelection(InterfaceTests.TextArea);
        });

        it('correctly reports the position of the caret (active selection)', function () {
          InterfaceTests.Tests.getCaretWithSelection(InterfaceTests.TextArea);
        });
      });
    });

    describe('Text Retrieval', function () {
      describe('getText', function () {
        it('correctly returns text (no active selection)', function () {
          InterfaceTests.Tests.getTextNoSelection(InterfaceTests.TextArea);
        });

        it('correctly returns text (with active selection)', function () {
          InterfaceTests.Tests.getTextWithSelection(InterfaceTests.TextArea);
        });
      });

      describe('getTextBeforeCaret', function () {
        it('correctly returns text (no active selection)', function () {
          InterfaceTests.Tests.getTextBeforeCaretNoSelection(InterfaceTests.TextArea);
        });

        it('correctly returns text (with active selection)', function () {
          InterfaceTests.Tests.getTextBeforeCaretWithSelection(InterfaceTests.TextArea);
        });
      });

      it('getSelectedText', function () {
        InterfaceTests.Tests.getSelectedText(InterfaceTests.Input);
      });

      describe('getTextAfterCaret', function () {
        it('correctly returns text (no active selection)', function () {
          InterfaceTests.Tests.getTextAfterCaretNoSelection(InterfaceTests.TextArea);
        });

        it('correctly returns text (with active selection)', function () {
          InterfaceTests.Tests.getTextAfterCaretWithSelection(InterfaceTests.TextArea);
        });
      });
    });

    describe('Text Mutation', function () {
      describe('clearSelection', function () {
        it('properly deletes selected text', function () {
          InterfaceTests.Tests.clearSelection(InterfaceTests.TextArea);
        });
      });

      describe('deleteCharsBeforeCaret', function () {
        it("correctly deletes characters from 'context' (no active selection)", function () {
          InterfaceTests.Tests.deleteCharsBeforeCaretNoSelection(InterfaceTests.TextArea);
        });

        it("correctly deletes characters from 'context' (with active selection)", function () {
          InterfaceTests.Tests.deleteCharsBeforeCaretWithSelection(InterfaceTests.TextArea);
        });
      });

      describe('insertTextBeforeCaret', function () {
        it("correctly replaces the element's 'context' (no active selection)", function () {
          InterfaceTests.Tests.insertTextBeforeCaretNoSelection(InterfaceTests.TextArea);
        });

        it("correctly replaces the element's 'context' (with active selection)", function () {
          InterfaceTests.Tests.insertTextBeforeCaretWithSelection(InterfaceTests.TextArea);
        });
      });

      it('correctly maintains deadkeys', function () {
        InterfaceTests.Tests.deadkeyMaintenance(InterfaceTests.TextArea);
      });
    });
  });

  /**
   * TODO:  Design and implement some 'complex', cross-Node selection tests.
   */
  describe('Wrapper: Content-Editable Elements (using DIVs)', function () {
    describe('Caret Handling', function () {
      describe('hasSelection', function () {
        it('correctly recognizes Selection ownership', function () {
          InterfaceTests.Tests.getSelectionOwned(InterfaceTests.ContentEditable);
        });

        it('correctly rejects lack of Selection ownership', function () {
          InterfaceTests.Tests.getSelectionUnowned(InterfaceTests.ContentEditable);
        });

        // Need to design a test (and necessary helpers!) for a 'partial ownership rejection' test.
      });
    });

    describe('Text Retrieval', function () {
      describe('getText', function () {
        it('correctly returns text (no active selection)', function () {
          InterfaceTests.Tests.getTextNoSelection(InterfaceTests.ContentEditable);
        });

        it('correctly returns text (with active selection)', function () {
          InterfaceTests.Tests.getTextWithSelection(InterfaceTests.ContentEditable);
        });
      });

      describe('getTextBeforeCaret', function () {
        it('correctly returns text (no active selection)', function () {
          InterfaceTests.Tests.getTextBeforeCaretNoSelection(InterfaceTests.ContentEditable);
        });

        it('correctly returns text (with active selection)', function () {
          InterfaceTests.Tests.getTextBeforeCaretWithSelection(InterfaceTests.ContentEditable);
        });
      });

      it.skip('getSelectedText', function () { // Not yet properly supported.
        InterfaceTests.Tests.getSelectedText(InterfaceTests.Input);
      });

      describe('getTextAfterCaret', function () {
        it('correctly returns text (no active selection)', function () {
          InterfaceTests.Tests.getTextAfterCaretNoSelection(InterfaceTests.ContentEditable);
        });

        it('correctly returns text (with active selection)', function () {
          InterfaceTests.Tests.getTextAfterCaretWithSelection(InterfaceTests.ContentEditable);
        });
      });
    });

    describe('Text Mutation', function () {
      describe('clearSelection', function () {
        it('properly deletes selected text', function () {
          InterfaceTests.Tests.clearSelection(InterfaceTests.ContentEditable);
        });
      });

      describe('deleteCharsBeforeCaret', function () {
        it("correctly deletes characters from 'context' (no active selection)", function () {
          InterfaceTests.Tests.deleteCharsBeforeCaretNoSelection(InterfaceTests.ContentEditable);
        });

        it("correctly deletes characters from 'context' (with active selection)", function () {
          InterfaceTests.Tests.deleteCharsBeforeCaretWithSelection(InterfaceTests.ContentEditable);
        });
      });

      describe('insertTextBeforeCaret', function () {
        it("correctly replaces the element's 'context' (no active selection)", function () {
          InterfaceTests.Tests.insertTextBeforeCaretNoSelection(InterfaceTests.ContentEditable);
        });

        it("correctly replaces the element's 'context' (with active selection)", function () {
          InterfaceTests.Tests.insertTextBeforeCaretWithSelection(InterfaceTests.ContentEditable);
        });
      });

      it('correctly maintains deadkeys', function () {
        InterfaceTests.Tests.deadkeyMaintenance(InterfaceTests.ContentEditable);
      });
    });
  });

  /**
   * TODO:  Design and implement some 'complex', cross-doc selection tests if possible.
   */
  describe('Wrapper: Design-Mode IFrames', function () {
    // We're asynchronously loading IFrames, and sequentially at that.
    // We'll need a larger timeout.
    this.timeout(DEFAULT_BROWSER_TIMEOUT);

    beforeEach(function (done) {
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

    describe('Text Retrieval', function () {
      describe('getText', function () {
        it('correctly returns text (no active selection)', function () {
          InterfaceTests.Tests.getTextNoSelection(InterfaceTests.DesignIFrame);
        });

        it('correctly returns text (with active selection)', function () {
          InterfaceTests.Tests.getTextWithSelection(InterfaceTests.DesignIFrame);
        });
      });

      describe('getTextBeforeCaret', function () {
        it('correctly returns text (no active selection)', function () {
          InterfaceTests.Tests.getTextBeforeCaretNoSelection(InterfaceTests.DesignIFrame);
        });

        it('correctly returns text (with active selection)', function () {
          InterfaceTests.Tests.getTextBeforeCaretWithSelection(InterfaceTests.DesignIFrame);
        });
      });

      it.skip('getSelectedText', function () { // Not yet properly supported.
        InterfaceTests.Tests.getSelectedText(InterfaceTests.Input);
      });

      describe('getTextAfterCaret', function () {
        it('correctly returns text (no active selection)', function () {
          InterfaceTests.Tests.getTextAfterCaretNoSelection(InterfaceTests.DesignIFrame);
        });

        it('correctly returns text (with active selection)', function () {
          InterfaceTests.Tests.getTextAfterCaretWithSelection(InterfaceTests.DesignIFrame);
        });
      });
    });

    describe('Text Mutation', function () {
      describe('clearSelection', function () {
        it('properly deletes selected text', function () {
          InterfaceTests.Tests.clearSelection(InterfaceTests.DesignIFrame);
        });
      });

      describe('deleteCharsBeforeCaret', function () {
        it("correctly deletes characters from 'context' (no active selection)", function () {
          InterfaceTests.Tests.deleteCharsBeforeCaretNoSelection(InterfaceTests.DesignIFrame);
        });

        it("correctly deletes characters from 'context' (with active selection)", function () {
          InterfaceTests.Tests.deleteCharsBeforeCaretWithSelection(InterfaceTests.DesignIFrame);
        });
      });

      describe('insertTextBeforeCaret', function () {
        it("correctly replaces the element's 'context' (no active selection)", function () {
          InterfaceTests.Tests.insertTextBeforeCaretNoSelection(InterfaceTests.DesignIFrame);
        });

        it("correctly replaces the element's 'context' (with active selection)", function () {
          InterfaceTests.Tests.insertTextBeforeCaretWithSelection(InterfaceTests.DesignIFrame);
        });
      });

      it('correctly maintains deadkeys', function () {
        InterfaceTests.Tests.deadkeyMaintenance(InterfaceTests.DesignIFrame);
      });
    });
  });

  describe('The "Mock" output target', function () {
    // Unique to the Mock type - element interface cloning tests.  Is element state properly copied?
    // As those require a very different setup, they're in the target_mocks.js test case file instead.

    // Basic text-retrieval unit tests are now done headlessly in keyman/engine/keyboard.

    describe('Text Mutation', function () {
      describe('deleteCharsBeforeCaret', function () {
        it("correctly deletes characters from 'context' (no active selection)", function () {
          InterfaceTests.Tests.deleteCharsBeforeCaretNoSelection(InterfaceTests.Mock);
        });

        it("correctly deletes characters from 'context' (with active selection)", function () {
          InterfaceTests.Tests.deleteCharsBeforeCaretWithSelection(InterfaceTests.Mock);
        });
      });

      describe('insertTextBeforeCaret', function () {
        it("correctly replaces the element's 'context' (no active selection)", function () {
          InterfaceTests.Tests.insertTextBeforeCaretNoSelection(InterfaceTests.Mock);
        });

        it("correctly replaces the element's 'context' (with active selection)", function () {
          InterfaceTests.Tests.insertTextBeforeCaretWithSelection(InterfaceTests.Mock);
        });
      });

      it('correctly maintains deadkeys', function () {
        InterfaceTests.Tests.deadkeyMaintenance(InterfaceTests.Mock);
      });
    });
  });
});