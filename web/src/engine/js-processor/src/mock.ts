import { OutputTargetInterface } from 'keyman/engine/keyboard';
import { OutputTargetBase } from './outputTargetBase.js';
import { KMWString } from '@keymanapp/web-utils';

export class Mock extends OutputTargetBase {
  text: string;

  selStart: number;
  selEnd: number;
  selForward: boolean = true;

  constructor(text?: string, caretPos?: number);
  constructor(text?: string, selStart?: number, selEnd?: number);
  constructor(text?: string, selStart?: number, selEnd?: number) {
    super();

    this.text = text ? text : "";
    const defaultLength = KMWString.length(this.text);

    // Ensures that `caretPos == 0` is handled correctly.
    this.selStart = typeof selStart == "number" ? selStart : defaultLength;

    // If no selection-end is set, selection length is implied to be 0.
    this.selEnd = typeof selEnd == "number" ? selEnd : this.selStart;

    this.selForward = this.selEnd >= this.selStart;
  }

  static assertIsOutputTargetBase(outputTarget: OutputTargetInterface): asserts outputTarget is OutputTargetBase {
    if (!(outputTarget instanceof OutputTargetBase)) {
      throw new TypeError("outputTarget is not a OutputTargetBase");
    }
  }

  // Clones the state of an existing EditableElement, creating a Mock version of its state.
  static from(outputTarget: OutputTargetInterface, readonly?: boolean): Mock {
    let clone: Mock;

    this.assertIsOutputTargetBase(outputTarget);

    if (outputTarget instanceof Mock) {
      // Avoids the need to run expensive kmwstring.ts `length()`
      // calculations when deep-copying Mock instances.
      const priorMock = outputTarget as Mock;
      clone = new Mock(priorMock.text, priorMock.selStart, priorMock.selEnd);
    } else {
      const text = outputTarget.getText();
      const textLen = KMWString.length(text);

      // If !hasSelection()
      let selectionStart: number = textLen;
      let selectionEnd: number = 0;

      if (outputTarget.hasSelection()) {
        const beforeText = outputTarget.getTextBeforeCaret();
        const afterText = outputTarget.getTextAfterCaret();
        selectionStart = KMWString.length(beforeText);
        selectionEnd = textLen - KMWString.length(afterText);
      }

      // readonly group or not, the returned Mock remains the same.
      // New-context events should act as if the caret were at the earlier-in-context
      // side of the selection, same as standard keyboard rules.
      clone = new Mock(text, selectionStart, selectionEnd);
    }

    // Also duplicate deadkey state!  (Needed for fat-finger ops.)
    clone.setDeadkeys((outputTarget as OutputTargetBase).deadkeys());

    return clone;
  }

  clearSelection(): void {
    this.text = this.getTextBeforeCaret() + this.getTextAfterCaret();
    this.selEnd = this.selStart;
    this.selForward = true;
  }

  invalidateSelection(): void {
    return;
  }

  isSelectionEmpty(): boolean {
    return this.selStart == this.selEnd;
  }

  hasSelection(): boolean {
    return true;
  }

  getDeadkeyCaret(): number {
    return this.selStart;
  }

  setSelection(start: number, end?: number) {
    this.selStart = start;
    this.selEnd = typeof end == 'number' ? end : start;

    this.selForward = end >= start;
    if (!this.selForward) {
      const temp = this.selStart;
      this.selStart = this.selEnd;
      this.selEnd = temp;
    }
  }

  getTextBeforeCaret(): string {
    return KMWString.substr(this.text, 0, this.selStart);
  }

  getSelectedText(): string {
    return KMWString.substr(this.text, this.selStart, this.selEnd - this.selStart);
  }

  getTextAfterCaret(): string {
    return KMWString.substr(this.text, this.selEnd);
  }

  getText(): string {
    return this.text;
  }

  deleteCharsBeforeCaret(dn: number): void {
    if (dn >= 0) {
      if (dn > this.selStart) {
        dn = this.selStart;
      }
      this.adjustDeadkeys(-dn);
      this.text = KMWString.substr(this.text, 0, this.selStart - dn) + KMWString.substr(this.text, this.selStart);
      this.selStart -= dn;
      this.selEnd -= dn;
    }
  }

  insertTextBeforeCaret(s: string): void {
    this.adjustDeadkeys(KMWString.length(s));
    this.text = this.getTextBeforeCaret() + s + KMWString.substr(this.text, this.selStart);
    this.selStart += KMWString.length(s);
    this.selEnd += KMWString.length(s);
  }

  handleNewlineAtCaret(): void {
    this.insertTextBeforeCaret('\n');
  }

  protected setTextAfterCaret(s: string): void {
    this.text = this.getTextBeforeCaret() + s;
  }

  /**
   * Indicates if this Mock represents an identical context to that of another Mock.
   * @param other
   * @returns
   */
  isEqual(other: Mock) {
    return this.text == other.text
      && this.selStart == other.selStart
      && this.selEnd == other.selEnd
      && this.deadkeys().equal(other.deadkeys());
  }

  doInputEvent() {
    // Mock isn't backed by an element, so it won't have any event listeners.
  }
}
