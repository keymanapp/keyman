import OutputTarget from './outputTarget.js';

export class Mock extends OutputTarget {
  text: string;

  selStart: number;
  selEnd: number;
  selForward: boolean = true;

  constructor(text?: string, caretPos?: number);
  constructor(text?: string, selStart?: number, selEnd?: number);
  constructor(text?: string, selStart?: number, selEnd?: number) {
    super();

    this.text = text ? text : "";
    var defaultLength = this.text._kmwLength();

    // Ensures that `caretPos == 0` is handled correctly.
    this.selStart = typeof selStart == "number" ? selStart : defaultLength;

    // If no selection-end is set, selection length is implied to be 0.
    this.selEnd = typeof selEnd == "number" ? selEnd : this.selStart;

    this.selForward = this.selEnd >= this.selStart;
  }

  // Clones the state of an existing EditableElement, creating a Mock version of its state.
  static from(outputTarget: OutputTarget, readonly?: boolean) {
    let clone: Mock;

    if (outputTarget instanceof Mock) {
      // Avoids the need to run expensive kmwstring.ts / `_kmwLength()`
      // calculations when deep-copying Mock instances.
      let priorMock = outputTarget as Mock;
      clone = new Mock(priorMock.text, priorMock.selStart, priorMock.selEnd);
    } else {
      const text = outputTarget.getText();
      const textLen = text._kmwLength();

      // If !hasSelection()
      let selectionStart: number = textLen;
      let selectionEnd: number = 0;

      if (outputTarget.hasSelection()) {
        let beforeText = outputTarget.getTextBeforeCaret();
        let afterText = outputTarget.getTextAfterCaret();
        selectionStart = beforeText._kmwLength();
        selectionEnd = textLen - afterText._kmwLength();
      }

      // readonly group or not, the returned Mock remains the same.
      // New-context events should act as if the caret were at the earlier-in-context
      // side of the selection, same as standard keyboard rules.
      clone = new Mock(text, selectionStart, selectionEnd);
    }

    // Also duplicate deadkey state!  (Needed for fat-finger ops.)
    clone.setDeadkeys(outputTarget.deadkeys());

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
      let temp = this.selStart;
      this.selStart = this.selEnd;
      this.selEnd = temp;
    }
  }

  getTextBeforeCaret(): string {
    return this.text.kmwSubstr(0, this.selStart);
  }

  getSelectedText(): string {
    return this.text.kmwSubstr(this.selStart, this.selEnd - this.selStart);
  }

  getTextAfterCaret(): string {
    return this.text.kmwSubstr(this.selEnd);
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
      this.text = this.text.kmwSubstr(0, this.selStart - dn) + this.text.kmwSubstr(this.selStart);
      this.selStart -= dn;
      this.selEnd -= dn;
    }
  }

  insertTextBeforeCaret(s: string): void {
    this.adjustDeadkeys(s._kmwLength());
    this.text = this.getTextBeforeCaret() + s + this.text.kmwSubstr(this.selStart);
    this.selStart += s.kmwLength();
    this.selEnd += s.kmwLength();
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
