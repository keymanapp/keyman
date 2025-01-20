import OutputTarget from './outputTarget.js';

export default class TextArea extends OutputTarget<{}> {
  root: HTMLTextAreaElement;

  /**
   * Tracks the most recently-cached selection start index.
   */
  private _cachedSelectionStart: number

  /**
   * Tracks the most recently processed, extended-string-based selection start index.
   * When the element's selectionStart value changes, this should be invalidated.
   */
  private processedSelectionStart: number;

  /**
   * Tracks the most recently processed, extended-string-based selection end index.
   * When the element's selectionEnd value changes, this should be invalidated.
   */
  private processedSelectionEnd: number;

  /**
   * Set, then unset within the `forceScroll` method in order to facilitate the
   * `isForcingScroll` flag.
   */
  private _activeForcedScroll: boolean;

  constructor(ele: HTMLTextAreaElement) {
    super();

    this.root = ele;
    this._cachedSelectionStart = -1;
  }

  get isSynthetic(): boolean {
    return false;
  }

  getElement(): HTMLTextAreaElement {
    return this.root;
  }

  clearSelection(): void {
    // Processes our codepoint-based variants of selectionStart and selectionEnd.
    this.getCaret(); // updates processedSelectionStart if required
    this.root.value = this.root.value._kmwSubstring(0, this.processedSelectionStart) + this.root.value._kmwSubstring(this.processedSelectionEnd); //I3319

    this.setCaret(this.processedSelectionStart);
  }

  isSelectionEmpty(): boolean {
    return this.root.selectionStart == this.root.selectionEnd;
  }

  hasSelection(): boolean {
    return true;
  }

  invalidateSelection() {
    // Since .selectionStart will never return this value, we use it to indicate
    // the need to refresh our processed indices.
    this._cachedSelectionStart = -1;
  }

  getCaret(): number {
    if(this.root.selectionStart != this._cachedSelectionStart) {
      this._cachedSelectionStart = this.root.selectionStart; // KMW-1
      this.processedSelectionStart = this.root.value._kmwCodeUnitToCodePoint(this.root.selectionStart); // I3319
      this.processedSelectionEnd = this.root.value._kmwCodeUnitToCodePoint(this.root.selectionEnd);     // I3319
    }
    return this.root.selectionDirection == 'forward' ? this.processedSelectionEnd : this.processedSelectionStart;
  }

  getDeadkeyCaret(): number {
    return this.getCaret();
  }

  setCaret(caret: number) {
    this.setSelection(caret, caret, "none");
  }

  setSelection(start: number, end: number, direction: "forward" | "backward" | "none") {
    let domStart = this.root.value._kmwCodePointToCodeUnit(start);
    let domEnd = this.root.value._kmwCodePointToCodeUnit(end);
    this.root.setSelectionRange(domStart, domEnd, direction);

    this.processedSelectionStart = start;
    this.processedSelectionEnd = end;

    this.forceScroll();

    this.root.setSelectionRange(domStart, domEnd, direction);
  }

  forceScroll() {
    // Only executes when com.keyman.DOMEventHandlers is defined.
    //
    // We bypass this whenever operating in the embedded format.
    const element = this.getElement();

    let selectionStart = element.selectionStart;
    let selectionEnd = element.selectionEnd;

    this._activeForcedScroll = true;

    try {
      //Forces scrolling; the re-focus triggers the scroll, at least.
      element.blur();
      element.focus();
    } finally {
      // On Edge, it appears that the blur/focus combination will reset the caret position
      // under certain scenarios during unit tests.  So, we re-set it afterward.
      element.selectionStart = selectionStart;
      element.selectionEnd = selectionEnd;
      this._activeForcedScroll = false;
    }
  }

  isForcingScroll(): boolean {
    return this._activeForcedScroll;
  }

  getSelectionDirection(): "forward" | "backward" | "none" {
    return this.root.selectionDirection;
  }

  getTextBeforeCaret(): string {
    this.getCaret();
    return this.getText()._kmwSubstring(0, this.processedSelectionStart);
  }

  setTextBeforeCaret(text: string) {
    this.getCaret();
    let selectionLength = this.processedSelectionEnd - this.processedSelectionStart;
    let direction = this.getSelectionDirection();
    let newCaret = text._kmwLength();
    this.root.value = text + this.getText()._kmwSubstring(this.processedSelectionStart);

    this.setSelection(newCaret, newCaret + selectionLength, direction);
  }

  protected setTextAfterCaret(s: string) {
    let direction = this.getSelectionDirection();

    this.root.value = this.getTextBeforeCaret() + s;
    this.setSelection(this.processedSelectionStart, this.processedSelectionEnd, direction);
  }

  getTextAfterCaret(): string {
    this.getCaret();
    return this.getText()._kmwSubstring(this.processedSelectionEnd);
  }

  getSelectedText(): string {
    this.getCaret();
    return this.getText()._kmwSubstring(this.processedSelectionStart, this.processedSelectionEnd);
  }

  getText(): string {
    return this.root.value;
  }

  deleteCharsBeforeCaret(dn: number) {
    if(dn > 0) {
      let curText = this.getTextBeforeCaret();
      let caret = this.processedSelectionStart;

      if(dn > caret) {
        dn = caret;
      }

      this.adjustDeadkeys(-dn);
      this.setTextBeforeCaret(curText.kmwSubstring(0, caret - dn));
      this.setCaret(caret - dn);
    }
  }

  insertTextBeforeCaret(s: string) {
    if(!s) {
      return;
    }

    let caret = this.getCaret();
    let front = this.getTextBeforeCaret();
    let back = this.getText()._kmwSubstring(this.processedSelectionStart);

    this.adjustDeadkeys(s._kmwLength());
    this.root.value = front + s + back;
    this.setCaret(caret + s._kmwLength());
  }

  handleNewlineAtCaret(): void {
    this.insertTextBeforeCaret('\n');
  }

  doInputEvent() {
    this.dispatchInputEventOn(this.root);
  }
}