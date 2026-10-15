import { AbstractElementTextStore } from './abstractElementTextStore.js';
import { KMWString } from 'keyman/common/web-utils';

interface EventMap  {
  /**
   * This event will be raised when a newline is received by wrapped elements not of
   * the 'search' or 'submit' types.
   *
   * Original code this is replacing:
   ```
      // Allows compiling this separately from the main body of KMW.
      // TODO:  rework class to accept a class-static 'callback' from the DOM module that this can call.
      //        Would eliminate the need for this 'static' reference.
      //        Only strongly matters once we better modularize KMW, with web-dom vs web-dom-targets vs web-core, etc.
      if(com.keyman["singleton"]) {
        com.keyman["singleton"].domManager.moveToNext(false);
      }
   ```
   * This does not belong in a modularized version of this class; it must be supplied
   * by the consuming top-level products instead.
   */
  'unhandlednewline': (element: HTMLInputElement) => void
}

export class InputElementTextStore extends AbstractElementTextStore<EventMap> {
  root: HTMLInputElement;

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

  constructor(ele: HTMLInputElement) {
    super();

    this.root = ele;
    this._cachedSelectionStart = -1;
  }

  get isSynthetic(): boolean {
    return false;
  }

  static isSupportedType(type: string): boolean {
    return type == 'email' || type == 'search' || type == 'text' || type == 'url';
  }

  getElement(): HTMLInputElement {
    return this.root;
  }

  clearSelection(): void {
    // Processes our codepoint-based variants of selectionStart and selectionEnd.
    this.getCaret(); // updates processedSelectionStart if required
    this.root.value = KMWString.substr(this.root.value, 0, this.processedSelectionStart) + KMWString.substr(this.root.value, this.processedSelectionEnd); //I3319

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
      this.processedSelectionStart = KMWString.codeUnitToCodePoint(this.root.value, this.root.selectionStart); // I3319
      this.processedSelectionEnd = KMWString.codeUnitToCodePoint(this.root.value, this.root.selectionEnd);     // I3319
    }
    return this.root.selectionDirection == 'forward' ? this.processedSelectionEnd : this.processedSelectionStart;
  }

  setCaret(caret: number) {
    this.setSelection(caret, caret, "none");
  }

  setSelection(start: number, end: number, direction: "forward" | "backward" | "none") {
    const domStart = KMWString.codePointToCodeUnit(this.root.value, start);
    const domEnd = KMWString.codePointToCodeUnit(this.root.value, end);
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

    const selectionStart = element.selectionStart;
    const selectionEnd = element.selectionEnd;

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
    return KMWString.substring(this.getText(), 0, this.processedSelectionStart);
  }

  getSelectedText(): string {
    this.getCaret();
    return KMWString.substring(this.getText(), this.processedSelectionStart, this.processedSelectionEnd);
  }

  setTextBeforeCaret(text: string) {
    this.getCaret();
    const selectionLength = this.processedSelectionEnd - this.processedSelectionStart;
    const direction = this.getSelectionDirection();
    const newCaret = KMWString.length(text);
    this.root.value = text + KMWString.substring(this.getText(), this.processedSelectionStart);

    this.setSelection(newCaret, newCaret + selectionLength, direction);
  }

  protected setTextAfterCaret(s: string) {
    const direction = this.getSelectionDirection();

    this.root.value = this.getTextBeforeCaret() + s;
    this.setSelection(this.processedSelectionStart, this.processedSelectionEnd, direction);
  }

  getTextAfterCaret(): string {
    this.getCaret();
    return KMWString.substring(this.getText(), this.processedSelectionEnd);
  }

  getText(): string {
    return this.root.value;
  }

  deleteCharsBeforeCaret(dn: number) {
    if(dn > 0) {
      const curText = this.getTextBeforeCaret();
      const caret = this.processedSelectionStart;

      if(dn > caret) {
        dn = caret;
      }

      this.adjustDeadkeys(-dn);
      this.setTextBeforeCaret(KMWString.substring(curText, 0, caret - dn));
      this.setCaret(caret - dn);
    }
  }

  insertTextBeforeCaret(s: string) {
    if(!s) {
      return;
    }

    const caret = this.getCaret();
    const front = this.getTextBeforeCaret();
    const back = KMWString.substring(this.getText(), this.processedSelectionStart);

    this.adjustDeadkeys(KMWString.length(s));
    this.root.value = front + s + back;
    this.setCaret(caret + KMWString.length(s));
  }

  handleNewlineAtCaret(): void {
    const inputEle = this.root;
    // Can't occur for SyntheticTextStores - just Input types.
    if (inputEle && (inputEle.type == 'search' || inputEle.type == 'submit')) {
      inputEle.disabled=false;
      inputEle.form.submit();
    } else {
      this.events.emit('unhandlednewline', inputEle);
    }
  }

  doInputEvent() {
    this.dispatchInputEventOn(this.root);
  }
}