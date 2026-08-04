import { TextStore } from './textStore.js';
import { TextStoreLanguageProcessorInterface } from './textStoreLanguageProcessorInterface.js';
import { KMWString } from 'keyman/common/web-utils';

export class SyntheticTextStore extends TextStore {
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

  // Clones the state of an existing EditableElement, creating a SyntheticTextStore version of its state.
  static from(textStore: TextStoreLanguageProcessorInterface, readonly?: boolean): SyntheticTextStore {
    let clone: SyntheticTextStore;

    this.assertIsTextStore(textStore);

    if (textStore instanceof SyntheticTextStore) {
      // Avoids the need to run expensive kmwstring.ts `length()`
      // calculations when deep-copying SyntheticTextStore instances.
      const priorTextStore = textStore as SyntheticTextStore;
      clone = new SyntheticTextStore(priorTextStore.text, priorTextStore.selStart, priorTextStore.selEnd);
    } else {
      const text = textStore.getText();
      const textLen = KMWString.length(text);

      // If !hasSelection()
      let selectionStart: number = textLen;
      let selectionEnd: number = 0;

      if (textStore.hasSelection()) {
        const beforeText = textStore.getTextBeforeCaret();
        const afterText = textStore.getTextAfterCaret();
        selectionStart = KMWString.length(beforeText);
        selectionEnd = textLen - KMWString.length(afterText);
      }

      // readonly group or not, the returned SyntheticTextStore remains the same.
      // New-context events should act as if the caret were at the earlier-in-context
      // side of the selection, same as standard keyboard rules.
      clone = new SyntheticTextStore(text, selectionStart, selectionEnd);
    }

    // TODO-web-core: we need to support saving Core markers here (km_core_context_get / km_core_context_set) (#15291)
    // Also duplicate deadkey state!  (Needed for fat-finger ops.) (#15291)
    clone.setDeadkeys(textStore.deadkeys());

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

  getCaret(): number {
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
   * Indicates if this SyntheticTextStore represents an identical context to that of another SyntheticTextStore.
   * @param other
   * @returns
   */
  isEqual(other: SyntheticTextStore) {
    return this.text == other.text
      && this.selStart == other.selStart
      && this.selEnd == other.selEnd
      && this.deadkeys().equal(other.deadkeys());
  }

  doInputEvent() {
    // SyntheticTextStore isn't backed by an element, so it won't have any event listeners.
  }
}
