// Defines KMW's string extension functions.
/// <reference path="../kmwstring.ts" />

namespace com.keyman.dom {
  export class TextArea implements EditableElement {
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
     * Used to temporarily store the y-axis scroll coordinate.
     */
    private scrollTop?: number;

    /**
     * Used to temporarily store the x-axis scroll coordinate.
     */
    private scrollLeft?: number;

    constructor(ele: HTMLTextAreaElement) {
      this.root = ele;
      this._cachedSelectionStart = -1;
    }

    getElement(): HTMLTextAreaElement {
      return this.root;
    }

    clearSelection(): void {
      let Pelem = this.root;

      // Processes our codepoint-based variants of selectionStart and selectionEnd.
      let caret = this.getCaret();
      Pelem.value = Pelem.value._kmwSubstring(0, caret) + Pelem.value._kmwSubstring(this.processedSelectionEnd); //I3319

      this.setCaret(caret);
    }

    hasSelection(): boolean {
      return true;
    }

    invalidateSelection() {
      // Since .selectionStart will never return this value, we use it to indicate
      this._cachedSelectionStart = -1;
    }

    getCaret(): number {
      if(this.root.selectionStart == this._cachedSelectionStart) {
        return this.processedSelectionStart;
      } else {
        this._cachedSelectionStart = this.root.selectionStart; // KMW-1
        this.processedSelectionStart = this.root.value._kmwCodeUnitToCodePoint(this.root.selectionStart); // I3319
        this.processedSelectionEnd = this.root.value._kmwCodeUnitToCodePoint(this.root.selectionEnd);     // I3319

        return this.processedSelectionStart;
      }
    }

    setCaret(caret: number) {
      let domCaret = this.root.value._kmwCodePointToCodeUnit(caret);
      this.root.setSelectionRange(domCaret, domCaret);
      this.processedSelectionEnd = caret;
    }

    getTextBeforeCaret(): string {
      return this.getText()._kmwSubstring(0, this.getCaret());
    }

    setTextBeforeCaret(text: string) {
      let newCaret = text._kmwLength();
      this.root.value = text + this.getTextAfterCaret();

      this.setCaret(newCaret);
    }

    getTextAfterCaret(): string {
      this.getCaret();
      return this.getText()._kmwSubstring(0, this.processedSelectionEnd);
    }

    getText(): string {
      return this.root.value;
    }

    /**
     * Facilitates temporary caching of the TextArea's scroll position.
     */
    saveScroll() {
      this.scrollLeft = this.root.scrollLeft;
      this.scrollTop = this.root.scrollTop;
    }

    /**
     * Restores a previously-saved scroll position for the TextArea.
     */
    restoreScroll() {
      if(this.scrollLeft !== undefined) {
        this.root.scrollLeft = this.scrollLeft;
        this.root.scrollTop = this.scrollTop;

        delete this.scrollLeft;
        delete this.scrollTop;
      }
    }
  }
}