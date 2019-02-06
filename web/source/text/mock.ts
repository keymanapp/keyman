///<reference path="../dom/editableElement.ts" />

namespace com.keyman.text {
  export class Mock extends dom.EditableElement {
    text: string;
    caretIndex: number;

    constructor(text?: string, caretPos?: number) {
      super();

      this.text = text ? text : "";
      this.caretIndex = caretPos ? caretPos : 0;
    }

    // Clones the state of an existing EditableElement, creating a Mock version of its state.
    static from(outputTarget: dom.EditableElement) {
      let preText = outputTarget.getTextBeforeCaret();
      let caretIndex = preText._kmwLength();

      // We choose to ignore (rather, pre-emptively remove) any actively-selected text,
      // as since it's always removed instantly during any text mutation operations.
      let clone = new Mock(preText + outputTarget.getTextAfterCaret(), caretIndex);

      clone.setDeadkeys(outputTarget.deadkeys());

      return clone;
    }

    getElement(): HTMLElement {
      return null;
    }
    
    clearSelection(): void {
      return;
    }

    invalidateSelection(): void {
      return;
    }

    hasSelection(): boolean {
      return true;
    }

    getDeadkeyCaret(): number {
      return this.caretIndex;
    }

    getTextBeforeCaret(): string {
      return this.text.kmwSubstr(0, this.caretIndex);
    }

    getTextAfterCaret(): string {
      return this.text.kmwSubstr(this.caretIndex);
    }

    getText(): string {
      return this.text;
    }

    deleteCharsBeforeCaret(dn: number): void {
      if(dn >= 0) {
        this.text = this.text.kmwSubstr(0, this.caretIndex - dn) + this.getTextAfterCaret();
        this.caretIndex -= dn;
      }
    }

    insertTextBeforeCaret(s: string): void {
      this.text = this.getTextBeforeCaret() + s + this.getTextAfterCaret();
      this.caretIndex += s.kmwLength();
    }
  }
}