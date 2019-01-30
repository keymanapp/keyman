// Defines the TouchAliasElement merged type.
/// <reference path="touchAliasElement.ts" />
// Defines the EditableElement interface.
/// <reference path="editableElement.ts" />

namespace com.keyman.dom {
  export class TouchAlias implements EditableElement {
    root: TouchAliasElement;

    constructor(e: TouchAliasElement) {
      this.root = e;
    }

    getElement(): HTMLDivElement {
      return this.root;
    }

    clearSelection(): void {
      // Touch-alias elements do not currently support selections.
      return;
    }

    invalidateSelection(): void {
      // Touch-alias elements do not currently support selections.
      return;
    }

    hasSelection(): boolean {
      // Always has an internal caret position.
      return true;
    }

    getTextBeforeCaret(): string {
      return this.root.getTextBeforeCaret();
    }

    getTextAfterCaret(): string {
      return this.root.getTextAfterCaret();
    }

    getText(): string {
      return this.root.getText();
    }

    deleteCharsBeforeCaret(dn: number): void {
      if(dn > 0) {
        let curText = this.getTextBeforeCaret();
        this.root.setTextBeforeCaret(curText.kmwSubstring(0, this.root.getTextCaret() - dn));
      }
    }
    
    insertTextBeforeCaret(s: string): void {
      this.root.setTextBeforeCaret(this.root.getTextBeforeCaret() + s);
    }
  }
}