// Defines the TouchAliasElement merged type.
/// <reference path="touchAliasElement.ts" />
// Defines the OutputTarget interface.
/// <reference path="../text/outputTarget.ts" />

namespace com.keyman.dom {
  export class TouchAlias extends text.OutputTarget {
    root: TouchAliasElement;

    constructor(e: TouchAliasElement) {
      super();
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

    getDeadkeyCaret(): number {
      return this.root.getTextCaret();
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
        if(this.getDeadkeyCaret() < dn) {
          dn = this.getDeadkeyCaret();
        }
        this.adjustDeadkeys(-dn);
        this.root.setTextBeforeCaret(curText.kmwSubstring(0, this.root.getTextCaret() - dn));
      }
    }
    
    insertTextBeforeCaret(s: string): void {
      this.adjustDeadkeys(s._kmwLength());
      this.root.setTextBeforeCaret(this.root.getTextBeforeCaret() + s);
    }

    protected setTextAfterCaret(s: string) {
      this.root.setText(this.getTextBeforeCaret() + s, this.getTextBeforeCaret()._kmwLength());
    }
  }
}