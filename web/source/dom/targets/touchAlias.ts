// Defines the TouchAliasElement merged type.
/// <reference path="../touchAliasElement.ts" />

namespace com.keyman.dom.targets {
  export class TouchAlias extends OutputTarget {
    root: TouchAliasElement;

    constructor(e: TouchAliasElement) {
      super();
      this.root = e;
    }

    getElement(): HTMLDivElement {
      return this.root;
    }

    clearSelection(): void {
      if (this.hasSelection()) {
        let Lsel = this.root.ownerDocument.getSelection();

        if(!Lsel.isCollapsed) {
          Lsel.deleteFromDocument();  // I2134, I2192
        }
      }
      return;
    }

    invalidateSelection(): void {
      // Touch-alias elements do not currently support selections.
      return;
    }

    hasSelection(): boolean {
      let Lsel = this.root.ownerDocument.getSelection();
      let outerSel = document.getSelection();

      if (Lsel && outerSel && Lsel.toString().length > 0) {
        // If the outer doc's selection matches, we're active.
        // Both start and end elements need to be the same
        if(outerSel.anchorNode == Lsel.anchorNode && outerSel.focusNode == Lsel.focusNode &&
            Lsel.anchorNode == Lsel.focusNode) {
          return true;
        }
      }
      return false; // designIFrame returns true?
    }

    getDeadkeyCaret(): number {
      return this.root.getTextCaret();
    }

    getTextBeforeCaret(): string {
      if (this.hasSelection()) {
        // If there's selected text, use nul for context
        return '';
      }
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

    handleNewlineAtCaret(): void {
      // Insert new line in text area fields
      if(this.root.base.nodeName == 'TEXTAREA') {
        // As the TouchAliasElement was implemented long before OutputTargets, it already has
        // built-in newline handling.
        this.insertTextBeforeCaret('\n');
      } else if(dom.Utils.instanceof(this.root.base, "HTMLInputElement")) {
        // HTMLInputElements do not permit newlines; they instead have DOM-specific behaviors.
        this.root.hideCaret();
        Input.newlineHandler(this.root.base as HTMLInputElement);
      } else {
        console.warn("TouchAlias OutputTarget cannot output newlines for unexpected base element types!");
      }
    }

    protected setTextAfterCaret(s: string) {
      this.root.setText(this.getTextBeforeCaret() + s, this.getTextBeforeCaret()._kmwLength());
    }

    doInputEvent() {
      // Dispatch the event on the aliased element, not the TouchAliasElement itself.
      this.dispatchInputEventOn(this.root.base);
    }
  }
}