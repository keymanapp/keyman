// Defines KMW's string extension functions.
/// <reference path="../kmwstring.ts" />
// Defines a basic HTMLInputElement wrapper.
///<reference path="input.ts" />
// Defines a basic HTMLTextAreaElement wrapper.
///<reference path="textarea.ts" />
// Defines a basic content-editable wrapper.
///<reference path="contentEditable.ts" />
// Defines a basic design-mode IFrame wrapper.
///<reference path="designIFrame.ts" />
// Defines a basic touch-alias element wrapper.
///<reference path="touchAlias.ts" />
// Makes TS aware of Window-based type prototypes
///<reference path="../kmwexthtml.ts" />

namespace com.keyman.dom {
  export function wrapElement(e: HTMLElement): EditableElement {
    // Complex type scoping is implemented here so that kmwutils.ts is not a dependency for test compilations.
    // TODO:  Use dom.Utils.instanceof instead of the current mild code replication done here.

    if(Utils.instanceof(e, "HTMLInputElement")) {
      return new Input(<HTMLInputElement> e);
    } else if(Utils.instanceof(e, "HTMLTextAreaElement")) {
      return new TextArea(<HTMLTextAreaElement> e);
    } else if(Utils.instanceof(e, "TouchAliasElement")) {
      return new TouchAlias(<TouchAliasElement> e);
    } else if(Utils.instanceof(e, "HTMLIFrameElement")) {
      let iframe = <HTMLIFrameElement> e;

      if(iframe.contentWindow && iframe.contentWindow.document && iframe.contentWindow.document.designMode == "on") {
        return new DesignIFrame(iframe);
      } else if (e.isContentEditable) {
        // Do content-editable <iframe>s make sense?
        return new ContentEditable(e);
      } else {
        return null;
      }
    } else if(e.isContentEditable) {
      return new ContentEditable(e);
    }
    
    return null;
  }

  export interface EditableElement {
    /**
     * Returns the underlying element / document modeled by the wrapper.
     */
    getElement(): Element | Document;

    /**
     * Clears any selected text within the wrapper's element(s).
     * Silently does nothing if no such text exists.
     */
    clearSelection(): void;

    /**
     * Clears any cached selection-related state values.
     */
    invalidateSelection(): void;
    
    /**
     * Indicates whether or not the underlying element has its own selection (input, textarea)
     * or is part of (or possesses) the DOM's active selection.
     */
    hasSelection(): boolean;
    
    /**
     * Relative to the caret, gets the current context within the wrapper's element.
     */
    getTextBeforeCaret(): string;

    /**
     * Relative to the caret (and/or active selection), gets the element's text after the caret,
     * excluding any actively selected text that would be immediately replaced upon text entry.
     */
    getTextAfterCaret(): string;

    /**
     * Gets the element's full text, including any text that is actively selected.
     */
    getText(): string;

    /**
     * Performs context deletions (from the left of the caret) as needed by the KeymanWeb engine.
     * @param dn The number of characters to delete.  If negative, context will be left unchanged.
     */
    deleteCharsBeforeCaret(dn: number): void;

    /**
     * Inserts text immediately before the caret's current position, moving the caret after the
     * newly inserted text in the process.
     * 
     * @param s Text to insert before the caret's current position.
     */
    insertTextBeforeCaret(s: string): void;
  }
}