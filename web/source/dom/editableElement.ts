// Defines KMW's string extension functions.
/// <reference path="../kmwstring.ts" />
// Makes TS aware of Window-based type prototypes
///<reference path="../kmwexthtml.ts" />
// Defines deadkey management in a manner attachable to each element interface.
///<reference path="../text/deadkeys.ts" />

namespace com.keyman.dom {
  export abstract class EditableElement {
    private _dks: text.DeadkeyTracker;

    constructor() {
      this._dks = new text.DeadkeyTracker();
    }

    getDeadkeys(): text.DeadkeyTracker {
      return this._dks;
    }

    /**
     * Returns the underlying element / document modeled by the wrapper.
     */
    abstract getElement(): Element;

    /**
     * Clears any selected text within the wrapper's element(s).
     * Silently does nothing if no such text exists.
     */
    abstract clearSelection(): void;

    /**
     * Clears any cached selection-related state values.
     */
    abstract invalidateSelection(): void;
    
    /**
     * Indicates whether or not the underlying element has its own selection (input, textarea)
     * or is part of (or possesses) the DOM's active selection.
     */
    abstract hasSelection(): boolean;

    /**
     * Returns an index corresponding to the caret's position for use with deadkeys.
     */
    abstract getDeadkeyCaret(): number;
    
    /**
     * Relative to the caret, gets the current context within the wrapper's element.
     */
    abstract getTextBeforeCaret(): string;

    /**
     * Relative to the caret (and/or active selection), gets the element's text after the caret,
     * excluding any actively selected text that would be immediately replaced upon text entry.
     */
    abstract getTextAfterCaret(): string;

    /**
     * Gets the element's full text, including any text that is actively selected.
     */
    abstract getText(): string;

    /**
     * Performs context deletions (from the left of the caret) as needed by the KeymanWeb engine.
     * @param dn The number of characters to delete.  If negative, context will be left unchanged.
     */
    abstract deleteCharsBeforeCaret(dn: number): void;

    /**
     * Inserts text immediately before the caret's current position, moving the caret after the
     * newly inserted text in the process.
     * 
     * @param s Text to insert before the caret's current position.
     */
    abstract insertTextBeforeCaret(s: string): void;

    /**
     * Saves element-specific state properties prone to mutation, enabling restoration after
     * text-output operations.
     */
    saveProperties() {
      // Most element interfaces won't need anything here.
    }

    /**
     * Restores previously-saved element-specific state properties.  Designed for use after text-output
     * ops to facilitate more-seamless web-dev and user interactions.
     */
    restoreProperties(){
      // Most element interfaces won't need anything here. 
    }
  }
}