// Defines KMW's string extension functions.
///<reference path="../text/kmwstring.ts" />
// Makes TS aware of Window-based type prototypes
///<reference path="../kmwexthtml.ts" />
// Defines deadkey management in a manner attachable to each element interface.
///<reference path="../text/deadkeys.ts" />

namespace com.keyman.text {
  export abstract class OutputTarget {
    private _dks: text.DeadkeyTracker;

    constructor() {
      this._dks = new text.DeadkeyTracker();
    }

    deadkeys(): text.DeadkeyTracker {
      return this._dks;
    }

    hasDeadkeyMatch(n: number, d: number): boolean {
      return this.deadkeys().isMatch(this.getDeadkeyCaret(), n, d);
    }

    insertDeadkeyBeforeCaret(d: number) {
      var dk: Deadkey = new Deadkey(this.getDeadkeyCaret(), d);
      this.deadkeys().add(dk);
    }

    /**
     * Should be called by each output target immediately before text mutation operations occur.
     * 
     * Maintains solutions to old issues:  I3318,I3319
     * @param {number} delta  Use negative values if characters were deleted, positive if characters were added.
     */
    protected adjustDeadkeys(delta: number) {
      this.deadkeys().adjustPositions(this.getDeadkeyCaret(), delta); 
    }

    /**
     * Needed to properly clone deadkeys for use with Mock element interfaces toward predictive text purposes.
     * @param {object}  dks   An existing set of deadkeys to deep-copy for use by this element interface.
     */
    protected setDeadkeys(dks: text.DeadkeyTracker) {
      this._dks = dks.clone();
    }

    /**
     * Returns the underlying element / document modeled by the wrapper.
     */
    abstract getElement(): HTMLElement;

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
     * Performs context deletions (from the left of the caret) as needed by the KeymanWeb engine and
     * corrects the location of any affected deadkeys.
     * 
     * Does not delete deadkeys (b/c KMW 1 & 2 behavior maintenance).
     * @param dn The number of characters to delete.  If negative, context will be left unchanged.
     */
    abstract deleteCharsBeforeCaret(dn: number): void;

    /**
     * Inserts text immediately before the caret's current position, moving the caret after the
     * newly inserted text in the process along with any affected deadkeys.
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