// Defines deadkey management in a manner attachable to each element interface.
///<reference path="../text/deadkeys.ts" />
// Defines the KeyEvent type.
///<reference path="keyEvent.ts" />

// Also relies on string-extensions provided by the web-utils package.

namespace com.keyman.text {
  export class TextTransform implements Transform {
    readonly insert: string;
    readonly deleteLeft: number;
    readonly deleteRight?: number;

    constructor(insert: string, deleteLeft: number, deleteRight?: number) {
      this.insert = insert;
      this.deleteLeft = deleteLeft;
      this.deleteRight = deleteRight || 0;
    }

    public static readonly nil = new TextTransform('', 0, 0);
  }

  export class Transcription {
    readonly token: number;
    readonly keystroke: KeyEvent;
    readonly transform: Transform;
    alternates: Alternate[]; // constructed after the rest of the transcription.
    readonly preInput: Mock;

    private static tokenSeed: number = 0;

    constructor(keystroke: KeyEvent, transform: Transform, preInput: Mock, alternates?: Alternate[]/*, removedDks: Deadkey[], insertedDks: Deadkey[]*/) {
      let token = this.token = Transcription.tokenSeed++;

      this.keystroke = keystroke;
      this.transform = transform;
      this.alternates = alternates;
      this.preInput = preInput;

      this.transform.id = this.token;

      // Assign the ID to each alternate, as well.
      if(alternates) {
        alternates.forEach(function(alt) {
          alt.sample.id = token;
        });
      }
    }
  }

  export type Alternate = ProbabilityMass<Transform>;

  export abstract class OutputTarget {
    private _dks: text.DeadkeyTracker;

    constructor() {
      this._dks = new text.DeadkeyTracker();
    }

    /**
     * Signifies that this OutputTarget has no default key processing behaviors.  This should be false
     * for OutputTargets backed by web elements like HTMLInputElement or HTMLTextAreaElement.
     */
    get isSynthetic(): boolean {
      return true;
    }

    resetContext(): void {
      this.deadkeys().clear();
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
     * Determines the basic operations needed to reconstruct the current OutputTarget's text from the prior state specified
     * by another OutputTarget based on their text and caret positions.
     * 
     * This is designed for use as a "before and after" comparison to determine the effect of a single keyboard rule at a time.
     * As such, it assumes that the caret is immediately after any inserted text.
     * @param from An output target (preferably a Mock) representing the prior state of the input/output system.
     */
    buildTransformFrom(original: OutputTarget): Transform {
      let to = this.getText();
      let from = original.getText();

      let fromCaret = original.getDeadkeyCaret();
      let toCaret = this.getDeadkeyCaret();

      // Step 1:  Determine the number of left-deletions.
      let maxLeftMatch = fromCaret < toCaret ? fromCaret : toCaret;
      for(var newCaret=0; newCaret < maxLeftMatch; newCaret++) {
        if(from._kmwCharAt(newCaret) != to._kmwCharAt(newCaret)) {
          break;
        }
      }

      let deletedLeft = fromCaret - newCaret;

      // Step 2:  Determine the other properties.
      // Since the 'after' OutputTarget's caret indicates the end of any inserted text, we
      // can easily calculate the rest.
      let insertedLength = toCaret - newCaret;
      let delta = to._kmwSubstr(newCaret, insertedLength);

      let undeletedRight = to._kmwLength() - toCaret;
      let originalRight = from._kmwLength() - fromCaret;
      let deletedRight = originalRight - undeletedRight;

      // May occur when reverting a suggestion that had been applied mid-word.
      if(deletedRight < 0) {
        // Restores deleteRight characters.
        delta = delta + to._kmwSubstr(toCaret, -deletedRight);
        deletedRight = 0;
      }

      return new TextTransform(delta, deletedLeft, deletedRight);
    }

    buildTranscriptionFrom(original: OutputTarget, keyEvent: KeyEvent, alternates?: Alternate[]): Transcription {
      let transform = this.buildTransformFrom(original);

      // If we ever decide to re-add deadkey tracking, this is the place for it.

      return new Transcription(keyEvent, transform, Mock.from(original), alternates);
    }

    /**
     * Restores the `OutputTarget` to the indicated state.  Designed for use with `Transcription.preInput`.
     * @param original An `OutputTarget` (usually a `Mock`).
     */
    restoreTo(original: OutputTarget) {
      //
      this.setTextBeforeCaret(original.getTextBeforeCaret());
      this.setTextAfterCaret(original.getTextAfterCaret());

      // Also, restore the deadkeys!
      this._dks = original._dks.clone();
    }

    apply(transform: Transform) {
      if(transform.deleteRight) {
        this.setTextAfterCaret(this.getTextAfterCaret()._kmwSubstr(transform.deleteRight));
      }

      if(transform.deleteLeft) {
        this.deleteCharsBeforeCaret(transform.deleteLeft);
      }

      if(transform.insert) {
        this.insertTextBeforeCaret(transform.insert);
      }

      // We assume that all deadkeys are invalidated after applying a Transform, since
      // prediction implies we'll be completing a word, post-deadkeys.
      this._dks.clear();
    }

    /**
     * Helper to `restoreTo` - allows directly setting the 'before' context to that of another
     * `OutputTarget`.
     * @param s 
     */
    protected setTextBeforeCaret(s: string): void {
      // This one's easy enough to provide a default implementation for.
      this.deleteCharsBeforeCaret(this.getTextBeforeCaret()._kmwLength());
      this.insertTextBeforeCaret(s);
    }

    /**
     * Helper to `restoreTo` - allows directly setting the 'after' context to that of another
     * `OutputTarget`.
     * @param s 
     */
    protected abstract setTextAfterCaret(s: string): void;

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
     * Allows element-specific handling for ENTER key inputs.  Conceptually, this should usually
     * correspond to `insertTextBeforeCaret('\n'), but actual implementation will vary greatly among
     * elements.
     */
    abstract handleNewlineAtCaret(): void;

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

    /**
     * Generates a synthetic event on the underlying element, signalling that its value has changed.
     */
    abstract doInputEvent(): void;
  }

  // Due to some interesting requirements on compile ordering in TS,
  // this needs to be in the same file as OutputTarget now.
  export class Mock extends OutputTarget {
    text: string;
    caretIndex: number;

    constructor(text?: string, caretPos?: number) {
      super();

      this.text = text ? text : "";
      var defaultLength = this.text._kmwLength();
      this.caretIndex = caretPos ? caretPos : defaultLength;
    }

    // Clones the state of an existing EditableElement, creating a Mock version of its state.
    static from(outputTarget: OutputTarget) {
      let preText = outputTarget.getTextBeforeCaret();
      let caretIndex = preText._kmwLength();

      // We choose to ignore (rather, pre-emptively remove) any actively-selected text,
      // as since it's always removed instantly during any text mutation operations.
      let clone = new Mock(preText + outputTarget.getTextAfterCaret(), caretIndex);

      clone.setDeadkeys(outputTarget.deadkeys());

      return clone;
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

    setDeadkeyCaret(index: number) {
      if(index < 0 || index > this.text._kmwLength()) {
        throw new Error("Provided caret index is out of range.");
      }
      this.caretIndex = index;
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
        if(dn > this.caretIndex) {
          dn = this.caretIndex;
        }
        this.text = this.text.kmwSubstr(0, this.caretIndex - dn) + this.getTextAfterCaret();
        this.caretIndex -= dn;
      }
    }

    insertTextBeforeCaret(s: string): void {
      this.text = this.getTextBeforeCaret() + s + this.getTextAfterCaret();
      this.caretIndex += s.kmwLength();
    }

    handleNewlineAtCaret(): void {
      this.insertTextBeforeCaret('\n');
    }

    protected setTextAfterCaret(s: string): void {
      this.text = this.getTextBeforeCaret() + s;
    }

    doInputEvent() {
      // Mock isn't backed by an element, so it won't have any event listeners.
    }
  }
}