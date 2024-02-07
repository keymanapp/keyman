///<reference types="@keymanapp/models-types" />

import { extendString } from "@keymanapp/web-utils";

extendString();

// Defines deadkey management in a manner attachable to each element interface.
import type KeyEvent from "./keyEvent.js";
import { Deadkey, DeadkeyTracker } from "./deadkeys.js";

// Also relies on string-extensions provided by the web-utils package.

export function isEmptyTransform(transform: Transform) {
  if(!transform) {
    return false;
  }
  return transform.insert === '' && transform.deleteLeft === 0 && (transform.deleteRight ?? 0) === 0;
}

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

export default abstract class OutputTarget {
  private _dks: DeadkeyTracker;

  constructor() {
    this._dks = new DeadkeyTracker();
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

  deadkeys(): DeadkeyTracker {
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
  protected setDeadkeys(dks: DeadkeyTracker) {
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
    let maxSMPLeftMatch = fromCaret < toCaret ? fromCaret : toCaret;

    // We need the corresponding non-SMP caret location in order to binary-search efficiently.
    // (Examining code units is much more computationally efficient.)
    let maxLeftMatch = to._kmwCodePointToCodeUnit(maxSMPLeftMatch);

    // 1.1:  use a non-SMP-aware binary search to determine the divergence point.
    let start = 0;
    let end = maxLeftMatch;  // the index AFTER the last possible matching char.

    // This search is O(maxLeftMatch).  1/2 + 1/4 + 1/8 + ... converges to = 1.
    while(start < end) {
      let mid = Math.floor((end+start+1) / 2); // round up (compare more)
      let fromLeft = from.substr(start, mid-start);
      let toLeft   = to.substr(start, mid-start);

      if(fromLeft == toLeft) {
        start = mid;
      } else {
        end = mid - 1;
      }
    }

    // At the loop's end:  `end` now holds the non-SMP-aware divergence point.
    // The 'caret' is after the last matching code unit.

    // 1.2:  detect a possible surrogate-pair split scenario, correcting for it
    //       (by moving the split before the high-surrogate) if detected.

    // If the split location is precisely on either end of the context, we can't
    // have split a surrogate pair.
    if(end > 0 && end < maxLeftMatch) {
      let potentialHigh    = from.charCodeAt(end-1);
      let potentialFromLow = from.charCodeAt(end);
      let potentialToLow   = to.charCodeAt(end);

      // if potentialHigh is a possible high surrogate...
      if(potentialHigh >= 0xD800 && potentialHigh <= 0xDBFF) {
        // and at least one potential 'low' is a possible low surrogate...
        let flag = potentialFromLow >= 0xDC00 && potentialFromLow <= 0xDFFF;
        flag = flag || (potentialToLow >= 0XDC00 && potentialToLow <= 0xDFFF);

        // Correct the split location, moving it 'before' the high surrogate.
        if(flag) {
          end = end - 1;
        }
      }
    }

    // 1.3:  take substring from start to the split point; determine SMP-aware length.
    //       This yields the SMP-aware divergence index, which gives the number of left-deletes.
    let newCaret = from._kmwCodeUnitToCodePoint(end);
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

  buildTranscriptionFrom(original: OutputTarget, keyEvent: KeyEvent, readonly: boolean, alternates?: Alternate[]): Transcription {
    let transform = this.buildTransformFrom(original);

    // If we ever decide to re-add deadkey tracking, this is the place for it.

    return new Transcription(keyEvent, transform, Mock.from(original, readonly), alternates);
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
    // Selected text should disappear on any text edit; application of a transform
    // certainly qualifies.
    this.clearSelection();

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
   * or is part of (or possesses) the DOM's active selection. Don't confuse with isSelectionEmpty().
   *
   * TODO: rename to supportsOwnSelection
   */
  abstract hasSelection(): boolean;

  /**
   * Returns true if there is no current selection -- that is, the selection range is empty
   */
  abstract isSelectionEmpty(): boolean;

  /**
   * Returns an index corresponding to the caret's position for use with deadkeys.
   */
  abstract getDeadkeyCaret(): number;

  /**
   * Relative to the caret, gets the current context within the wrapper's element.
   */
  abstract getTextBeforeCaret(): string;

  /**
   * Gets the element's-currently selected text.
   */
  abstract getSelectedText(): string;

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

  selStart: number;
  selEnd: number;
  selForward: boolean = true;

  constructor(text?: string, caretPos?: number);
  constructor(text?: string, selStart?: number, selEnd?: number);
  constructor(text?: string, selStart?: number, selEnd?: number) {
    super();

    this.text = text ? text : "";
    var defaultLength = this.text._kmwLength();

    // Ensures that `caretPos == 0` is handled correctly.
    this.selStart = typeof selStart == "number" ? selStart : defaultLength;

    // If no selection-end is set, selection length is implied to be 0.
    this.selEnd = typeof selEnd == "number" ? selEnd : this.selStart;

    this.selForward = this.selEnd >= this.selStart;
  }

  // Clones the state of an existing EditableElement, creating a Mock version of its state.
  static from(outputTarget: OutputTarget, readonly?: boolean) {
    let clone: Mock;

    if(outputTarget instanceof Mock) {
      // Avoids the need to run expensive kmwstring.ts / `_kmwLength()`
      // calculations when deep-copying Mock instances.
      let priorMock = outputTarget as Mock;
      clone = new Mock(priorMock.text, priorMock.selStart, priorMock.selEnd);
    } else {
      const text = outputTarget.getText();
      const textLen = text._kmwLength();

      // If !hasSelection()
      let selectionStart: number = textLen;
      let selectionEnd: number = 0;

      if(outputTarget.hasSelection()) {
        let beforeText = outputTarget.getTextBeforeCaret();
        let afterText = outputTarget.getTextAfterCaret();
        selectionStart = beforeText._kmwLength();
        selectionEnd = textLen - afterText._kmwLength();
      }

      // readonly group or not, the returned Mock remains the same.
      // New-context events should act as if the caret were at the earlier-in-context
      // side of the selection, same as standard keyboard rules.
      clone = new Mock(text, selectionStart, selectionEnd);
    }

    // Also duplicate deadkey state!  (Needed for fat-finger ops.)
    clone.setDeadkeys(outputTarget.deadkeys());

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

  getDeadkeyCaret(): number {
    return this.selStart;
  }

  setSelection(start: number, end?: number) {
    this.selStart = start;
    this.selEnd = typeof end == 'number' ? end : start;

    this.selForward = end >= start;
    if(!this.selForward) {
      let temp = this.selStart;
      this.selStart = this.selEnd;
      this.selEnd = temp;
    }
  }

  getTextBeforeCaret(): string {
    return this.text.kmwSubstr(0, this.selStart);
  }

  getSelectedText(): string {
    return this.text.kmwSubstr(this.selStart, this.selEnd - this.selStart);
  }

  getTextAfterCaret(): string {
    return this.text.kmwSubstr(this.selEnd);
  }

  getText(): string {
    return this.text;
  }

  deleteCharsBeforeCaret(dn: number): void {
    if(dn >= 0) {
      if(dn > this.selStart) {
        dn = this.selStart;
      }
      this.adjustDeadkeys(-dn);
      this.text = this.text.kmwSubstr(0, this.selStart - dn) + this.text.kmwSubstr(this.selStart);
      this.selStart -= dn;
      this.selEnd -= dn;
    }
  }

  insertTextBeforeCaret(s: string): void {
    this.adjustDeadkeys(s._kmwLength());
    this.text = this.getTextBeforeCaret() + s + this.text.kmwSubstr(this.selStart);
    this.selStart += s.kmwLength();
    this.selEnd += s.kmwLength();
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