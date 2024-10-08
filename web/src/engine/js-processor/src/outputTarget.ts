import { extendString } from "@keymanapp/web-utils";
import { findCommonSubstringEndIndex } from "./stringDivergence.js";
import { Mock } from "./mock.js";

extendString();

// Defines deadkey management in a manner attachable to each element interface.
import { type KeyEvent } from 'keyman/engine/keyboard';
import { Deadkey, DeadkeyTracker } from "./deadkeys.js";
import { OutputTarget as OutputTargetInterface, ProbabilityMass, Transform } from '@keymanapp/common-types';

// Also relies on string-extensions provided by the web-utils package.

export function isEmptyTransform(transform: Transform) {
  if(!transform) {
    return true;
  }
  return transform.insert === '' && transform.deleteLeft === 0 && (transform.deleteRight ?? 0) === 0;
}

export class TextTransform implements Transform {
  readonly insert: string;
  readonly deleteLeft: number;
  readonly deleteRight: number;
  readonly erasedSelection: boolean;
  id: number;

  constructor(insert: string, deleteLeft: number, deleteRight: number, erasedSelection: boolean) {
    this.insert = insert;
    this.deleteLeft = deleteLeft;
    this.deleteRight = deleteRight;
    this.erasedSelection = erasedSelection;
  }

  public static readonly nil = new TextTransform('', 0, 0, false);
}

export class Transcription {
  readonly token: number;
  readonly keystroke: KeyEvent;
  readonly transform: TextTransform;
  alternates: Alternate[]; // constructed after the rest of the transcription.
  readonly preInput: Mock;

  private static tokenSeed: number = 0;

  constructor(keystroke: KeyEvent, transform: TextTransform, preInput: Mock, alternates?: Alternate[]/*, removedDks: Deadkey[], insertedDks: Deadkey[]*/) {
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

export default abstract class OutputTarget implements OutputTargetInterface {
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
  buildTransformFrom(original: OutputTarget): TextTransform {
    const toLeft = this.getTextBeforeCaret();
    const fromLeft = original.getTextBeforeCaret();

    const leftDivergenceIndex = findCommonSubstringEndIndex(fromLeft, toLeft, false);
    const deletedLeft = fromLeft.substring(leftDivergenceIndex)._kmwLength();
    // No need for our specialized variant here.
    const insertedText = toLeft.substring(leftDivergenceIndex);

    const toRight = this.getTextAfterCaret();
    const fromRight = original.getTextAfterCaret();
    const rightDivergenceIndex = findCommonSubstringEndIndex(fromRight, toRight, true);

    // Right insertions aren't supported, but right deletions will matter in some scenarios.
    // In particular, once we allow right-deletion for pred-text suggestions applied with the
    // caret mid-word..
    const deletedRight = fromRight.substring(0, rightDivergenceIndex + 1)._kmwLength();

    return new TextTransform(insertedText, deletedLeft, deletedRight, original.getSelectedText() && !this.getSelectedText());
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
    this.clearSelection();
    // We currently do not restore selected text; the mechanism isn't supported at present for
    // all output target types - especially in regard to re-selecting the text if restored.
    //
    // I believe this would mostly matter if/when reverting predictions based upon selected text.
    // That pattern isn't well-supported yet, though.

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
