import { KMWString } from "keyman/common/web-utils";
import { Alternate, TextTransform } from "./keyboards/textTransform.js";
import { Transcription } from "./keyboards/transcription.js";
import { findCommonSubstringEndIndex } from "./stringDivergence.js";
import { SyntheticTextStore } from "./syntheticTextStore.js";

// Defines deadkey management in a manner attachable to each element interface.
import { type KeyEvent } from './keyEvent.js';
import { TextStoreLanguageProcessorInterface } from './textStoreLanguageProcessorInterface.js';
import { Deadkey, DeadkeyTracker } from "./deadkeys.js";
import { LexicalModelTypes } from '@keymanapp/common-types';

export abstract class TextStore {
  private _deadkeys: DeadkeyTracker;

  constructor() {
    this._deadkeys = new DeadkeyTracker();
  }

  /**
   * Signifies that this TextStore has no default key processing behaviors.  This should be false
   * for TextStores backed by web elements like HTMLInputElement or HTMLTextAreaElement.
   */
  get isSynthetic(): boolean {
    return true;
  }

  resetContext(): void {
    this.deadkeys().clear();
  }

  deadkeys(): DeadkeyTracker {
    return this._deadkeys;
  }

  hasDeadkeyMatch(n: number, d: number): boolean {
    return this.deadkeys().isMatch(this.getCaret(), n, d);
  }

  insertDeadkeyBeforeCaret(d: number) {
    const dk: Deadkey = new Deadkey(this.getCaret(), d);
    this.deadkeys().add(dk);
  }

  /**
   * Should be called by each textStore immediately before text mutation operations occur.
   *
   * Maintains solutions to old issues:  I3318,I3319
   * @param {number} delta  Use negative values if characters were deleted, positive if characters were added.
   */
  protected adjustDeadkeys(delta: number) {
    this.deadkeys().adjustPositions(this.getCaret(), delta);
  }

  /**
   * Needed to properly clone deadkeys for use with SyntheticTextStore element interfaces toward predictive text purposes.
   * @param {object}  dks   An existing set of deadkeys to deep-copy for use by this element interface.
   */
  protected setDeadkeys(dks: DeadkeyTracker) {
    this._deadkeys = dks.clone();
  }

  static assertIsTextStore(textStore: TextStoreLanguageProcessorInterface): asserts textStore is TextStore {
    if (!(textStore instanceof TextStore)) {
      throw new TypeError("textStore is not a TextStore");
    }
  }


  /**
   * Determines the basic operations needed to reconstruct the current TextStore's text from the prior state specified
   * by another TextStore based on their text and caret positions.
   *
   * This is designed for use as a "before and after" comparison to determine the effect of a single keyboard rule at a time.
   * As such, it assumes that the caret is immediately after any inserted text.
   * @param textStore   A text store (preferably a SyntheticTextStore) representing the prior state of the input/output system.
   */
  buildTransformFrom(textStore: TextStoreLanguageProcessorInterface): TextTransform {
    TextStore.assertIsTextStore(textStore);

    const toLeft = this.getTextBeforeCaret();
    const fromLeft = textStore.getTextBeforeCaret();

    const leftDivergenceIndex = findCommonSubstringEndIndex(fromLeft, toLeft, false);
    const deletedLeft = KMWString.length(fromLeft.substring(leftDivergenceIndex));
    // No need for our specialized variant here.
    const insertedText = toLeft.substring(leftDivergenceIndex);

    const toRight = this.getTextAfterCaret();
    const fromRight = textStore.getTextAfterCaret();
    const rightDivergenceIndex = findCommonSubstringEndIndex(fromRight, toRight, true);

    // Right insertions aren't supported, but right deletions will matter in some scenarios.
    // In particular, once we allow right-deletion for pred-text suggestions applied with the
    // caret mid-word..
    const deletedRight = KMWString.length(fromRight.substring(0, rightDivergenceIndex + 1));

    return new TextTransform(insertedText, deletedLeft, deletedRight, textStore.getSelectedText() && !this.getSelectedText());
  }

  buildTranscriptionFrom(original: TextStore, keyEvent: KeyEvent, readonly: boolean, alternates?: Alternate[]): Transcription {
    const transform = this.buildTransformFrom(original);

    // If we ever decide to re-add deadkey tracking, this is the place for it.

    return new Transcription(keyEvent, transform, SyntheticTextStore.from(original, readonly), alternates);
  }

  /**
   * Restores the `TextStore` to the indicated state.  Designed for use with `Transcription.preInput`.
   * @param original An `TextStore` (usually a `SyntheticTextStore`).
   */
  restoreTo(original: TextStore) {
    this.clearSelection();
    // We currently do not restore selected text; the mechanism isn't supported at present for
    // all TextStore types - especially in regard to re-selecting the text if restored.
    //
    // I believe this would mostly matter if/when reverting predictions based upon selected text.
    // That pattern isn't well-supported yet, though.

    //
    this.setTextBeforeCaret(original.getTextBeforeCaret());
    this.setTextAfterCaret(original.getTextAfterCaret());

    // Also, restore the deadkeys!
    // TODO-web-core: km_core_context_set or similar? (#15291)
    this.setDeadkeys(original._deadkeys);
  }

  apply(transform: LexicalModelTypes.Transform) {
    // Selected text should disappear on any text edit; application of a transform
    // certainly qualifies.
    this.clearSelection();

    if(transform.deleteRight) {
      this.setTextAfterCaret(KMWString.substr(this.getTextAfterCaret(), transform.deleteRight));
    }

    if(transform.deleteLeft) {
      this.deleteCharsBeforeCaret(transform.deleteLeft);
    }

    if(transform.insert) {
      this.insertTextBeforeCaret(transform.insert);
    }

    // We assume that all deadkeys are invalidated after applying a Transform, since
    // prediction implies we'll be completing a word, post-deadkeys.
    this.deadkeys().clear();
  }

  /**
   * Helper to `restoreTo` - allows directly setting the 'before' context to that of another
   * `TextStore`.
   * @param s
   */
  protected setTextBeforeCaret(s: string): void {
    // This one's easy enough to provide a default implementation for.
    this.deleteCharsBeforeCaret(KMWString.length(this.getTextBeforeCaret()));
    this.insertTextBeforeCaret(s);
  }

  /**
   * Helper to `restoreTo` - allows directly setting the 'after' context to that of another
   * `TextStore`.
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
   * Returns an index corresponding to the caret's position in the text store.
   */
  abstract getCaret(): number;

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
