/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
export interface OutputTarget {
  /**
   * Signifies that this OutputTarget has no default key processing behaviors.  This should be false
   * for OutputTargets backed by web elements like HTMLInputElement or HTMLTextAreaElement.
   */
  get isSynthetic(): boolean;

  resetContext(): void;

  hasDeadkeyMatch(n: number, d: number): boolean;

  insertDeadkeyBeforeCaret(d: number): void;

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
   * or is part of (or possesses) the DOM's active selection. Don't confuse with isSelectionEmpty().
   *
   * TODO: rename to supportsOwnSelection
   */
  hasSelection(): boolean;

  /**
   * Returns true if there is no current selection -- that is, the selection range is empty
   */
  isSelectionEmpty(): boolean;

  /**
   * Returns an index corresponding to the caret's position for use with deadkeys.
   */
  getDeadkeyCaret(): number;

  /**
   * Relative to the caret, gets the current context within the wrapper's element.
   */
  getTextBeforeCaret(): string;

  /**
   * Gets the element's-currently selected text.
   */
  getSelectedText(): string;

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
   * Performs context deletions (from the left of the caret) as needed by the KeymanWeb engine and
   * corrects the location of any affected deadkeys.
   *
   * Does not delete deadkeys (b/c KMW 1 & 2 behavior maintenance).
   * @param dn The number of characters to delete.  If negative, context will be left unchanged.
   */
  deleteCharsBeforeCaret(dn: number): void;

  /**
   * Inserts text immediately before the caret's current position, moving the caret after the
   * newly inserted text in the process along with any affected deadkeys.
   *
   * @param s Text to insert before the caret's current position.
   */
  insertTextBeforeCaret(s: string): void;

  /**
   * Allows element-specific handling for ENTER key inputs.  Conceptually, this should usually
   * correspond to `insertTextBeforeCaret('\n'), but actual implementation will vary greatly among
   * elements.
   */
  handleNewlineAtCaret(): void;

  /**
   * Saves element-specific state properties prone to mutation, enabling restoration after
   * text-output operations.
   */
  saveProperties(): void;

  /**
   * Restores previously-saved element-specific state properties.  Designed for use after text-output
   * ops to facilitate more-seamless web-dev and user interactions.
   */
  restoreProperties(): void;

  /**
   * Generates a synthetic event on the underlying element, signalling that its value has changed.
   */
  doInputEvent(): void;
}
