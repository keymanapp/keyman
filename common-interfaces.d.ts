/**
 * A JavaScript string with the restriction that it must only
 * contain Unicode scalar values.
 * 
 * This means that any lone high surrogate must be paired with
 * a low surrogate, if it exists. Lone surrogate code units are
 * forbidden.
 * 
 * See also: https://developer.mozilla.org/en-US/docs/Web/API/USVString
 */
type USVString = string;

/**
 * Describes a potential change to a text buffer.
 */
interface Transform {
  /**
   * The Unicode scalar values (i.e., characters) to be inserted at the
   * cursor position.
   *
   * Corresponds to `s` in com.keyman.KeyboardInterface.output.
   */
  insert: USVString;

  /**
   * The number of code units to delete to the left of the cursor.
   *
   * Corresponds to `dn` in com.keyman.KeyboardInterface.output.
   */
  delete: number;

  /**
   * The number of code units to delete to the right of the cursor.
   * Not available on all platforms.
   */
  deleteRight?: number;
}
