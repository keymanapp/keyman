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
  delete: number,

  /**
   * The number of code units to delete to the right of the cursor.
   */
  deleteRight?: number,
}

/**
 * The context is the text surrounding the insertion point,
 * before any transforms are applied to the text buffer.
 */
interface Context {
  /**
   * Up to maxLeftContextCodeUnits code units of Unicode scalar value
   * (i. e., characters) to the left of the insertion point in the
   * buffer. If there is nothing to the left of the buffer, this returns
   * an empty string.
   */
  left: USVString;

  /**
   * Up to maxRightContextCodeUnits code units of Unicode scalar value
   * (i. e., characters) to the right of the insertion point in the
   * buffer. If there is nothing to the right of the buffer, this returns
   * an empty string.
   */
  right?: USVString;

  /**
   * Whether the insertion point is at the start of the buffer.
   */
  startOfBuffer: boolean;

  /**
   * Whether the insertion point is at the end of the buffer.
   */
  endOfBuffer: boolean;
}
