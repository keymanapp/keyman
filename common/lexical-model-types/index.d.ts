/**
 * TypeScript interfaces and types required in both within the LMLayer, and for
 * tools that create lexical models.
 */

/****************************** Lexical Models ******************************/

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
declare type USVString = string;

/**
 * Describes how to change a buffer at the cursor position.
 * first, you delete the specified amount amount from the left
 * and right, then you insert the provided text.
 */
declare interface Transform {
  /**
   * Facilitates use of unique identifiers for tracking the Transform and
   * any related data from its original source, as the reference cannot be
   * preserved across WebWorker boundaries.
   * 
   * This is *separate* from any LMLayer-internal identification values.
   */
  id?: number;

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
  deleteLeft: number;

  /**
   * The number of code units to delete to the right of the cursor.
   * Not available on all platforms.
   */
  deleteRight?: number;
}

/**
 * A concrete suggestion
 */
declare interface Suggestion {
  /**
   * Indicates the externally-supplied id of the Transform that prompted
   * the Suggestion.  Automatically handled by the LMLayer; models should
   * not handle this field.
   */
  transformId?: number;

  /**
   * The suggested update to the buffer. Note that this transform should
   * be applied AFTER the instigating transform, if any.
   */
  transform: Transform;

  /**
   * A string to display the suggestion to the typist.
   * This should aid the typist understand what the transform
   * will do to their text.
   * 
   * When suggesting a word, `displayAs` should be that entire word.
   */
  displayAs: string;

  /**
   * A single metalabel data describing the relation of the suggestion
   * to the input text.  Ex:  'keep', 'emoji', 'correction', etc.
   */
  // TODO: declare a concrete set of tags
  tag?: string;
}

/**
 * The text and environment surrounding the insertion point (text cursor).
 */
declare interface Context {
  /**
   * Up to maxLeftContextCodeUnits code units of Unicode scalar value
   * (i. e., characters) to the left of the insertion point in the
   * buffer. If there is nothing to the left of the buffer, this is
   * an empty string.
   */
  left: USVString;

  /**
   * Up to maxRightContextCodeUnits code units of Unicode scalar value
   * (i. e., characters) to the right of the insertion point in the
   * buffer. If there is nothing to the right of the buffer, this is
   * an empty string.
   * 
   * This property may be missing entirely.
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


/******************************** Messaging ********************************/

/**
 * Describes the capabilities of the keyboard's platform.
 * This includes upper bounds for how much text will be sent on each
 * prediction, as well as what operations the keyboard is allowed to do on the
 * underlying buffer.
 */
declare interface Capabilities {
  /**
   * The maximum amount of UTF-16 code units that the keyboard will provide to
   * the left of the cursor, as an integer.
   */
  maxLeftContextCodeUnits: number,

  /**
   * The maximum amount of code units that the keyboard will provide to the
   * right of the cursor, as an integer. The value 0 or the absence of this
   * rule implies that the right contexts are not supported.
   */
  maxRightContextCodeUnits?: number,

  /**
   * Whether the platform supports deleting to the right. The absence of this
   * rule implies false.
   */
  supportsDeleteRight?: false,
}

/**
 * Configuration of the LMLayer, sent back to the keyboard.
 */
declare interface Configuration {
  /**
   * How many UTF-16 code units maximum to send as the context to the
   * left of the cursor ("left" in the Unicode character stream).
   *
   * Affects the `context` property sent in `predict` messages.
   *
   * While the left context MUST NOT bisect surrogate pairs, they MAY
   * bisect graphical clusters.
   */
  leftContextCodeUnits: number;

  /**
   * How many UTF-16 code units maximum to send as the context to the
   * right of the cursor ("right" in the Unicode character stream).
   *
   * Affects the `context` property sent in `predict` messages.
   *
   * While the right context MUST NOT bisect surrogate pairs, they MAY
   * bisect graphical clusters.
   */
  rightContextCodeUnits: number;
}
