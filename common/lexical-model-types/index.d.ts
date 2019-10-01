/**
 * TypeScript interfaces and types required in both within the LMLayer, and for
 * tools that create lexical models.
 */

 /*************************** Lexical Models ********************************/

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