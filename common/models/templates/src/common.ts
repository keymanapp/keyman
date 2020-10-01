namespace models {
  export const SENTINEL_CODE_UNIT = '\uFDD0';

  export function applyTransform(transform: Transform, context: Context): Context {
    // First, get the current context
    let fullLeftContext = context.left || '';
    let lLen = fullLeftContext.length;
    let lDel = lLen < transform.deleteLeft ? lLen : transform.deleteLeft;

    let leftContext = fullLeftContext.substring(0, lLen - lDel) + (transform.insert || '');

    let fullRightContext = context.right || '';
    let rLen = fullRightContext.length;
    let rDel = rLen < transform.deleteRight ? rLen : transform.deleteRight;

    let rightContext = fullRightContext.substring(rDel);

    return {
      left: leftContext,
      right: rightContext,
      startOfBuffer: context.startOfBuffer,
      endOfBuffer: context.endOfBuffer
    };
  }

  /**
   *
   * @param transform Merges one transform into another, mutating the first parameter to
   *                  include the effects of the second.
   * @param prefix
   */
  export function prependTransform(transform: Transform, prefix: Transform) {
    transform.insert = prefix.insert + transform.insert;
    transform.deleteLeft += prefix.deleteLeft;
    if(prefix.deleteRight) {
      transform.deleteRight = (transform.deleteRight || 0) + prefix.deleteRight;
    }
  }

  /**
   * Checks whether or not the specified UCS-2 character corresponds to a UTF-16 high surrogate.
   * 
   * @param char A single JavaScript (UCS-2) char corresponding to a single code unit.
   */
  export function isHighSurrogate(char: string): boolean;
  /**
   * Checks whether or not the specified UCS-2 character corresponds to a UTF-16 high surrogate.
   * 
   * @param codeUnit A code unit corresponding to a single UCS-2 char.
   */
  export function isHighSurrogate(codeUnit: number): boolean;
  export function isHighSurrogate(codeUnit: string|number): boolean {
    if(typeof codeUnit == 'string') {
      codeUnit = codeUnit.charCodeAt(0);
    }

    return codeUnit >= 0xD800 && codeUnit <= 0xDBFF;
  }

  export function isSentinel(char: string): boolean {
    return char == models.SENTINEL_CODE_UNIT;
  }

  /**
   * Builds a Suggestion based on a Transform corresponding to a predictive-text op.
   * 
   * Assumes that the Transform's `insert` property represents a completed word,
   * as models generally delete the whole prefix, replacing it with the full lexical entry.
   * @param transform 
   */
  export function transformToSuggestion(transform: Transform): Suggestion;
  export function transformToSuggestion(transform: Transform, p: number): Suggestion & {p: number}; 
  export function transformToSuggestion(transform: Transform, p?: number): Suggestion & {p?: number} {
    return {
      transform: transform,
      transformId: transform.id,
      displayAs: transform.insert,
      p: p
    };
  }
}
