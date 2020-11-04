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
   * Merges two Transforms as if they were applied to a `Context` successively.
   * @param first 
   * @param second 
   */
  export function buildMergedTransform(first: Transform, second: Transform): Transform {
    // These exist to avoid parameter mutation.
    let mergedFirstInsert: string = first.insert;
    let mergedSecondDelete: number = second.deleteLeft;

    // The 'fun' case:  the second Transform wants to delete something from the first.
    if(second.deleteLeft) {
      let firstLength = first.insert.kmwLength();
      if(firstLength <= second.deleteLeft) {
        mergedFirstInsert = '';
        mergedSecondDelete = second.deleteLeft - firstLength;
      } else {
        mergedFirstInsert = first.insert.kmwSubstr(0, firstLength - second.deleteLeft);
        mergedSecondDelete = 0;
      }
    }

    return {
      insert: mergedFirstInsert + second.insert,
      deleteLeft: first.deleteLeft + mergedSecondDelete,
      // As `first` would affect the context before `second` could take effect,
      // this is the correct way to merge `deleteRight`.
      deleteRight: (first.deleteRight || 0) + (second.deleteRight || 0)
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
  export function transformToSuggestion(transform: Transform, p: number): WithOutcome<Suggestion>; 
  export function transformToSuggestion(transform: Transform, p?: number): Outcome<Suggestion> {
    let suggestion: Outcome<Suggestion> = {
      transform: transform,
      transformId: transform.id,
      displayAs: transform.insert
    };

    if(p === 0 || p) {
      suggestion.p = p;
    }
    return suggestion;
  }

  export function defaultApplyCasing(casing: CasingEnum, text: string): string {
    switch(casing) {
      case 'lower':
        return text.toLowerCase();
      case 'upper':
        return text.toUpperCase();
      case 'initial':
        let headCode = text.charCodeAt(0);
        // The length of the first code unit, as measured in code points.
        let headUnitLength = 1;
  
        // Is the first character a high surrogate, indicating possible use of UTF-16 
        // surrogate pairs?  Also, is the string long enough for there to BE a pair?
        if(text.length > 1 && headCode >= 0xD800 && headCode <= 0xDBFF) {
          // It's possible, so now we check for low surrogates.
          let lowSurrogateCode = text.charCodeAt(1);
  
          if(lowSurrogateCode >= 0xDC00 && lowSurrogateCode <= 0xDFFF) {
            // We have a surrogate pair; this pair is the 'first' character.
            headUnitLength++;
          }
        }
  
        // Capitalizes the first code unit of the string, leaving the rest intact.
        return text.substring(0, headUnitLength).toUpperCase() .concat(text.substring(headUnitLength));
    }
  }
}
