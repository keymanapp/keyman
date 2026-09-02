import { Uni_IsSurrogate1, Uni_IsSurrogate2 } from '@keymanapp/common-types';

/**
 * Returns the index for the code point divergence point between two strings, as measured in code
 * unit coordinates.
 * @param str1
 * @param str2
 * @param commonSuffix If false, asserts a common prefix to the strings.  If true, asserts a common suffix.
 * @returns The code unit index within `str1` for the start of the code point not common to both.
 *
 * Follows the convention of (start, end) substring parameterizations having 'end' be exclusive.
 */
export function findCommonSubstringEndIndex(str1: string, str2: string, commonSuffix: boolean): number {
  /**
   * The maximum number of iterations to consider; exceeding this would go past a string boundary.
   */
  const maxInterval = Math.min(str1.length, str2.length);

  /**
   * The first valid index within the string.
   */
  let start: number;

  /**
   * The current index within the string under consideration as the divergence point.
   */
  let index: number;

  /**
   * The index at which to terminate the search for a divergence point.
   */
  let end: number;

  /**
   * Index shift per loop iteration.
   */
  let inc: number;

  /**
   * Difference in index for comparison between strings.
   * Mostly matters when assuming a common right-hand side.
   */
  let offset: number;

  if(commonSuffix) {
    start = index = str1.length - 1; // e.g. str.length == 10 => start = 9.
    end = index - maxInterval;       // e.g. maxInterval 8, start 9 => iterate from 9 to 2, end at 1.
    inc = -1;
    offset = str2.length - str1.length;
  } else {
    start = index = 0;
    end = maxInterval; // last valid index: - 1.  e.g. maxInterval 8 => iterate from 0 to 7, end at 8.
    inc = 1;
    offset = 0;
  }

  // Step 1: Find the index for the first code unit different between the strings.
  for(; index != end; index += inc) {
    if(str1.charAt(index) != str2.charAt(index + offset)) {
      break;
    }
  }

  // Step 2:  Ensure that we're not splitting a surrogate pair.

  // `index` corresponds to the first char that is different _in the direction indicated by inc_.
  // If it's the start position, it can't split a (completed) surrogate pair.
  if(index != start && index != end) {
    // if commonLeft, high surrogate; if commonRight, low surrogate.
    const commonPotentialSurrogate = str1.charCodeAt(index - inc);
    // Opposite surrogate type from the previous variable.
    const divergentChar1 = str1.charCodeAt(index);
    const divergentChar2 = str2.charCodeAt(index + offset);

    const commonSurrogateChecker = commonSuffix ? Uni_IsSurrogate2 : Uni_IsSurrogate1;
    const divergentSurrogateChecker = commonSuffix ? Uni_IsSurrogate1 : Uni_IsSurrogate2;

    // If the last common character if of the direction-appropriate surrogate type (for
    // comprising a potential split surrogate pair representing a non-BMP char)...
    if(commonSurrogateChecker(commonPotentialSurrogate)) {
      // And one of the two divergent chars is a qualifying match - a surrogate
      // of the opposite type...
      if(divergentSurrogateChecker(divergentChar1) || divergentSurrogateChecker(divergentChar2)) {
        // Our current index would split a surrogate pair; decrement the index to
        // preserve the pair.
        return index - inc;
      }
    }
  }

  return index;
}
