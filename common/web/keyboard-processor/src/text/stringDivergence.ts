/**
 * Returns the index for the code point divergence point in code unit coordinates.
 * @param str1
 * @param str2
 * @param commonSuffix If false, asserts a common prefix to the strings.  If true, asserts a common suffix.
 * @returns The code unit indices within each string for the start of the code point not common to both.
 */
export function searchStringDivergence(str1: string, str2: string, commonSuffix: boolean): [number, number] {
  let maxInterval = Math.min(str1.length, str2.length) - 1;
  const commonPrefix = !commonSuffix;

  let start: number;
  let index: number;
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

  if(commonPrefix) {
    start = index = 0;
    end = maxInterval;
    inc = 1;
    offset = 0;
  } else {
    start = index = str1.length - 1;
    end = index - maxInterval;
    inc = -1;
    offset = str2.length - str1.length;
  }

  // Step 1: Find the index for the first code unit different between the strings.
  for(; commonPrefix ? index <= end: index >= end; index += inc) {
    if(str1.charAt(index) != str2.charAt(index + offset)) {
      break;
    }
  }

  // Step 2:  Ensure that we're not splitting a surrogate pair.

  // `index` corresponds to the first char that is different _in the direction indicated by inc_.
  // If it's the start position, it can't split a (completed) surrogate pair.
  if(index != start) {
    // if commonLeft, high surrogate; if commonRight, low surrogate.
    const commonPotentialSurrogate = str1.charCodeAt(index - inc);
    // Opposite surrogate type from the previous variable.
    const divergentChar1 = str1.charCodeAt(index);
    const divergentChar2 = str2.charCodeAt(index + offset);

    const isHigh = (charCode: number) => charCode >= 0xD800 && charCode <= 0xDBFF;
    const isLow =  (charCode: number) => charCode >= 0xDC00 && charCode <= 0xDFFF;
    const commonChecker = commonPrefix ? isHigh : isLow;
    const divergentChecker = commonPrefix ? isLow : isHigh;

    // If the last common char qualifies as a direction-appropriate SMP surrogate...
    if(commonChecker(commonPotentialSurrogate)) {
      // And one of the two divergent chars is a qualifying match - a surrogate
      // of the opposite type...
      if(divergentChecker(divergentChar1) || divergentChecker(divergentChar2)) {
        // Our current index would split a surrogate pair; decrement the index to
        // preserve the pair.
        return [index - inc, index - inc + offset];
      }
    }
  }

  return [index, index + offset];
}