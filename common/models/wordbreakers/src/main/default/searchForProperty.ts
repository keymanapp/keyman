import { WordBreakProperty, WORD_BREAK_PROPERTY_BMP, WORD_BREAK_PROPERTY_NON_BMP } from "./data.js";

export function searchForProperty(codePoint: number): WordBreakProperty {
  const bucketSize = codePoint <= 0xFFFF ? 2 : 3;

  // SMP chars take a bit more space to encode.
  const encodedArray = bucketSize == 2 ? WORD_BREAK_PROPERTY_BMP : WORD_BREAK_PROPERTY_NON_BMP;

  return _searchForProperty(encodedArray, codePoint, bucketSize, 0, encodedArray.length / bucketSize - 1) - 0x20;
}

/**
 * Binary search for the word break property of a given CODE POINT.
 *
 * The auto-generated data.ts master strings encode **character range**
 * lookup tables.  If a character's codepoint is equal to or greater than
 * the start-of-range value for an entry and exclusively less than the next
 * entry's start-of-range, it falls within the first entry's range bucket
 * and is classified accordingly by this method.
 */
function _searchForProperty(encodedArray: string, codePoint: number, bucketSize: number, left: number, right: number): WordBreakProperty {
  // All items that are not found in the array are assigned the 'Other' property.
  if (right < left) {  // May need special handling at end of BMP / start of non-BMP.
    return WordBreakProperty.Other;
  }

  let midpoint = left + ~~((right - left) / 2);
  let candidate = encodedArray.codePointAt(bucketSize * midpoint);

  // If out-of-bounds, gives NaN.
  let nextRange = encodedArray.codePointAt(bucketSize * (midpoint + 1));
  let startOfNextRange = isNaN(nextRange) ? Infinity : nextRange;

  if (codePoint < candidate) {
    return _searchForProperty(encodedArray, codePoint, bucketSize, left, midpoint - 1);
  } else if (codePoint >= startOfNextRange) {
    return _searchForProperty(encodedArray, codePoint, bucketSize, midpoint + 1, right);
  }

  // We found it!
  const propertyCode = encodedArray.charCodeAt(bucketSize * (midpoint + 1) - 1);
  return propertyCode as WordBreakProperty;
}