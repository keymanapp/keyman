/***
 KeymanWeb 19.0
  Copyright 2025 SIL International

  NOTE: in previous versions of Keyman Engine for Web, these methods were set on the standard
  JS String type in a manner considered a "side-effect".  (When inspecting older versions, it may help
  to know that they were originally implemented within a file named kmwstring.ts.)
  *
  These methods seek to support SMP-aware string functionality within the Web engine, as many
  keyboards exist that utilize characters within such ranges.
***/

/**
 * Indicates if functionality supporting Supplementary Multilingual Plane (SMP)
 * characters should be enabled.
 */
let EXTENSION_ENABLED = false;

/**
 * Enables or disables support for SMP-aware string handling.
 * @param bEnable
 */
export function enableSupplementaryPlane(bEnable: boolean) {
  EXTENSION_ENABLED = bEnable;
}

/**
 * Returns a number indicating the Unicode value of the character at the given
 * code point index, with support for supplementary plane characters.
 *
 * @param  {string}  s      The string
 * @param  {number}  index  The code point index into the string (not
                            the code unit index) to return
 * @return {number}         The Unicode character value
 */
export function charCodeAt(s: string, index: number) {
  // While String.codepointAt exists, it does not use code-point indexing and so is
  // unsuited to our needs here.
  const str = s;

  if(!EXTENSION_ENABLED) {
    return s.charCodeAt(index);
  }

  let codeUnitIndex = 0;

  if(index < 0 || index  >= str.length) {
    return NaN;
  }

  for(let i = 0; i < index; i++) {
    codeUnitIndex = nextChar(str, codeUnitIndex);
    if(codeUnitIndex === null) {
      return NaN;
    }
  }

  const first = str.charCodeAt(codeUnitIndex);
  if(first >= 0xD800 && first <= 0xDBFF && str.length > codeUnitIndex + 1) {
    const second = str.charCodeAt(codeUnitIndex + 1);
    if(second >= 0xDC00 && second <= 0xDFFF) {
      return ((first - 0xD800) << 10) + (second - 0xDC00) + 0x10000;
    }
  }
  return first;
}

/**
 * Returns the code point index within the calling String object of the first occurrence
 * of the specified value, or -1 if not found.
 *
 * @param  {string}  s            The string
 * @param  {string}  searchValue  The value to search for
 * @param  {number=} fromIndex    Optional code point index to start searching from
 * @return {number}               The code point index of the specified search value
 */
export function indexOf(s: string, searchValue: string, fromIndex?: number) {
  const str = s;

  if(!EXTENSION_ENABLED) {
    return s.indexOf(searchValue, fromIndex);
  }

  const codeUnitIndex = str.indexOf(searchValue, fromIndex);

  if(codeUnitIndex < 0) {
    return codeUnitIndex;
  }

  let codePointIndex = 0;
  for(let i = 0; i !== null && i < codeUnitIndex; i = nextChar(str, i)) {
    codePointIndex++;
  }
  return codePointIndex;
}

/**
 * Returns the code point index within the calling String object of the last occurrence
 * of the specified value, or -1 if not found.
 *
 * @param  {string}  searchValue    The value to search for
 * @param  {number=} fromIndex      Optional code point index to start searching from
 * @return {number}                 The code point index of the specified search value
 */
export function lastIndexOf(s: string, searchValue: string, fromIndex?: number) {
  const str = s;

  if(!EXTENSION_ENABLED) {
    return str.lastIndexOf(searchValue, fromIndex);
  }

  const codeUnitIndex = str.lastIndexOf(searchValue, fromIndex);

  if(codeUnitIndex < 0) {
    return codeUnitIndex;
  }

  let codePointIndex = 0;
  for(let i = 0; i !== null && i < codeUnitIndex; i = nextChar(str, i)) {
    codePointIndex++;
  }
  return codePointIndex;
}

/**
 * Returns the length of the string in code points, as opposed to code units.
 *
 * @param  {string}  s      The string
 * @return {number}         The length of the string in code points
 */
export function length(s: string) {
  const str = s;

  if(!EXTENSION_ENABLED) {
    return str.length;
  }

  if(str.length == 0) {
    return 0;
  }

  let i: number;
  let codeUnitIndex: number;
  for(i = 0, codeUnitIndex = 0; codeUnitIndex !== null; i++) {
    codeUnitIndex = nextChar(str, codeUnitIndex);
  }
  return i;
}


/**
 * Extracts a section of a string and returns a new string.
 *
 * @param  {string}  s             The string
 * @param  {number}  beginSlice    The start code point index in the string to
 *                                 extract from
 * @param  {number}  endSlice      Optional end code point index in the string
 *                                 to extract to
 * @return {string}                The substring as selected by beginSlice and
 *                                 endSlice
 */
export function slice(s: string, beginSlice: number, endSlice?: number) {
  const str = s;

  if(!EXTENSION_ENABLED) {
    return str.slice(beginSlice, endSlice);
  }

  const beginSliceCodeUnit = codePointToCodeUnit(str, beginSlice);
  const endSliceCodeUnit = codePointToCodeUnit(str, endSlice);
  if(beginSliceCodeUnit === null || endSliceCodeUnit === null) {
    return '';
  } else {
    return str.slice(beginSliceCodeUnit, endSliceCodeUnit);
  }
}

/**
 * Returns the characters in a string beginning at the specified location through
 * the specified number of characters.
 *
 * @param  {string}  s             The string
 * @param  {number}  start         The start code point index in the string to
 *                                 extract from
 * @param  {number=}  len          Optional length to extract
 * @return {string}                The substring as selected by start and length
 */
export function substr(s: string, start: number, len?: number) {
  const str = s;

  if(!EXTENSION_ENABLED) {
    if(start > -1) {
      return str.substr(start,len);
    } else {
      return str.substr(str.length+start,-start);
    }
  }

  if(start < 0) {
    start = length(str) + start;
  }
  if(start < 0) {
    start = 0;
  }
  const startCodeUnit = codePointToCodeUnit(str, start);
  let endCodeUnit = startCodeUnit;

  if(startCodeUnit === null) {
    return '';
  }

  if(arguments.length < 3) {
    endCodeUnit = str.length;
  } else {
    for(let i = 0; i < len; i++) {
      endCodeUnit = nextChar(str, endCodeUnit);
    }
  }

  if(endCodeUnit === null) {
    return str.substring(startCodeUnit);
  } else {
    return str.substring(startCodeUnit, endCodeUnit);
  }
}

/**
   * Returns the characters in a string between two indexes into the string.
   *
   * @param  {string}  s             The string
   * @param  {number}  indexA        The start code point index in the string to
   *                                 extract from
   * @param  {number=}  indexB        The end code point index in the string to
   *                                 extract to
   * @return {string}                The substring as selected by indexA and indexB
   */
export function substring(s: string, indexA: number, indexB?: number) {
  const str = s;

  if(!EXTENSION_ENABLED) {
    return str.substring(indexA, indexB);
  }

  let indexACodeUnit, indexBCodeUnit;

  if(typeof(indexB) == 'undefined') {
    indexACodeUnit = codePointToCodeUnit(str, indexA);
    indexBCodeUnit =  str.length;
  } else {
    if(indexA > indexB) {
      const c = indexA;
      indexA = indexB;
      indexB = c;
    }

    indexACodeUnit = codePointToCodeUnit(str, indexA);
    indexBCodeUnit = codePointToCodeUnit(str, indexB);
  }

  if(isNaN(indexACodeUnit) || indexACodeUnit === null) {
    indexACodeUnit = 0;
  }
  if(isNaN(indexBCodeUnit) || indexBCodeUnit === null) {
    indexBCodeUnit = str.length;
  }

  return str.substring(indexACodeUnit, indexBCodeUnit);
}

/**
 * Returns the code unit index for the next code point in the string, accounting for
 * supplementary pairs
 *
 * @param  {string}  s      The string
 * @param  {number|null}  codeUnitIndex  The code unit position to increment
 * @return {number|null}                 The index of the next code point in the string,
 *                                       in code units
 */
export function nextChar(s: string, codeUnitIndex: number) {
  const str = s;
  if(codeUnitIndex === null || codeUnitIndex < 0 || codeUnitIndex >= str.length - 1) {
    return null;
  }

  if(!EXTENSION_ENABLED) {
    return codeUnitIndex+1;
  }

  const first = str.charCodeAt(codeUnitIndex);
  if(first >= 0xD800 && first <= 0xDBFF && str.length > codeUnitIndex + 1) {
    const second = str.charCodeAt(codeUnitIndex + 1);
    if(second >= 0xDC00 && second <= 0xDFFF) {
      if(codeUnitIndex == str.length - 2) {
        return null;
      }
      return codeUnitIndex + 2;
    }
  }
  return codeUnitIndex + 1;
}

/**
 * Returns the code unit index for the previous code point in the string, accounting
 * for supplementary pairs
 *
 * @param  {string}       s              The string
 * @param  {number|null}  codeUnitIndex  The code unit position to decrement
 * @return {number|null}                 The index of the previous code point in the
 *                                       string, in code units
*/
export function prevChar(s: string, codeUnitIndex: number) {
  const str = s;

  if(codeUnitIndex == null || codeUnitIndex <= 0 || codeUnitIndex > str.length) {
    return null;
  }

  if(!EXTENSION_ENABLED) {
    return codeUnitIndex-1;
  }

  const second = str.charCodeAt(codeUnitIndex - 1);
  if(second >= 0xDC00 && second <= 0xDFFF && codeUnitIndex > 1) {
    const first = str.charCodeAt(codeUnitIndex - 2);
    if(first >= 0xD800 && first <= 0xDBFF) {
      return codeUnitIndex - 2;
    }
  }
  return codeUnitIndex - 1;
}

/**
 * Returns the corresponding code unit index to the code point index passed
 *
 * @param  {string}      s               The string
 * @param  {number|null} codePointIndex  A code point index in the string
 * @return {number|null}                 The corresponding code unit index
 */
export function codePointToCodeUnit(s: string, codePointIndex: number) {
  if(codePointIndex === null) {
    return null;
  }

  const str = s;
  let codeUnitIndex = 0;

  if(codePointIndex < 0) {
    codeUnitIndex = str.length;
    for(let i = 0; i > codePointIndex; i--) {
      codeUnitIndex = prevChar(str, codeUnitIndex);
    }
    return codeUnitIndex;
  }

  if(codePointIndex == length(str)) {
    return str.length;
  }

  for(let i = 0; i < codePointIndex; i++) {
    codeUnitIndex = nextChar(str, codeUnitIndex);
  }
  return codeUnitIndex;
}

/**
 * Returns the corresponding code point index to the code unit index passed
 *
 * @param  {number|null}  codeUnitIndex  A code unit index in the string
 * @return {number|null}                 The corresponding code point index
 */
export function codeUnitToCodePoint(s: string, codeUnitIndex: number) {
  const str = s;

  if(codeUnitIndex === null) {
    return null;
  } else if(codeUnitIndex == 0) {
    return 0;
  } else if(codeUnitIndex < 0) {
    return length(str.substr(codeUnitIndex));
  } else {
    return length(str.substr(0,codeUnitIndex));
  }
}

/**
 * Returns the character at a the code point index passed
 *
 * @param  {string}  s      The string
 * @param  {number}  index  A code point index in the string
 * @return {string}         The corresponding character
 */
export function charAt(s: string, index: number) {
  const str = s;

  if(!EXTENSION_ENABLED) {
    return str.charAt(index);
  }

  if(index >= 0) {
    return substr(str, index, 1);
  } else {
    return ''
  };
}