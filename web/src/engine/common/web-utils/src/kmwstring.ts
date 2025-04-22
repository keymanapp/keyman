/***
   KeymanWeb 19.0
   Copyright 2025 SIL International
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

  if(!EXTENSION_ENABLED) {
    return codePointIndex;
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

  if(!EXTENSION_ENABLED) {
    return codeUnitIndex;
  }

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

/*
 * TODO:  Remove this file as part of addressing https://github.com/keymanapp/keyman/issues/2492.
 */

declare global {
  interface StringConstructor {
    kmwEnableSupplementaryPlane(bEnable: boolean): void
  }

  interface String {
    kmwCharCodeAt(codePointIndex: number): number,
    kmwCharAt(codePointIndex: number) : string,
    kmwIndexOf(searchValue: string, fromIndex?: number) : number,
    kmwLastIndexOf(searchValue: string, fromIndex?: number) : number,
    kmwSlice(beginSlice: number, endSlice: number) : string,
    kmwSubstring(start: number, length: number) : string,
    kmwSubstr(start: number, length?: number) : string,
    kmwBMPSubstr(start: number, length?: number) : string,
    _kmwCharCodeAt(codePointIndex: number): number,
    _kmwCharAt(codePointIndex: number) : string,
    _kmwIndexOf(searchValue: string, fromIndex?: number) : number,
    _kmwLastIndexOf(searchValue: string, fromIndex?: number) : number,
    _kmwSlice(beginSlice: number, endSlice: number) : string,
    _kmwSubstring(start: number, length?: number) : string,
    _kmwSubstr(start: number, length?: number) : string
  }
}

export default function extendString() {
  /**
   * Returns a number indicating the Unicode value of the character at the given
   * code point index, with support for supplementary plane characters.
   *
   * @param  {number}  codePointIndex  The code point index into the string (not
                                       the code unit index) to return
  * @return {number}                  The Unicode character value
  */
  String.prototype.kmwCharCodeAt = function(codePointIndex) {
    var str = String(this);
    var codeUnitIndex = 0;

    if (codePointIndex < 0 || codePointIndex  >= str.length) {
      return NaN;
    }

    for(var i = 0; i < codePointIndex; i++) {
      codeUnitIndex = nextChar(str, codeUnitIndex);
      if(codeUnitIndex === null) return NaN;
    }

    var first = str.charCodeAt(codeUnitIndex);
    if (first >= 0xD800 && first <= 0xDBFF && str.length > codeUnitIndex + 1) {
      var second = str.charCodeAt(codeUnitIndex + 1);
      if (second >= 0xDC00 && second <= 0xDFFF) {
        return ((first - 0xD800) << 10) + (second - 0xDC00) + 0x10000;
      }
    }
    return first;
  }

  /**
   * Returns the code point index within the calling String object of the first occurrence
   * of the specified value, or -1 if not found.
   *
   * @param  {string}  searchValue    The value to search for
   * @param  {number}  [fromIndex]    Optional code point index to start searching from
   * @return {number}                 The code point index of the specified search value
   */
  String.prototype.kmwIndexOf = function(searchValue, fromIndex) {
    var str = String(this);
    var codeUnitIndex = str.indexOf(searchValue, fromIndex);

    if(codeUnitIndex < 0) {
      return codeUnitIndex;
    }

    var codePointIndex = 0;
    for(var i = 0; i !== null && i < codeUnitIndex; i = nextChar(str, i)) codePointIndex++;
    return codePointIndex;
  }

  /**
   * Returns the code point index within the calling String object of the last occurrence
   * of the specified value, or -1 if not found.
   *
   * @param  {string}  searchValue    The value to search for
   * @param  {number}  fromIndex      Optional code point index to start searching from
   * @return {number}                 The code point index of the specified search value
   */
  String.prototype.kmwLastIndexOf = function(searchValue, fromIndex)
  {
    var str = String(this);
    var codeUnitIndex = str.lastIndexOf(searchValue, fromIndex);

    if(codeUnitIndex < 0) {
      return codeUnitIndex;
    }

    var codePointIndex = 0;
    for(var i = 0; i !== null && i < codeUnitIndex; i = nextChar(str, i)) codePointIndex++;
    return codePointIndex;
  }

  /**
   * Extracts a section of a string and returns a new string.
   *
   * @param  {number}  beginSlice    The start code point index in the string to
   *                                 extract from
   * @param  {number}  endSlice      Optional end code point index in the string
   *                                 to extract to
   * @return {string}                The substring as selected by beginSlice and
   *                                 endSlice
   */
  String.prototype.kmwSlice = function(beginSlice, endSlice) {
    var str = String(this);
    var beginSliceCodeUnit = codePointToCodeUnit(str, beginSlice);
    var endSliceCodeUnit = codePointToCodeUnit(str, endSlice);
    if(beginSliceCodeUnit === null || endSliceCodeUnit === null)
      return '';
    else
      return str.slice(beginSliceCodeUnit, endSliceCodeUnit);
  }

  /**
   * Returns the characters in a string beginning at the specified location through
   * the specified number of characters.
   *
   * @param  {number}  start         The start code point index in the string to
   *                                 extract from
   * @param  {number=}  len           Optional length to extract
   * @return {string}                The substring as selected by start and length
   */
  String.prototype.kmwSubstr = function(start, len?)
  {
    var str = String(this);
    if(start < 0)
    {
      start = length(str) + start;
    }
    if(start < 0) start = 0;
    var startCodeUnit = codePointToCodeUnit(str, start);
    var endCodeUnit = startCodeUnit;

    if(startCodeUnit === null) return '';

    if(arguments.length < 2) {
      endCodeUnit = str.length;
    } else {
      for(var i = 0; i < len; i++) endCodeUnit = nextChar(str, endCodeUnit);
    }
    if(endCodeUnit === null)
      return str.substring(startCodeUnit);
    else
      return str.substring(startCodeUnit, endCodeUnit);
  }

  /**
   * Returns the characters in a string between two indexes into the string.
   *
   * @param  {number}  indexA        The start code point index in the string to
   *                                 extract from
   * @param  {number}  indexB        The end code point index in the string to
   *                                 extract to
   * @return {string}                The substring as selected by indexA and indexB
   */
  String.prototype.kmwSubstring = function(indexA, indexB)
  {
    var str = String(this),indexACodeUnit,indexBCodeUnit;

    if(typeof(indexB) == 'undefined')
    {
      indexACodeUnit = codePointToCodeUnit(str, indexA);
      indexBCodeUnit =  str.length;
    }
    else
    {
      if(indexA > indexB) { var c = indexA; indexA = indexB; indexB = c; }

      indexACodeUnit = codePointToCodeUnit(str, indexA);
      indexBCodeUnit = codePointToCodeUnit(str, indexB);
    }
    if(isNaN(indexACodeUnit) || indexACodeUnit === null) indexACodeUnit = 0;
    if(isNaN(indexBCodeUnit) || indexBCodeUnit === null) indexBCodeUnit = str.length;

    return str.substring(indexACodeUnit, indexBCodeUnit);
  }

  /*
    Helper functions
  */

  /**
   * Returns the character at a the code point index passed
   *
   * @param  {number}  codePointIndex  A code point index in the string
   * @return {string}                  The corresponding character
   */
  String.prototype.kmwCharAt = function(codePointIndex) {
    var str = String(this);

    if(codePointIndex >= 0) return str.kmwSubstr(codePointIndex,1); else return '';
  }

  /**
   * String prototype library extensions for basic plane characters,
   * to simplify enabling or disabling supplementary plane functionality (I3319)
   */

  /**
   * Returns a substring
   *
   * @param  {number}  n
   * @param  {number=}  ln
   * @return {string}
   */
  String.prototype.kmwBMPSubstr = function(n,ln?)
  {
    var str=String(this);
    if(n > -1)
      return str.substr(n,ln);
    else
      return str.substr(str.length+n,-n);
  }

  /**
   * Enable or disable supplementary plane string handling
   *
   * @param  {boolean}  bEnable
   */
  String.kmwEnableSupplementaryPlane = function(bEnable)
  {
    var p=String.prototype;
    p._kmwCharAt = bEnable ? p.kmwCharAt : p.charAt;
    p._kmwCharCodeAt = bEnable ? p.kmwCharCodeAt : p.charCodeAt;
    p._kmwIndexOf = bEnable ? p.kmwIndexOf :p.indexOf;
    p._kmwLastIndexOf = bEnable ? p.kmwLastIndexOf : p.lastIndexOf ;
    p._kmwSlice = bEnable ? p.kmwSlice : p.slice;
    p._kmwSubstring = bEnable ? p.kmwSubstring : p.substring;
    p._kmwSubstr = bEnable ? p.kmwSubstr : p.kmwBMPSubstr;

    enableSupplementaryPlane(bEnable);
  }

  // Ensure that _all_ String extensions are established, even if disabled by default.
  if(!""._kmwCharAt) {
    String.kmwEnableSupplementaryPlane(false);
  }
}

// For side-effect imports:
extendString();