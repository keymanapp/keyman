/***
   KeymanWeb 14.0
   Copyright 2020 SIL International
***/


/*
 * TODO:  Remove this file as part of addressing https://github.com/keymanapp/keyman/issues/2492.
 */

declare global {
  interface StringConstructor {
    kmwFromCharCode(cp0: number): string,
    _kmwFromCharCode(cp0: number): string,
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
    kmwLength(): number,
    kmwBMPLength(): number,
    kmwNextChar(codeUnitIndex: number): number,
    kmwBMPNextChar(codeUnitIndex: number): number,
    kmwPrevChar(codeUnitIndex: number): number,
    kmwBMPPrevChar(codeUnitIndex: number): number,
    kmwCodePointToCodeUnit(codePointIndex: number) : number,
    kmwBMPCodePointToCodeUnit(codePointIndex: number) : number,
    kmwCodeUnitToCodePoint(codeUnitIndex: number) : number,
    kmwBMPCodeUnitToCodePoint(codeUnitIndex: number) : number,
    _kmwCharCodeAt(codePointIndex: number): number,
    _kmwCharAt(codePointIndex: number) : string,
    _kmwIndexOf(searchValue: string, fromIndex?: number) : number,
    _kmwLastIndexOf(searchValue: string, fromIndex?: number) : number,
    _kmwSlice(beginSlice: number, endSlice: number) : string,
    _kmwSubstring(start: number, length?: number) : string,
    _kmwSubstr(start: number, length?: number) : string,
    _kmwLength(): number,
    _kmwNextChar(codeUnitIndex: number): number,
    _kmwPrevChar(codeUnitIndex: number): number,
    _kmwCodePointToCodeUnit(codePointIndex: number) : number,
    _kmwCodeUnitToCodePoint(codeUnitIndex: number) : number
  }
}

export default function extendString() {
  /**
   * Constructs a string from one or more Unicode character codepoint values
   * passed as integer parameters.
   *
   * @param  {number} cp0,...   1 or more Unicode codepoints, e.g. 0x0065, 0x10000
   * @return {string|null}      The new String object.
   */
  String.kmwFromCharCode = function(cp0) {
    var chars = [], i;
    for (i = 0; i < arguments.length; i++) {
      var c = Number(arguments[i]);
      if (!isFinite(c) || c < 0 || c > 0x10FFFF || Math.floor(c) !== c) {
        throw new RangeError("Invalid code point " + c);
      }
      if (c < 0x10000) {
        chars.push(c);
      } else {
        c -= 0x10000;
        chars.push((c >> 10) + 0xD800);
        chars.push((c % 0x400) + 0xDC00);
      }
    }
    return String.fromCharCode.apply(undefined, chars);
  }

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
      codeUnitIndex = str.kmwNextChar(codeUnitIndex);
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
    for(var i = 0; i !== null && i < codeUnitIndex; i = str.kmwNextChar(i)) codePointIndex++;
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
    for(var i = 0; i !== null && i < codeUnitIndex; i = str.kmwNextChar(i)) codePointIndex++;
    return codePointIndex;
  }

  /**
   * Returns the length of the string in code points, as opposed to code units.
   *
   * @return {number}                 The length of the string in code points
   */
  String.prototype.kmwLength = function() {
    var str = String(this);

    if(str.length == 0) return 0;

    for(var i = 0, codeUnitIndex = 0; codeUnitIndex !== null; i++)
      codeUnitIndex = str.kmwNextChar(codeUnitIndex);
    return i;
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
    var beginSliceCodeUnit = str.kmwCodePointToCodeUnit(beginSlice);
    var endSliceCodeUnit = str.kmwCodePointToCodeUnit(endSlice);
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
   * @param  {number=}  length        Optional length to extract
   * @return {string}                The substring as selected by start and length
   */
  String.prototype.kmwSubstr = function(start, length?)
  {
    var str = String(this);
    if(start < 0)
    {
      start = str.kmwLength() + start;
    }
    if(start < 0) start = 0;
    var startCodeUnit = str.kmwCodePointToCodeUnit(start);
    var endCodeUnit = startCodeUnit;

    if(startCodeUnit === null) return '';

    if(arguments.length < 2) {
      endCodeUnit = str.length;
    } else {
      for(var i = 0; i < length; i++) endCodeUnit = str.kmwNextChar(endCodeUnit);
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
      indexACodeUnit = str.kmwCodePointToCodeUnit(indexA);
      indexBCodeUnit =  str.length;
    }
    else
    {
      if(indexA > indexB) { var c = indexA; indexA = indexB; indexB = c; }

      indexACodeUnit = str.kmwCodePointToCodeUnit(indexA);
      indexBCodeUnit = str.kmwCodePointToCodeUnit(indexB);
    }
    if(isNaN(indexACodeUnit) || indexACodeUnit === null) indexACodeUnit = 0;
    if(isNaN(indexBCodeUnit) || indexBCodeUnit === null) indexBCodeUnit = str.length;

    return str.substring(indexACodeUnit, indexBCodeUnit);
  }

  /*
    Helper functions
  */

  /**
   * Returns the code unit index for the next code point in the string, accounting for
   * supplementary pairs
   *
   * @param  {number|null}  codeUnitIndex  The code unit position to increment
   * @return {number|null}                 The index of the next code point in the string,
   *                                       in code units
   */
  String.prototype.kmwNextChar = function(codeUnitIndex) {
    var str = String(this);

    if(codeUnitIndex === null || codeUnitIndex < 0 || codeUnitIndex >= str.length - 1) {
      return null;
    }

    var first = str.charCodeAt(codeUnitIndex);
    if (first >= 0xD800 && first <= 0xDBFF && str.length > codeUnitIndex + 1) {
      var second = str.charCodeAt(codeUnitIndex + 1);
      if (second >= 0xDC00 && second <= 0xDFFF) {
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
   * @param  {number|null}  codeUnitIndex  The code unit position to decrement
   * @return {number|null}                 The index of the previous code point in the
   *                                       string, in code units
  */
  String.prototype.kmwPrevChar = function(codeUnitIndex) {
    var str = String(this);

    if(codeUnitIndex == null || codeUnitIndex <= 0 || codeUnitIndex > str.length) {
      return null;
    }

    var second = str.charCodeAt(codeUnitIndex - 1);
    if (second >= 0xDC00 && second <= 0xDFFF && codeUnitIndex > 1) {
      var first = str.charCodeAt(codeUnitIndex - 2);
      if(first >= 0xD800 && first <= 0xDBFF) {
        return codeUnitIndex - 2;
      }
    }
    return codeUnitIndex - 1;
  }

  /**
   * Returns the corresponding code unit index to the code point index passed
   *
   * @param  {number|null} codePointIndex  A code point index in the string
   * @return {number|null}                 The corresponding code unit index
   */
  String.prototype.kmwCodePointToCodeUnit = function(codePointIndex) {

    if(codePointIndex === null) return null;

    var str = String(this);
    var codeUnitIndex = 0;

    if(codePointIndex < 0) {
      codeUnitIndex = str.length;
      for(var i = 0; i > codePointIndex; i--)
        codeUnitIndex = str.kmwPrevChar(codeUnitIndex);
      return codeUnitIndex;
    }

    if(codePointIndex == str.kmwLength()) return str.length;

    for(var i = 0; i < codePointIndex; i++)
      codeUnitIndex = str.kmwNextChar(codeUnitIndex);
    return codeUnitIndex;
  }

  /**
   * Returns the corresponding code point index to the code unit index passed
   *
   * @param  {number|null}  codeUnitIndex  A code unit index in the string
   * @return {number|null}                 The corresponding code point index
   */
  String.prototype.kmwCodeUnitToCodePoint = function(codeUnitIndex) {
    var str = String(this);

    if(codeUnitIndex === null)
      return null;
    else if(codeUnitIndex == 0)
      return 0;
    else if(codeUnitIndex < 0)
      return str.substr(codeUnitIndex).kmwLength();
    else
      return str.substr(0,codeUnitIndex).kmwLength();
  }

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
   * Returns the code unit index for the next code point in the string
   *
   * @param  {number}  codeUnitIndex    A code point index in the string
   * @return {number|null}                   The corresponding character
   */
  String.prototype.kmwBMPNextChar = function(codeUnitIndex)
  {
    var str = String(this);
    if(codeUnitIndex < 0 || codeUnitIndex >= str.length - 1) {
      return null;
    }
    return codeUnitIndex + 1;
  }

  /**
   * Returns the code unit index for the previous code point in the string
   *
   * @param  {number}  codeUnitIndex    A code unit index in the string
   * @return {number|null}                   The corresponding character
   */
  String.prototype.kmwBMPPrevChar = function(codeUnitIndex)
  {
    var str = String(this);

    if(codeUnitIndex <= 0 || codeUnitIndex > str.length) {
      return null;
    }
    return codeUnitIndex - 1;
  }

  /**
   * Returns the code unit index for a code point index
   *
   * @param  {number}  codePointIndex   A code point index in the string
   * @return {number}                   The corresponding character
   */
  String.prototype.kmwBMPCodePointToCodeUnit = function(codePointIndex)
  {
    return codePointIndex;
  }

  /**
   * Returns the code point index for a code unit index
   *
   * @param  {number}  codeUnitIndex    A code point index in the string
   * @return {number}                   The corresponding character
   */
  String.prototype.kmwBMPCodeUnitToCodePoint = function(codeUnitIndex)
  {
    return codeUnitIndex;
  }

  /**
   * Returns the length of a BMP string
   *
   * @return {number}                   The length in code points
   */
  String.prototype.kmwBMPLength = function()
  {
    var str = String(this);
    return str.length;
  }

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
    String._kmwFromCharCode = bEnable ? String.kmwFromCharCode : String.fromCharCode;
    p._kmwCharAt = bEnable ? p.kmwCharAt : p.charAt;
    p._kmwCharCodeAt = bEnable ? p.kmwCharCodeAt : p.charCodeAt;
    p._kmwIndexOf = bEnable ? p.kmwIndexOf :p.indexOf;
    p._kmwLastIndexOf = bEnable ? p.kmwLastIndexOf : p.lastIndexOf ;
    p._kmwSlice = bEnable ? p.kmwSlice : p.slice;
    p._kmwSubstring = bEnable ? p.kmwSubstring : p.substring;
    p._kmwSubstr = bEnable ? p.kmwSubstr : p.kmwBMPSubstr;
    p._kmwLength = bEnable ? p.kmwLength : p.kmwBMPLength;
    p._kmwNextChar = bEnable ? p.kmwNextChar : p.kmwBMPNextChar;
    p._kmwPrevChar = bEnable ? p.kmwPrevChar : p.kmwBMPPrevChar;
    p._kmwCodePointToCodeUnit = bEnable ? p.kmwCodePointToCodeUnit : p.kmwBMPCodePointToCodeUnit;
    p._kmwCodeUnitToCodePoint = bEnable ? p.kmwCodeUnitToCodePoint : p.kmwBMPCodeUnitToCodePoint;
  }

  // Ensure that _all_ String extensions are established, even if disabled by default.
  if(!String._kmwFromCharCode) {
    String.kmwEnableSupplementaryPlane(false);
  }
}

// For side-effect imports:
extendString();