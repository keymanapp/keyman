import { MATCH_HEX_ESCAPE, CONTAINS_QUAD_ESCAPE, MATCH_QUAD_ESCAPE } from './consts.js';
export { MATCH_HEX_ESCAPE, CONTAINS_QUAD_ESCAPE, MATCH_QUAD_ESCAPE };

/**
 * xml2js will not place single-entry objects into arrays. Easiest way to fix
 * this is to box them ourselves as needed. Ensures that o[x] is always an array.
 *
 * @param o Object with property to box
 * @param x Name of element to box
 */
export function boxXmlArray(o: any, x: string): void {

  if (typeof o == 'object' && !Array.isArray(o[x])) {
    if (o[x] === null || o[x] === undefined) {
      o[x] = [];
    }
    else {
      o[x] = [o[x]];
    }
  }
}

export class UnescapeError extends Error {
}

/**
 * Unescape one codepoint
 * @param hex one codepoint in hex, such as '0127'
 * @returns the unescaped codepoint
 */
export function unescapeOne(hex: string): string {
  const codepoint = Number.parseInt(hex, 16);
  return String.fromCodePoint(codepoint);
}

/**
 * Unescape one single quad string such as \u0127 / \U00000000
 * Throws exception if the string doesn't match MATCH_QUAD_ESCAPE
 * Note this does not attempt to handle or reject surrogates.
 * So, `\\uD838\\uDD09` will work but other combinations may not.
 * @param s input string
 * @returns output
 */
export function unescapeOneQuadString(s: string): string {
  if (!s || !s.match(MATCH_QUAD_ESCAPE)) {
    throw new UnescapeError(`Not a quad escape: ${s}`);
  }
  function processMatch(str: string, m16: string, m32: string): string {
    return unescapeOne(m16 || m32); // either \u or \U
  }
  s = s.replace(MATCH_QUAD_ESCAPE, processMatch);
  return s;
}

/** unscape multiple occurences of \u0127 style strings */
export function unescapeQuadString(s: string): string {
  s = s.replaceAll(MATCH_QUAD_ESCAPE, (quad) => unescapeOneQuadString(quad));
  return s;
}


/**
 * Unescapes a string according to UTS#18§1.1, see <https://www.unicode.org/reports/tr18/#Hex_notation>
 * @param s escaped string
 * @returns
 */
export function unescapeString(s: string): string {
  if (!s) {
    return s;
  }
  try {
    /**
     * process one regex match
     * @param str ignored
     * @param matched the entire match such as '0127' or '22 22'
     * @returns the unescaped match
     */
    function processMatch(str: string, matched: string): string {
      const codepoints = matched.split(' ');
      const unescaped = codepoints.map(unescapeOne);
      return unescaped.join('');
    }
    s = s.replaceAll(MATCH_HEX_ESCAPE, processMatch);
  } catch (e) {
    if (e instanceof RangeError) {
      throw new UnescapeError(`Out of range while unescaping '${s}': ${e.message}`, { cause: e });
      /* c8 ignore next 3 */
    } else {
      throw e; // pass through some other error
    }
  }
  return s;
}

/** 0000 … FFFF */
export function hexQuad(n: number): string {
  if (n < 0x0000 || n > 0xFFFF) {
    throw RangeError(`${n} not in [0x0000,0xFFFF]`);
  }
  return n.toString(16).padStart(4, '0');
}

/** 00000000 … FFFFFFFF */
export function hexOcts(n: number): string {
  if (n < 0x0000 || n > 0xFFFFFFFF) {
    throw RangeError(`${n} not in [0x00000000,0xFFFFFFFF]`);
  }
  return n.toString(16).padStart(8, '0');
}

/** escape one char for regex in \uXXXX form */
export function escapeRegexChar(ch: string) {
  const code = ch.codePointAt(0);
  if (code <= 0xFFFF) {
    return '\\u' + hexQuad(code);
  } else {
    return '\\U' + hexOcts(code);
  }
}

/** chars that must be escaped: syntax, C0 + C1 controls */
const REGEX_SYNTAX_CHAR = /^[\u0000-\u001F\u007F-\u009F{}\[\]\\?|.^$*()/+-]$/;

function escapeRegexCharIfSyntax(ch: string) {
  // escape if syntax or not valid
  if (REGEX_SYNTAX_CHAR.test(ch) || !isValidUnicode(ch.codePointAt(0))) {
    return escapeRegexChar(ch);
  } else {
    return ch; // leave unescaped
  }
}

/**
 * Unescape one codepoint to \u or \U format
 * @param hex one codepoint in hex, such as '0127'
 * @returns the unescaped codepoint
 */
function regexOne(hex: string): string {
  const unescaped = unescapeOne(hex);
  // re-escape as 16 or 32 bit code units
  return Array.from(unescaped).map(ch => escapeRegexCharIfSyntax(ch)).join('');
}
/**
 * Escape a string (\uxxxx form) if there are any problematic codepoints
 */
export function escapeStringForRegex(s: string): string {
  return s.split('').map(ch => escapeRegexCharIfSyntax(ch)).join('');
}

/**
 * Unescapes a string according to UTS#18§1.1, see <https://www.unicode.org/reports/tr18/#Hex_notation>
 * @param s escaped string
 * @returns
 */
export function unescapeStringToRegex(s: string): string {
  if (!s) {
    return s;
  }
  try {
    /**
     * process one regex match
     * @param str ignored
     * @param matched the entire match such as '0127' or '22 22'
     * @returns the unescaped match
     */
    function processMatch(str: string, matched: string): string {
      const codepoints = matched.split(' ');
      const unescaped = codepoints.map(regexOne);
      return unescaped.join('');
    }
    s = s.replaceAll(MATCH_HEX_ESCAPE, processMatch);
  } catch (e) {
    if (e instanceof RangeError) {
      throw new UnescapeError(`Out of range while unescaping '${s}': ${e.message}`, { cause: e });
      /* c8 ignore next 3 */
    } else {
      throw e; // pass through some other error
    }
  }
  return s;
}

/** True if this string *could* be a UTF-32 single char */
export function
  isOneChar(value: string): boolean {
  return [...value].length === 1;
}

export function
  toOneChar(value: string): number {
  if (!isOneChar(value)) {
    throw Error(`Not a single char: ${value}`);
  }
  return value.codePointAt(0);
}

export function describeCodepoint(ch: number): string {
  let s;
  const p = BadStringAnalyzer.getProblem(ch);
  if (p != null) {
    // for example: 'PUA (U+E010)'
    s = p;
  } else {
    // for example: '"a" (U+61)'
    s = `"${String.fromCodePoint(ch)}"`;
  }
  return `${s} (U+${Number(ch).toString(16).toUpperCase()})`;
}

/**
 * @brief  function to convert a Html-Entity Hex value to a unicode character e.g. &#x63 -> c;  &#x1F60E; -> 😎
 * @param  hex_Entity the value that will converted
 * @return a unicode character or undefined if in_str is not recognized
 */
export function decodeHexEntity(hex_Entity: string) {
  const hex = hex_Entity.replace('&#x', '').replace(';', '');
  if (hex_Entity.length > 9)
    return undefined;
  else
    return String.fromCodePoint(parseInt(hex, 16));
}

/**
 * @brief  function to convert a Html-Entity decimal value value to a unicode character e.g. &#99 -> c;  &#128526; -> 😎
 * @param  dec_Entity the value that will converted
 * @return a unicode character or undefined if in_str is not recognized
 */
export function decodeDecEntity(dec_Entity: string) {
  const dec = dec_Entity.replace('&#', '').replace(';', '');
  if (dec_Entity.length > 9)
    return undefined;
  else
    return String.fromCodePoint(parseInt(dec, 10));
}

/**
 * @brief  function to convert a unicode hex value to a unicode character e.g. U+0063 -> c;  U+1F60E; -> 😎
 * @param  Uni_Val the value that will converted
 * @return a unicode character or undefined if in_str is not recognized
 */
export function decodeUniHexValue(Uni_Val: string): any {
  const hexval = Uni_Val.replace('U+', '0x');
  if (Uni_Val.length > 8)
    return undefined;
  else
    return String.fromCodePoint(parseInt(hexval, 16));
}

/**
 * @brief  function to convert a numeric character reference or a unicode value to a unicode character e.g. &#x63 -> c;  U+1F60E -> 😎
 * @param  in_str the value that will converted
 * @return a unicode character like 'c', 'ሴ', '😎' or undefined if in_str is not recognized
 */
export function convertToUnicodeCharacter(in_str: string): string {

  if ((in_str === null) || (in_str === undefined)) {
    return undefined;
  }

  else if (in_str.substring(0, 3) === "&#x") {
    return decodeHexEntity(in_str);
  }

  else if (in_str.substring(0, 2) === "&#") {
    return decodeDecEntity(in_str);
  }

  else if (in_str.substring(0, 1) === "&") {
    return undefined;
  }

  else if (in_str.substring(0, 2) === "U+") {
    return decodeUniHexValue(in_str);
  }

  else if ([...new Intl.Segmenter().segment(in_str)].length <= 1) {
    return in_str;
  }
  else {
    return undefined;
  }
}
/**
 * @brief  function to convert a numeric character reference to a unicode Code Point e.g. &#4660 -> U+1234;  &#x10F601 -> U+1F60E
 * @param  instr the value that will converted
 * @return returns a unicode Code Point like U+0063, U+1234, U+1F60E; returns the input character if a non-numeric reference is used or returns 'undefined' if instr is not recognized
 */
export function convertToUnicodeCodePoint(instr: string): string {

  if ((instr === null) || (instr === undefined)) {
    return undefined;
  }

  if (instr.substring(0, 3) === "&#x") {
    const num_length = instr.length - instr.indexOf("x") - 1;
    const num_str = instr.substring(instr.indexOf("x") + 1, instr.length - 1);
    return ("U+" + num_str.slice(-num_length).padStart(4, "0"));
  }

  // if not hex: convert to hex
  if ((instr.substring(0, 2) === "&#")) {
    const num_length = instr.length - instr.indexOf("#") - 1;
    const num_str = instr.substring(instr.indexOf("#") + 1, instr.length - 1);
    return "U+" + Number(num_str.slice(-num_length)).toString(16).slice(-6).toUpperCase().padStart(4, "0");
  }
  else
    return instr;
}

export enum BadStringType {
  pua = 'PUA',
  unassigned = 'Unassigned',
  illegal = 'Illegal',
  denormalized = "Denormalized"
};

// Following from kmx_xstring.h / .cpp

const Uni_LEAD_SURROGATE_START = 0xD800;
const Uni_LEAD_SURROGATE_END = 0xDBFF;
const Uni_TRAIL_SURROGATE_START = 0xDC00;
const Uni_TRAIL_SURROGATE_END = 0xDFFF;
const Uni_SURROGATE_START = Uni_LEAD_SURROGATE_START;
const Uni_SURROGATE_END = Uni_TRAIL_SURROGATE_END;
const Uni_FD_NONCHARACTER_START = 0xFDD0;
const Uni_FD_NONCHARACTER_END = 0xFDEF;
const Uni_FFFE_NONCHARACTER = 0xFFFE;
const Uni_PLANE_MASK = 0x1F0000;
const Uni_MAX_CODEPOINT = 0x10FFFF;
// plane 0, 15, and 16 PUA
const Uni_PUA_00_START = 0xE000;
const Uni_PUA_00_END = 0xF8FF;
const Uni_PUA_15_START = 0x0F0000;
const Uni_PUA_15_END = 0x0FFFFD;
const Uni_PUA_16_START = 0x100000;
const Uni_PUA_16_END = 0x10FFFD;


/**
 * @brief True if a lead surrogate
 * \def Uni_IsSurrogate1
 */
export function Uni_IsSurrogate1(ch: number) {
  return ((ch) >= Uni_LEAD_SURROGATE_START && (ch) <= Uni_LEAD_SURROGATE_END);
}
/**
 * @brief True if a trail surrogate
 * \def Uni_IsSurrogate2
 */
export function Uni_IsSurrogate2(ch: number) {
  return ((ch) >= Uni_TRAIL_SURROGATE_START && (ch) <= Uni_TRAIL_SURROGATE_END);
}

/**
 * @brief True if any surrogate
 * \def UniIsSurrogate
*/
export function Uni_IsSurrogate(ch: number) {
  return (Uni_IsSurrogate1(ch) || Uni_IsSurrogate2(ch));
}

function Uni_IsEndOfPlaneNonCharacter(ch: number) {
  return (((ch) & Uni_FFFE_NONCHARACTER) == Uni_FFFE_NONCHARACTER); // matches FFFF or FFFE
}

function Uni_IsNoncharacter(ch: number) {
  return (((ch) >= Uni_FD_NONCHARACTER_START && (ch) <= Uni_FD_NONCHARACTER_END) || Uni_IsEndOfPlaneNonCharacter(ch));
}

function Uni_InCodespace(ch: number) {
  return (ch >= 0 && ch <= Uni_MAX_CODEPOINT);
};

function Uni_IsValid1(ch: number) {
  return (Uni_InCodespace(ch) && !Uni_IsSurrogate(ch) && !Uni_IsNoncharacter(ch));
}

export function isValidUnicode(start: number, end?: number) {
  if (!end) {
    // single char
    return Uni_IsValid1(start);
  } else if (!Uni_IsValid1(end) || !Uni_IsValid1(start) || (end < start)) {
    // start or end out of range, or inverted range
    return false;
  } else if ((start <= Uni_SURROGATE_END) && (end >= Uni_SURROGATE_START)) {
    // contains some of the surrogate range
    return false;
  } else if ((start <= Uni_FD_NONCHARACTER_END) && (end >= Uni_FD_NONCHARACTER_START)) {
    // contains some of the noncharacter range
    return false;
  } else if ((start & Uni_PLANE_MASK) != (end & Uni_PLANE_MASK)) {
    // start and end are on different planes, meaning that the U+__FFFE/U+__FFFF noncharacters
    // are contained.
    // As a reminder, we already checked that start/end are themselves valid,
    // so we know that 'end' is not on a noncharacter at end of plane.
    return false;
  } else {
    return true;
  }
}

export function isPUA(ch: number) {
  return ((ch >= Uni_PUA_00_START && ch <= Uni_PUA_00_END) ||
    (ch >= Uni_PUA_15_START && ch <= Uni_PUA_15_END) ||
    (ch >= Uni_PUA_16_START && ch <= Uni_PUA_16_END));
}

/** @returns false if s is NEITHER NFC nor NFD. (Returns true for falsy) */
export function isNormalized(s: string): boolean {
  if (!s) return true; // empty or null
  const nfc = s.normalize("NFC");
  const nfd = s.normalize("NFD");
  if (s !== nfc && s !== nfd) return false;
  return true;
}

class BadStringMap extends Map<BadStringType, Set<number>> {
  public toString(): string {
    if (!this.size) {
      return "{}";
    }
    return Array.from(this.entries()).map(([t, s]) => `${t}: ${Array.from(s.values()).map(describeCodepoint).join(' ')}`).join(', ');
  }
}

/** abstract class for analyzing and categorizing strings */
export abstract class StringAnalyzer {
  /** add a string for analysis */
  public add(s: string) {
    for (const c of [...s]) {
      const ch = c.codePointAt(0);
      const problem = this.analyzeCodePoint(c, ch);
      if (problem) {
        this.addProblem(ch, problem);
      }
    }
  }

  /**
   * subclass interface
   * @param c single codepoint to analyze (string)
   * @param ch single codepoint to analyze (scalar)
   */
  protected abstract analyzeCodePoint(c: string, ch: number): BadStringType;

  /** internal interface for the result of an analysis */
  protected addProblem(ch: number, type: BadStringType) {
    if (!this.m.has(type)) {
      this.m.set(type, new Set<number>());
    }
    this.m.get(type).add(ch);
  }

  /** get the results of the analysis */
  public analyze(): BadStringMap {
    if (this.m.size == 0) {
      return null;
    } else {
      return this.m;
    }
  }

  /** internal map */
  private m = new BadStringMap();
}

/** analyze a string looking for bad unicode */
export class BadStringAnalyzer extends StringAnalyzer {
  /** analyze one codepoint */
  protected analyzeCodePoint(c: string, ch: number): BadStringType {
    return BadStringAnalyzer.getProblem(ch);
  }
  /** export analyzer function  */
  public static getProblem(ch: number) {
    if (!isValidUnicode(ch)) {
      return BadStringType.illegal;
    } else if (isPUA(ch)) {
      return BadStringType.pua;
    } else { // TODO-LDML: unassigned
      return null;
    }
  }
}

/** Analyzer that checks if something isn't NFD */
export class NFDAnalyzer extends StringAnalyzer {
  protected analyzeCodePoint(c: string, ch: number): BadStringType {
    const nfd = c.normalize("NFD");
    if (c !== nfd) {
      return BadStringType.denormalized;
    } else {
      return null;
    }
  }
}
