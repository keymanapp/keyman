/**
 * xml2js will not place single-entry objects into arrays. Easiest way to fix
 * this is to box them ourselves as needed. Ensures that o.x is an array.
 *
 * @param o Object with property to box
 * @param x Name of element to box
 */
export function boxXmlArray(o: any, x: string): void {
  if(typeof o == 'object' && !Array.isArray(o[x])) {
    if(o[x] === null || o[x] === undefined) {
      o[x] = [];
    }
    else {
      o[x] = [o[x]];
    }
  }
}

// TODO-LDML: #7569 the below regex works, but captures more than it should
// (it would include \u{fffffffffffffffff } which
// is overlong and has a space at the end.) The second regex does not work yet.
export const MATCH_HEX_ESCAPE = /\\u{([0-9a-fA-F ]{1,})}/g;
// const MATCH_HEX_ESCAPE = /\\u{((?:(?:[0-9a-fA-F]{1,5})|(?:10[0-9a-fA-F]{4})(?: (?!}))?)+)}/g;

/** regex for single quad escape such as \u0127 */
export const MATCH_QUAD_ESCAPE = /\\u([0-9a-fA-F]{4})/g;

export class UnescapeError extends Error {
}

/**
 * Unescape one codepoint
 * @param hex one codepoint in hex, such as '0127'
 * @returns the unescaped codepoint
 */
function unescapeOne(hex: string): string {
  const codepoint = Number.parseInt(hex, 16);
  return String.fromCodePoint(codepoint);
}

/**
 * Unescape one single quad string such as \u0127.
 * Throws exception if the string doesn't match MATCH_QUAD_ESCAPE
 * @param s input string
 * @returns output
 */
export function unescapeOneQuadString(s: string): string {
  if (!s || !s.match(MATCH_QUAD_ESCAPE)) {
    throw new UnescapeError(`Not a quad escape: ${s}`);
  }
  function processMatch(str: string, matched: string): string {
    return unescapeOne(matched);
  }
  s = s.replace(MATCH_QUAD_ESCAPE, processMatch);
  return s;
}

/**
 * Unescapes a string according to UTS#18§1.1, see <https://www.unicode.org/reports/tr18/#Hex_notation>
 * @param s escaped string
 * @returns
 */
export function unescapeString(s: string): string {
  if(!s) {
    return s;
  }
  try {
    /**
     * process one regex match
     * @param str ignored
     * @param matched the entire match such as '0127' or '22 22'
     * @returns the unescaped match
     */
    function processMatch(str: string, matched: string) : string {
      const codepoints = matched.split(' ');
      const unescaped = codepoints.map(unescapeOne);
      return unescaped.join('');
    }
    s = s.replaceAll(MATCH_HEX_ESCAPE, processMatch);
  } catch(e) {
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
isOneChar(value: string) : boolean {
  return [...value].length === 1;
}

export function
toOneChar(value: string) : number {
  if (!isOneChar(value)) {
    throw Error(`Not a single char: ${value}`);
  }
  return value.codePointAt(0);
}

export function describeCodepoint(ch : number) : string {
  const s = isValidUnicode(ch) ? String.fromCodePoint(ch) : "INVALID";
  return `"${s}" (U+${Number(ch).toString(16)})`;
}

export enum BadStringType {
  pua = 'pua',
  unassigned = 'unassigned',
  illegal = 'illegal',
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

/**
 * @brief True if a lead surrogate
 * \def Uni_IsSurrogate1
 */
function Uni_IsSurrogate1(ch : number) {
  return ((ch) >= Uni_LEAD_SURROGATE_START && (ch) <= Uni_LEAD_SURROGATE_END);
}
/**
 * @brief True if a trail surrogate
 * \def Uni_IsSurrogate2
 */
function Uni_IsSurrogate2(ch : number) {
  return ((ch) >= Uni_TRAIL_SURROGATE_START && (ch) <= Uni_TRAIL_SURROGATE_END);
}

/**
 * @brief True if any surrogate
 * \def UniIsSurrogate
*/
function Uni_IsSurrogate(ch : number) {
  return (Uni_IsSurrogate1(ch) || Uni_IsSurrogate2(ch));
}

function Uni_IsEndOfPlaneNonCharacter(ch : number) {
  return (((ch) & Uni_FFFE_NONCHARACTER) == Uni_FFFE_NONCHARACTER); // matches FFFF or FFFE
}

function Uni_IsNoncharacter(ch : number) {
  return (((ch) >= Uni_FD_NONCHARACTER_START && (ch) <= Uni_FD_NONCHARACTER_END) || Uni_IsEndOfPlaneNonCharacter(ch));
}

function Uni_InCodespace(ch : number) {
  return ((ch) <= Uni_MAX_CODEPOINT);
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
  return ((ch >= 0xE000 && ch <= 0xF8FF) ||
    (ch >= 0xF0000 && ch <= 0xFFFFD) ||
    (ch >= 0x100000 && ch <= 0x10FFFD));
}

class BadStringMap extends Map<BadStringType, Set<number>> {
  public toString() : string {
    if (!this.size) {
      return "{}";
    }
    return Array.from(this.entries()).map(([t, s]) => `${t}: ${Array.from(s.values()).map(describeCodepoint).join(' ')}`).join(', ');
  }
}

function getProblem(ch : number) : BadStringType {
  if (!isValidUnicode(ch)) {
    return BadStringType.illegal;
  } else if(isPUA(ch)) {
    return BadStringType.pua;
  } else { // TODO-LDML: unassigned
    return null;
  }
}

export class BadStringAnalyzer {
  /** add a string for analysis */
  public add(s : string) {
    for (const c of s) {
      const ch = c.codePointAt(0);
      const problem = getProblem(ch);
      if (problem) {
        this.addProblem(ch, problem);
      }
    }
  }

  private addProblem(ch : number, type : BadStringType) {
    if (!this.m.has(type)) {
      this.m.set(type, new Set<number>());
    }
    this.m.get(type).add(ch);
  }

  public analyze() : BadStringMap {
    if (this.m.size == 0) {
      return null;
    } else {
      return this.m;
    }
  }

  private m = new BadStringMap();
}
