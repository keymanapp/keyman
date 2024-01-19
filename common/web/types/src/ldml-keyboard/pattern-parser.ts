/**
 * Utilities for transform and marker processing
 */

import { constants } from "@keymanapp/ldml-keyboard-constants";
import { MATCH_QUAD_ESCAPE, isOneChar, unescapeOneQuadString, unescapeString, hexQuad } from "../util/util.js";


/**
 * Helper function for extracting matched items
 * @param str input string
 * @param match global RegEx to use
 * @returns array of matched values
 */
function matchArray(str: string, match: RegExp) : string[] {
  const refs = (str || '').matchAll(match);
  return Array.from(refs).map(r => r[1]);
}

/**
 * Common regex for an ID
 */
const COMMON_ID = /^[0-9A-Za-z_]{1,32}$/;

/** for use with markers, means an ordering can be determined */
export interface OrderedStringList {
  /** @returns the ordering of an item (0..), or -1 if not found */
  getItemOrder(item : string) : number;
}

/**
 * Class for helping with markers
 */
export class MarkerParser {
  /**
   * A marker id has the same constraint as a key id. TODO-LDML: Needs to be reflected in the spec
   */
  public static readonly ID = COMMON_ID;

  /**
   * Special marker reference referring to any marker
   */
  public static readonly ANY_MARKER = '\\m{.}';

  /**
   * id of the 'any' marker
   */
  public static readonly ANY_MARKER_ID = '.';

  /**
   * Marker sentinel as a string - U+FFFF
   */
  public static readonly SENTINEL = String.fromCodePoint(constants.uc_sentinel);
  static readonly SENTINEL_MATCH = '\\u' + hexQuad(constants.uc_sentinel);
  /**
   * Marker code as a string - U+0008
   */
  public static readonly MARKER_CODE = String.fromCodePoint(constants.marker_code);
  static readonly MARKER_CODE_MATCH = '\\u' + hexQuad(constants.marker_code);

  /** Minimum ID (trailing code unit) */
  public static readonly MIN_MARKER_INDEX = constants.marker_min_index;
  /** Index meaning 'any marker' == `\m{.}` */
  public static readonly ANY_MARKER_INDEX = constants.marker_any_index;
  /** Maximum usable marker index */
  public static readonly MAX_MARKER_INDEX = constants.marker_max_index;
  /** Max count of markers */
  public static readonly MAX_MARKER_COUNT = constants.marker_max_count;

  private static anyMarkerMatch() : string {
    const start = hexQuad(this.MIN_MARKER_INDEX);
    const end   = hexQuad(this.MAX_MARKER_INDEX);
    return `${this.SENTINEL_MATCH}${this.MARKER_CODE_MATCH}[\\u${start}-\\u${end}]`; // TODO-LDML: #9121 wrong escape format
  }

  /** Expression that matches any marker */
  public static readonly ANY_MARKER_MATCH = MarkerParser.anyMarkerMatch();

  /**
   * Pattern for matching a marker reference, OR the special marker \m{.}
   */
  public static readonly REFERENCE = /\\m{([0-9A-Za-z_]{1,32}|\.)}/g;

  /**
   * parse a string into marker references
   * @param str input string such as "\m{a} … \m{.}"
   * @returns `[]` or an array of all markers referenced
   */
  public static allReferences(str: string): string[] {
    if (!str) {
      return [];
    }
    return matchArray(str, this.REFERENCE);
  }

  private static markerCodeToString(n: number, forMatch?: boolean): string {
    if (!forMatch) {
      return String.fromCharCode(n);
    } else {
      return `\\u${hexQuad(n)}`; // TODO-LDML: #9121 wrong escape format
    }
  }

  /** @returns string for marker #n */
  public static markerOutput(n: number, forMatch?: boolean): string {
    if (n < MarkerParser.MIN_MARKER_INDEX || n > MarkerParser.ANY_MARKER_INDEX) {
      throw RangeError(`Internal Error: marker index out of range ${n}`);
    }
    if (forMatch) {
      return this.SENTINEL_MATCH + this.MARKER_CODE_MATCH + this.markerCodeToString(n, forMatch);
    } else {
      return this.SENTINEL + this.MARKER_CODE + this.markerCodeToString(n, forMatch);
    }
  }

  /** @returns all marker strings as sentinel values */
  public static toSentinelString(s: string, markers?: OrderedStringList, forMatch?: boolean) : string {
    if (!s) return s;
    return s.replaceAll(this.REFERENCE, (sub, arg) => {
      if (arg === MarkerParser.ANY_MARKER_ID) {
        if (forMatch) {
          return this.ANY_MARKER_MATCH;
        }
        return MarkerParser.markerOutput(MarkerParser.ANY_MARKER_INDEX);
      }
      if (!markers) {
        throw RangeError(`Internal Error: Could not find marker \\m{${arg}} (no markers defined)`);
      }
      const order = markers.getItemOrder(arg);
      if (order === -1) {
        throw RangeError(`Internal Error: Could not find marker \\m{${arg}}`);
      } else if(order > MarkerParser.MAX_MARKER_INDEX) {
        throw RangeError(`Internal Error: marker \\m{${arg}} has out of range index ${order}`);
      } else {
        return MarkerParser.markerOutput(order + 1, forMatch);
      }
    });
  }

  /**
   * NFD a string, respecting markers.
   * @param s input string
   * @param forMatch true if regex, false if individual
   * @returns the normalized string
   */
  public static nfd_markers(s: string, forMatch?: boolean) : string {
    const m : MarkerMap = [];
    return MarkerParser.nfd_markers_segment(s, m, forMatch);
  }

  public static nfd_markers_segment(s: string, map: MarkerMap, forMatch?: boolean) : string {
    return s.normalize("NFD");
  }


  public static remove_markers(s: string, map: MarkerMap, forMatch?: boolean) : string {
    let out : string = '';
    let last_markers : number[] = [];
    let a : string[] = [...s];
    function add_pending_markers(l : string) {
      // first char, or, marker-before-eot
      const ch = (l === '') ? MARKER_BEFORE_EOT : [...l][0];
      while(last_markers.length) {
        const marker = last_markers[0];
        last_markers = last_markers.slice(1); // pop from front
        map.push({ ch, marker });
      }
    }
    while (a.length > 0) {
      const p = this.parse_next_marker(a.join(''), forMatch);
      if (!p?.match) {
        // no match
        add_pending_markers(a[0]); // add any pending markers
        out = out + a[0]; // add the non-marker text
        a = a.slice(1); // move forward 1 char
    } else {
        const { marker, match } = p;
        if (marker >= constants.marker_min_index) {
          last_markers.push(marker);
        }
        a = a.slice([...match].length); // skip over matched marker
      }
    }
    add_pending_markers('');
    return out;
  }

  public static parse_next_marker(s: string, forMatch?: boolean) : MarkerResult {
    if(!forMatch) {
      const m = s.match(PARSE_SENTINEL_MARKER);
      if (m) {
        const match = m[0];
        const marker = match.codePointAt(2);
        return ({ match, marker });
      }
    } else {
      const m = s.match(PARSE_REGEX_MARKER);
      if (m) {
        const match = m[0];
        const single = m[1];
        if (single) {
          return ({ match, marker: Number.parseInt(single.substring(3), 16) });
        } else {
          return ({ match, marker: constants.marker_any_index });
        }
      }
    }
    return null;
  }
};

const MARKER_BEFORE_EOT = '\ufffe';
const PARSE_SENTINEL_MARKER = new RegExp(`^${MarkerParser.ANY_MARKER_MATCH}`);
const PARSE_REGEX_MARKER    = /^\\uffff\\u0008(?:(\\u[0-9a-fA-F]{4})|(\[\\u[0-9a-fA-F]{4}-\\u[0-9a-fA-F]{4}\]))/;

export interface MarkerEntry {
  ch? : string;
  marker? : number;
};

export type MarkerMap = Array<MarkerEntry>;

export interface MarkerResult {
  // if matched, the number of the marker. or falsy
  marker?: number;
  // if matched, the entire marker sequence
  match?: string;
};

/**
 * Class for helping with markers
 */
export class VariableParser {
  /**
   * A marker id has the same constraint as a key id. TODO-LDML: Needs to be reflected in the spec
   */
  public static readonly ID = COMMON_ID;

  /**
   * Pattern for matching a string reference `$(str)`
   */
  public static readonly STRING_REFERENCE = /\${([0-9A-Za-z_]{1,32})}/g;

  /**
   * Pattern for matching a set reference `$[set]`
   */
  public static readonly SET_REFERENCE = /\$\[([0-9A-Za-z_]{1,32})\]/g;

  /**
   * Pattern for matching a capture set reference `($[set])`
   */
  public static readonly CAPTURE_SET_REFERENCE = /\(\$\[([0-9A-Za-z_]{1,32})\]\)/g;

  /**
   * `$[1:variable]`
   * This regex matches the whole string.
   */
  public static readonly MAPPED_SET_REFERENCE = /^\$\[1:([0-9A-Za-z_]{1,32})\]$/;

  /**
   * parse a string into references
   * @param str input string
   * @returns `[]` or an array of all string references referenced
   */
  public static allStringReferences(str: string): string[] {
    return matchArray(str, this.STRING_REFERENCE);
  }

  /**
   * parse a string into references
   * @param str input string
   * @returns `[]` or an array of all string references referenced
   */
  public static allSetReferences(str: string): string[] {
    return matchArray(str, this.SET_REFERENCE);
  }

  /**
   * Split an input string into a proper set
   * @param str input string
   * @returns
   */
  public static setSplitter(str: string): string[] {
    const s = str?.trim();
    if (!s) return [];
    return s.split(/\s+/);
  }
}

/** for ElementParser.segment() */
export enum ElementType {
  codepoint = '.',
  escaped ='\\',
  uset = '[',
  string = '*',
};

/** one portion of a segmented element string */
export class ElementSegment {
  public readonly type: ElementType;
  /**
   * @param segment the string in the segment
   * @param type type of segment. Will be calculated if not provided.
   */
  constructor(public segment: string, type?: ElementType) {
    if (type) {
      this.type = type;
    } else if (ElementParser.MATCH_USET.test(segment)) {
      this.type = ElementType.uset;
    } else if(ElementParser.MATCH_ESCAPED.test(segment)) {
      this.type = ElementType.escaped;
    } else {
      this.type = ElementType.codepoint;
    }
  }

  /** unescaped format */
  get unescaped() : string {
    if (this.type !== ElementType.escaped) {
      return this.segment;
    } else {
      if (MATCH_QUAD_ESCAPE.test(this.segment)) {
        return unescapeOneQuadString(this.segment);
      } else {
        return unescapeString(this.segment);
      }
    }
  }
};

/** Class for helping with Element strings (i.e. reorder) */
export class ElementParser {
  /**
   * Matches any complex UnicodeSet that would otherwise be misinterpreted
   * by `MATCH_ELEMENT_SEGMENTS` due to nested `[]`'s.
   * For example, `[[a-z]-[aeiou]]` could be
   * mis-segmented into `[[a-z]`, `-`, `[aeiou]`, `]` */
  public static readonly MATCH_NESTED_SQUARE_BRACKETS = /\[[^\]]*\[/;

  /** Match (segment) UnicodeSets OR hex escapes OR single Unicode codepoints */
  public static readonly MATCH_ELEMENT_SEGMENTS =
    /(?:\[[^\]]*\]|\\u[0-9a-fA-F]{4}|\\u\{[0-9a-fA-F]{1,6}\}|\\u\{(?:[0-9a-fA-F]{1,6})(?: [0-9a-fA-F]{1,6}){1,}\}|.)/gu;

  /** Does it start with a UnicodeSet? Used to test the segments. */
  public static readonly MATCH_USET = /^\[/;

  /** Does it start with an escaped char? Used to test the segments. */
  public static readonly MATCH_ESCAPED = /^\\u/;

  /** Split a string into ElementSegments */
  public static segment(str: string): ElementSegment[] {
    if (this.MATCH_NESTED_SQUARE_BRACKETS.test(str)) {
      throw Error(`Unsupported: nested square brackets in element segment: ${str}`);
    }
    const list: ElementSegment[] = [];
    for(let m of str.match(ElementParser.MATCH_ELEMENT_SEGMENTS)) {
      const e = new ElementSegment(m);
      if (e.type === ElementType.escaped) {
        // unescape
        const { unescaped } = e;
        if (isOneChar(unescaped)) {
          list.push(e);
        } else {
          // need to split the escaped segment, \u{41 42} -> \u{41}, \u{42}
          for (let s of unescaped) {
            list.push(new ElementSegment(`\\u{${s.codePointAt(0).toString(16)}}`));
          }
        }
      } else {
        // all others
        list.push(e);
      }
    }
    return list;
  }
};
