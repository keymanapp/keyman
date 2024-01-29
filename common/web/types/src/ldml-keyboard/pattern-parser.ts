/**
 * Utilities for transform and marker processing
 */

import { constants } from "@keymanapp/ldml-keyboard-constants";
import { MATCH_QUAD_ESCAPE, isOneChar, unescapeOneQuadString, unescapeString, hexQuad } from "../util/util.js";
// see comment in file - generated file
import { nfdNoBoundaryBefore } from "./nfd-table.js";
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
  /** Marker sentinel as a regex match */
  static readonly SENTINEL_MATCH = '\\u' + hexQuad(constants.uc_sentinel);
  /**
   * Marker code as a string - U+0008
   */
  public static readonly MARKER_CODE = String.fromCodePoint(constants.marker_code);
  /** Marker code as a regex match */
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
    const start = hexQuad(MarkerParser.MIN_MARKER_INDEX);
    const end   = hexQuad(MarkerParser.MAX_MARKER_INDEX);
    return `${MarkerParser.SENTINEL_MATCH}${MarkerParser.MARKER_CODE_MATCH}[\\u${start}-\\u${end}]`; // TODO-LDML: #9121 wrong escape format
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
    return matchArray(str, MarkerParser.REFERENCE);
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
      return MarkerParser.SENTINEL_MATCH + MarkerParser.MARKER_CODE_MATCH + MarkerParser.markerCodeToString(n, forMatch);
    } else {
      return MarkerParser.SENTINEL + MarkerParser.MARKER_CODE + MarkerParser.markerCodeToString(n, forMatch);
    }
  }

  /** @returns all marker strings as sentinel values */
  public static toSentinelString(s: string, markers?: OrderedStringList, forMatch?: boolean) : string {
    if (!s) return s;
    return s.replaceAll(MarkerParser.REFERENCE, (sub, arg) => {
      if (arg === MarkerParser.ANY_MARKER_ID) {
        if (forMatch) {
          return MarkerParser.ANY_MARKER_MATCH;
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
    // Note: parallel to ldml::normalize_nfd_markers()
    if(!s) return s;
    /** Accumulate output normalized string */
    let out = '';
    /** 's' split into code point units */
    let a = [...s];
    /** index into `a` where the current segment starts */
    let seg_start = 0;
    /** index into 'a' following end of current segment */
    let seg_end = 0;
    /** iterator over 'a' */
    let i = 0;
    /** for parallelism with C++, end of string */
    let str_end = a.length;

    /** map of all markers in the string */
    const mall : MarkerMap = [];
    // preprocess string, also check for markers
    const all_no_markers = MarkerParser.remove_markers(s, mall, forMatch);
    if(mall.length == 0) {
      // no markers, so just normalize as a single segment
      return all_no_markers.normalize("NFD");
    }

    // now we'll loop through looking for normalization-safe subsegments of [seg_start, seg_end)
    // For example (sentinel mode) the following would be segments:
    //     - A
    //     - E\u0320\u0302
    //     - U\u0320\m{marker}\u0300
    //
    // The marker will typically 'look' like an NFD-safe boundary, but it's not! It's part of the
    // subsegment, we want to skip over it. This is why we use parse_next_marker to look-ahead to see
    // if there is actually a marker under the iterator.
    //
    // We don't assume that the marker sentinel or regex appears as an NFD boundary, that is why
    // the parse function and the nfd function are called in parallel.

    do {
      /** remainder of string i..end, for match */
      const rest = a.slice(i).join('');
      const p = MarkerParser.parse_next_marker(rest, forMatch);
      const have_marker = !!(p?.match);
      const has_nfd_boundary_before = (nfdNoBoundaryBefore.indexOf(a[i]?.codePointAt(0)) === -1);

      // First, categorize the current character.

      if (i === str_end) {
        // at end of string, so this is also the end of the segment.
        seg_end = i;
      } else if (has_nfd_boundary_before && !have_marker) {
        // it's an NFD safe boundary, but NOT a marker
        // (if it were a marker, we would fall through to consume the marker)
        // segment ends before this code point
        seg_end = i;
        // move the index past the NFD boundary
        i++;
      } else if(have_marker) {
        // we have a marker. move the index past the marker.
        i += [...p.match].length;
      } else {
        // non boundary. just move the index forward
        i++;
      }

      // Next, if we found a segment boundary (which includes
      // end of string) process it

      if(seg_end !== seg_start) {
        // re-join the segment from the iterated code points
        const segment = a.slice(seg_start, seg_end).join('');
        const m : MarkerMap = [];
        // process the subsegment
        const segment_nfd = MarkerParser.nfd_markers_segment(segment, m, forMatch);
        // append the processed segment to the output buffer
        out = out + segment_nfd;
        // the end of this segment is the start of the next one.
        seg_start = seg_end;
      }
    } while (seg_end != str_end); // until the last codepoint processed
    return out;
  }

  /**
   * NFD a safe subset of a string, respecting markers
   * @param s input string
   * @param map output array of marker chars
   * @param forMatch true if used for regexes
   * @returns the updated string
   */
  public static nfd_markers_segment(s: string, map: MarkerMap, forMatch?: boolean) : string {
    // remove (and parse) the markers first
    const str_unmarked = MarkerParser.remove_markers(s, map, forMatch);
    // then, NFD the normalized string
    const str_unmarked_nfd = str_unmarked.normalize("NFD");
    if(map.length == 0) {
      // no markers, so we can safely return the normalized unmarked string
      return str_unmarked_nfd;
    } else if(str_unmarked_nfd === str_unmarked) {
      // normalization didn't shuffle anything, so it's entirely a no-op.
      return s;
    } else {
      // we had markers AND the normalization made a difference.
      // add the markers back per the map, and return
      return MarkerParser.add_back_markers(str_unmarked_nfd, map, forMatch);
    }
  }

  /** return the string s but with a marker sequence before it */
  public static prepend_marker(s: string, marker: number, forMatch?: boolean) : string {
    if (forMatch && marker === constants.marker_any_index) {
      return MarkerParser.ANY_MARKER_MATCH + s;
    } else {
      return MarkerParser.markerOutput(marker, forMatch) + s;
    }
  }

  /**
   * Add back all markers in the map to the string
   * @param s input string
   * @param map output: the marker map
   * @param forMatch if true, use regex format
   */
  public static add_back_markers(s: string, map: MarkerMap, forMatch?: boolean) : string {
    // quick check: if no string, or no map: nothing to do
    if (!s || !map?.length) {
      return s;
    }
    /** output string */
    let out = '';
    /** for checking: the total number of markers expected */
    const max_markers = map.length;
    /** for checking: the number of markers we've written */
    let written_markers = 0;
    /** we are going to mutate the map, so copy it */
    const map2 : MarkerMap = [...map]; // make a copy
    // First, add back all 'MARKER_BEFORE_EOT' markers
    while (map2.length && map2[map2.length - 1].ch === MARKER_BEFORE_EOT) {
      // remove from list
      const { marker } = map2.pop();
      out = MarkerParser.prepend_marker(out, marker, forMatch);
      written_markers++;
    }
    // Then, take each codepoint (from back to front)
    for (let p of [...s].reverse()) {
      // reverse order code units, prepend to out
      out = p + out;

      // 1. we write any markers which match the output string in order,
      // and pop them off the list as dones
      while(map2.length > 0 && map2[map2.length-1].ch === p) {
        const { marker } = map2.pop();
        if (marker !== constants.marker_no_index) { // if not already written
          out = MarkerParser.prepend_marker(out, marker, forMatch);
          written_markers++;
          // no need to update .marker here, we're about to pop it
        }
      }
      // 2. now, any out of order markers. iterate with an index so we can record.
      // will skip this if map2.length < 2
      for (let i = map2.length - 2; i >= 0; i--) {
        const { ch, marker } = map2[i]; // don't pop here, as it might not be the right char
        if (ch === p && marker !== constants.marker_no_index) {
          out = MarkerParser.prepend_marker(out, marker, forMatch);
          written_markers++;
          map2[i].marker = constants.marker_no_index; // mark as written
        }
      }
    }
    // validate that we consumed all markers
    if(written_markers !== max_markers) {
      throw Error(`Internal Error: We should have written ${max_markers} markers but only wrote ${written_markers}`);
    }
    return out;
  }

  /**
   * Remove (and parse) markers from a string
   * @param s input string
   * @param map output map containing marker locations
   * @param forMatch true if regex
   * @returns the original string, without any markers
   */
  public static remove_markers(s: string, map: MarkerMap, forMatch?: boolean) : string {
    /** accumulated output */
    let out: string = '';
    /** array of marker ids in order waiting to be added */
    let last_markers: number[] = [];
    /** input string, split into codepoint runs */
    let a: string[] = [...s];

    /**
     * subfunc: add all markers in the pending (last_markers) queue
     * @param l string the marker is 'glued' to, or '' for end
     */
    function add_pending_markers(l: string): void {
      // first char, or, marker-before-eot. Must be first char of NFD sequence
      const ch = (l === '') ? MARKER_BEFORE_EOT : [...(l.normalize("NFD"))][0];
      while(last_markers.length) {
        const marker = last_markers[0];
        last_markers = last_markers.slice(1); // pop from front
        map.push({ ch, marker });
      }
    }

    // iterate until the codepoint list is empty
    while (a.length > 0) {
      // does 'a' begin with a marker?
      const p = MarkerParser.parse_next_marker(a.join(''), forMatch);
      if (!p?.match) {
        // no match
        add_pending_markers(a[0]); // add any pending markers
        out = out + a[0]; // add the non-marker text to the buffer
        a = a.slice(1); // move forward 1 codepoint
      } else {
        // found a marker
        const { marker, match } = p;
        if ((marker == constants.marker_any_index) ||
          (marker >= constants.marker_min_index && marker <= constants.marker_max_index)) {
          last_markers.push(marker);
        } else {
          throw RangeError(`String contained out-of-range marker ${marker}: '${s}'`);
        }
        a = a.slice([...match].length); // move forward over matched marker
      }
    }
    // finally, add any remaining markers at the end of the string
    add_pending_markers('');
    return out;
  }

  /**
   * analyze the string to see if it begins with a marker
   * @param s input string
   * @param forMatch true if regex
   * @returns parsed marker details
   */
  public static parse_next_marker(s: string, forMatch?: boolean) : MarkerResult {
    if(!forMatch) {
      // plain
      const m = s.match(PARSE_SENTINEL_MARKER);
      if (m) {
        // full string matched
        const match  = m[0];
        // extract the marker number
        const marker = match.codePointAt(2);
        return ({ match, marker });
      }
    } else {
      // regex
      const m = s.match(PARSE_REGEX_MARKER);
      if (m) {
        // full string
        const match =  m[0];
        // hex digit (if a single)
        const single = m[1];
        if (single) {
          return ({ match, marker: Number.parseInt(single.substring(3), 16) });
        } else {
          // it's a range, so it's an any match
          return ({ match, marker: constants.marker_any_index });
        }
      }
    }
    return null;
  }
};

/** special noncharacter value denoting end of string */
const MARKER_BEFORE_EOT = '\ufffe';
/** matcher for a sentinel */
const PARSE_SENTINEL_MARKER = new RegExp(`^${MarkerParser.ANY_MARKER_MATCH}`);
/** matcher for a regex marker, either single or any */
const PARSE_REGEX_MARKER    = /^\\uffff\\u0008(?:(\\u[0-9a-fA-F]{4})|(\[\\u[0-9a-fA-F]{4}-\\u[0-9a-fA-F]{4}\]))/;

export interface MarkerEntry {
  /** code point 'glued' to, or MARKER_BEFORE_EOT  */
  ch? : string;
  /** marker number, 1-based */
  marker? : number;
};

/** list of marker entries, from remove_markers */
export type MarkerMap = Array<MarkerEntry>;

/** return type from parse_next_marker */
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
    return matchArray(str, VariableParser.STRING_REFERENCE);
  }

  /**
   * parse a string into references
   * @param str input string
   * @returns `[]` or an array of all string references referenced
   */
  public static allSetReferences(str: string): string[] {
    return matchArray(str, VariableParser.SET_REFERENCE);
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
    if (ElementParser.MATCH_NESTED_SQUARE_BRACKETS.test(str)) {
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
