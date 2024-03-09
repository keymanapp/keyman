import { WordBreakProperty, WORD_BREAK_PROPERTY, I, propertyMap } from "./data.js";

/**
 * A set of options used to customize and extend the behavior of the default
 * Unicode wordbreaker.
 */
export interface DefaultWordBreakerOptions {
  /**
   * Allows addition of custom wordbreaking rules, which will be applied
   * after WB1-WB4 and before all other default wordbreaking rules.
   *
   * @see `WordbreakerRule`
   */
  rules?: WordbreakerRule[];

  /**
   * Allows assignment of characters to different word-breaking properties than
   * their standard word-breaking assignment, including to custom properties
   * specified within `customProperties`.
   * @param char
   */
  propertyMapping?(char: string): string;

  /**
   * Allows definition of extra word-breaking properties for use with custom
   * rules.
   */
  customProperties?: string[];
}

/**
 * Word breaker based on Unicode Standard Annex #29, Section 4.1:
 * Default Word Boundary Specification.
 *
 * @see http://unicode.org/reports/tr29/#Word_Boundaries
 * @see https://github.com/eddieantonio/unicode-default-word-boundary/tree/v12.0.0
 */
export default function default_(text: string, options?: DefaultWordBreakerOptions): Span[] {
  let boundaries = findBoundaries(text, options);
  if (boundaries.length == 0) {
    return [];
  }

  // All non-empty strings have at least TWO boundaries: at the start and at the end of
  // the string.
  let spans = [];
  for (let i = 0; i < boundaries.length - 1; i++) {
    let start = boundaries[i];
    let end = boundaries[i + 1];
    let span = new LazySpan(text, start, end);

    if (isNonSpace(span.text, options)) {
      spans.push(span);
      // Preserve a sequence-final space if it exists.  Needed to signal "end of word".
    } else if (i == boundaries.length - 2) { // if "we just checked the final boundary"...
      // We don't want to return the whitespace itself; the correct token is simply ''.
      span = new LazySpan(text, end, end);
      spans.push(span);
    }
  }
  return spans;
}

/**
 * A span that does not cut out the substring until it absolutely has to!
 */
class LazySpan implements Span {
  private _source: string;
  readonly start: number;
  readonly end: number;
  constructor(source: string, start: number, end: number) {
    this._source = source;
    this.start = start;
    this.end = end;
  }

  get text(): string {
    return this._source.substring(this.start, this.end);
  }

  get length(): number {
    return this.end - this.start;
  }
}

/**
 * An abstraction supporting custom wordbreaker boundary rules.  While this doesn't provide
 * support for more complex rules like WB4, WB15, or WB16, this is sufficient for all other
 * default word-breaking rules and can be used to define custom rules of similar structure.
 *
 * @see https://unicode.org/reports/tr29/#WB_Rule_Macros
 */
export interface WordbreakerRule {
  /**
   * Indicates whether or not the rule applies in the specified context.
   * @param context
   */
  match(context: BreakerContext): boolean;

  /**
   * Indicates whether or not the rule indicates a word boundary at the context's site when it matches.
   */
  breakIfMatch: boolean;
}

/**
 * Provides a useful presentation for wordbreaker's context for use in word-breaking rules.
 *
 * @see https://unicode.org/reports/tr29/#Word_Boundary_Rules
 */
export class BreakerContext {
  // Referenced by this object in order to facilitate `lookahead` maintenance.
  private readonly text: string;
  readonly options?: DefaultWordBreakerOptions;

  /**
   * Represents the property of character immediately preceding `left`'s character.
   */
  readonly lookbehind: WordBreakProperty = WordBreakProperty.sot;

  /**
   * Represents the property of the character immediately preceding the potential word boundary.
   */
  readonly left:       WordBreakProperty = WordBreakProperty.sot;

  /**
   * Represents the property of the character immediately following the potential word boundary.
   */
  readonly right:      WordBreakProperty = WordBreakProperty.sot;

  /**
   * Represents the property of the character immediately following `right`'s character.
   */
  readonly lookahead:  WordBreakProperty; // Always initialized by constructor.

  /**
   * Initializes the word-breaking context at the start of the word-breaker's boundary-detection
   * algorithm.
   * @param text          The text to be word-broken
   * @param lookaheadPos  The position corresponding to `lookahead`.
   */
  constructor(text: string, options: DefaultWordBreakerOptions | undefined, lookaheadPos: number);
  /**
   * Used internally by the boundary-detection algorithm during context-shifting operations.
   * @param text
   * @param lookbehind
   * @param left
   * @param right
   * @param lookahead
   */
  constructor(text: string,
              options: DefaultWordBreakerOptions | undefined,
              lookbehind: WordBreakProperty,
              left:       WordBreakProperty,
              right:      WordBreakProperty,
              lookahead:  WordBreakProperty);
  constructor(text: string,
              options: DefaultWordBreakerOptions | undefined,
              prop1:  WordBreakProperty | number,
              prop2?: WordBreakProperty,
              prop3?: WordBreakProperty,
              prop4?: WordBreakProperty) {
    this.text = text;
    this.options = options;

    if(arguments.length == 3) {
      this.lookahead = this.wordbreakPropertyAt(prop1);// prop1;
    } else /*if(arguments.length == 6)*/ {
      this.lookbehind = prop1 as WordBreakProperty;
      this.left       = prop2 as WordBreakProperty;
      this.right      = prop3 as WordBreakProperty;
      this.lookahead  = prop4 as WordBreakProperty;
    }
  }

  /**
   * The general use-case when shifting boundary-check position if WB4 is not active.
   * @param lookahead The WordBreakProperty for the character to become `lookahead`.
   * @returns
   */
  public next(lookaheadPos: number): BreakerContext {
    let newLookahead = this.wordbreakPropertyAt(lookaheadPos);
    return new BreakerContext(this.text, this.options, this.left, this.right, this.lookahead, newLookahead);
  }

  /**
   * Used for WB4:  when ignoring characters before an intervening linebreak, we
   * replace `right` with the current `lookahead`, without affecting `lookbehind`
   * or `left`.  A new `lookahead` is then needed.
   * @param lookahead
   * @returns
   */
  public ignoringRight(lookaheadPos: number) {
    let newLookahead = this.wordbreakPropertyAt(lookaheadPos);
    return new BreakerContext(this.text, this.options, this.lookbehind, this.left, this.lookahead, newLookahead);
  }

  /**
   * Used for WB4:  when ignoring characters after an intervening linebreak, it's
   * `lookahead` that gets replaced without shifting the other tracked properties.
   * @param lookahead
   * @returns
   */
  public ignoringLookahead(lookaheadPos: number) {
    let newLookahead = this.wordbreakPropertyAt(lookaheadPos);
    return new BreakerContext(this.text, this.options, this.lookbehind, this.left, this.right, newLookahead);
  }

  /**
   * Return the value of the Word_Break property at the given string index.
   * @param pos position in the text.
   */
  private wordbreakPropertyAt(pos: number) {
    if (pos < 0) {
      return WordBreakProperty.sot; // Always "start of string" before the string starts!
    } else if (pos >= this.text.length) {
      return WordBreakProperty.eot; // Always "end of string" after the string ends!
    } else if (isStartOfSurrogatePair(this.text[pos])) {
      // Surrogate pairs the next TWO items from the string!
      return property(this.text[pos] + this.text[pos + 1]);
    }
    return property(this.text[pos], this.options);
  }

  /**
   * Returns `true` if and only if each member of the context has a property included within
   * its corresponding set (when specified).  Any set may be replaced with null to disable
   * a check against its corresponding property.
   * @param lookbehindSet
   * @param leftSet
   * @param rightSet
   * @param lookaheadSet
   */
  public match(lookbehindSet: WordBreakProperty[] | null,
                leftSet:       WordBreakProperty[] | null,
                rightSet:      WordBreakProperty[] | null,
                lookaheadSet:  WordBreakProperty[] | null) : boolean {
    let result: boolean = lookbehindSet?.includes(this.lookbehind) ?? true;
    result = result && (leftSet?.includes(this.left) ?? true);
    result = result && (rightSet?.includes(this.right) ?? true);
    return   result && (lookaheadSet?.includes(this.lookahead) ?? true);
  }

  /**
   * Returns `true` if and only if each member of the context has a property included within
   * its corresponding set (when specified).  Any set may be replaced with null to disable
   * a check against its corresponding property.
   *
   * Names should match those found at https://unicode.org/reports/tr29/#Word_Boundary_Rules
   * or defined in the word-breaker customization options; matching is case-insensitive.
   * Also includes two extra properties:
   * - `sot` - start of text
   * - `eot` - end of text
   * @param lookbehindSet
   * @param leftSet
   * @param rightSet
   * @param lookaheadSet
   */
  public propertyMatch(lookbehindSet: string[] | null,
                        leftSet:       string[] | null,
                        rightSet:      string[] | null,
                        lookaheadSet:  string[] | null) : boolean {
      const propMapper = (name: string) => propertyVal(name, this.options);
      return this.match(lookbehindSet?.map(propMapper) as WordBreakProperty[] | null,
                        leftSet?.map(propMapper)       as WordBreakProperty[] | null,
                        rightSet?.map(propMapper)      as WordBreakProperty[] | null,
                        lookaheadSet?.map(propMapper)  as WordBreakProperty[] | null);
    }
  }

/**
 * Returns true when the chunk does not solely consist of whitespace.
 *
 * @param chunk a chunk of text. Starts and ends at word boundaries.
 */
function isNonSpace(chunk: string, options?: DefaultWordBreakerOptions): boolean {
  return !chunk.split('').map((char) => property(char, options)).every(wb => (
    wb === WordBreakProperty.CR ||
    wb === WordBreakProperty.LF ||
    wb === WordBreakProperty.Newline ||
    wb === WordBreakProperty.WSegSpace
  ));
}

/**
 * Yields a series of string indices where a word break should
 * occur. That is, there should be a break BEFORE each string
 * index yielded by this generator.
 *
 * @param text Text to find word boundaries in.
 */
function findBoundaries(text: string, options?: DefaultWordBreakerOptions): number[] {
  // WB1 and WB2: no boundaries if given an empty string.
  if (text.length === 0) {
    // There are no boundaries in an empty string!
    return [];
  }

  if(options && !options.rules) {
    options.rules = [];
  }

  // This algorithm works by maintaining a sliding window of four SCALAR VALUES.
  //
  //  - Scalar values? JavaScript strings are NOT actually a string of
  //    Unicode code points; some characters are made up of TWO
  //    JavaScript indices. e.g.,
  //        "💩".length === 2;
  //        "💩"[0] === '\uD83D';
  //        "💩"[1] === '\uDCA9';
  //
  //    These characters that are represented by TWO indices are
  //    called "surrogate pairs". Since we don't want to be in the
  //    "middle" of a character, make sure we're always advancing
  //    by scalar values, and NOT indices. That means, we sometimes
  //    need to advance by TWO indices, not just one.
  //  - Four values? Some rules look at what's to the left of
  //    left, and some look at what's to the right of right. So
  //    keep track of this!

  let boundaries = [];

  let rightPos: number;
  let lookaheadPos = 0; // lookahead, one scalar value to the right of right.
  // Before the start of the string is also the start of the string.
  let state = new BreakerContext(text, options, lookaheadPos);
  // Count RIs to make sure we're not splitting emoji flags:
  let nConsecutiveRegionalIndicators = 0;

  do  {
    // Shift all positions, one scalar value to the right.
    rightPos = lookaheadPos;
    lookaheadPos = positionAfter(text, lookaheadPos);
    // Shift all properties, one scalar value to the right.
    state = state.next(lookaheadPos);

    // Break at the start and end of text, unless the text is empty.
    // WB1: Break at start of text...
    if (state.match(null, [WordBreakProperty.sot], null, null)) {
      boundaries.push(rightPos);
      continue;
    }
    // WB2: Break at the end of text...
    if (state.match(null, null, [WordBreakProperty.eot], null)) {
      boundaries.push(rightPos);
      break; // Reached the end of the string. We're done!
    }
    // WB3: Do not break within CRLF:
    if (state.match(null, [WordBreakProperty.CR], [WordBreakProperty.LF], null)) {
      continue;
    }

    // WB3b: Otherwise, break after...
    const NEWLINE_SET = [WordBreakProperty.Newline, WordBreakProperty.CR, WordBreakProperty.LF];
    if(state.match(null, NEWLINE_SET, null, null)) {
      boundaries.push(rightPos);
      continue;
    }
    // WB3a: ...and before newlines
    if (state.match(null, null, NEWLINE_SET, null)) {
      boundaries.push(rightPos);
      continue;
    }

    // TODO: WB3c is not implemented, due to its complex, error-prone
    // implementation, requiring a ginormous regexp, and the fact that
    // the only thing it does is prevent big emoji sequences from being
    // split up, like 🧚🏼‍♂️
    // https://www.unicode.org/Public/emoji/12.0/emoji-zwj-sequences.txt

    // WB3d: Keep horizontal whitespace together
    if (state.match(null, [WordBreakProperty.WSegSpace], [WordBreakProperty.WSegSpace], null)) {
      continue;
    }

    // WB4: Ignore format and extend characters
    // This is to keep grapheme clusters together!
    // See: Section 6.2: https://unicode.org/reports/tr29/#Grapheme_Cluster_and_Format_Rules
    // N.B.: The rule about "except after sot, CR, LF, and
    // Newline" already been by WB1, WB2, WB3a, and WB3b above.
    const SET_WB4_IGNORE = [WordBreakProperty.Format, WordBreakProperty.Extend, WordBreakProperty.ZWJ];
    while (state.match(null, null, SET_WB4_IGNORE, null)) {
      // Continue advancing in the string, as if these
      // characters do not exist. DO NOT update left and
      // lookbehind however!
      [rightPos, lookaheadPos] = [lookaheadPos, positionAfter(text, lookaheadPos)];
      state = state.ignoringRight(lookaheadPos);
    }
    // In ignoring the characters in the previous loop, we could
    // have fallen off the end of the string, so end the loop
    // prematurely if that happens!
    if (state.right === WordBreakProperty.eot) {
      boundaries.push(rightPos);
      break;
    }
    // WB4 (continued): Lookahead must ALSO ignore these format,
    // extend, ZWJ characters!
    while (state.match(null, null, null, SET_WB4_IGNORE)) {
      // Continue advancing in the string, as if these
      // characters do not exist. DO NOT update left and right,
      // however!
      lookaheadPos = positionAfter(text, lookaheadPos);
      state = state.ignoringLookahead(lookaheadPos);
    }

    // See: https://unicode.org/reports/tr29/#WB_Rule_Macros
    const SET_AHLETTER   = [WordBreakProperty.ALetter,   WordBreakProperty.Hebrew_Letter];
    const SET_MIDNUMLETQ = [WordBreakProperty.MidNumLet, WordBreakProperty.Single_Quote];

    // Custom rules may override the base ruleset aside from the first few fundamental ones.
    if(options?.rules) {
      let customMatch: boolean = false;
      for(const rule of options.rules) {
        customMatch = rule.match(state);
        if(customMatch) {
          if(rule.breakIfMatch) {
            boundaries.push(rightPos);
          }
          break; // as customMatch == true here, this will trigger the `continue` that follows.
        }
      }
      if(customMatch) {
        continue;
      }
    }

    // WB5: Do not break between most letters.
    // if (isAHLetter(state.left) && isAHLetter(state.right))
    if(state.match(null, SET_AHLETTER, SET_AHLETTER, null)) {
      continue;
    }
    // Do not break across certain punctuation
    // WB6: (Don't break before apostrophes in contractions)
    const SET_ALL_MIDLETTER = [WordBreakProperty.MidLetter].concat(SET_MIDNUMLETQ);
    if(state.match(null, SET_AHLETTER, SET_ALL_MIDLETTER, SET_AHLETTER)) {
      continue;
    }
    // WB7: (Don't break after apostrophes in contractions)
    if(state.match(SET_AHLETTER, SET_ALL_MIDLETTER, SET_AHLETTER, null)) {
      continue;
    }

    // WB7a
    if(state.match(null, [WordBreakProperty.Hebrew_Letter], [WordBreakProperty.Single_Quote], null)) {
      continue;
    }
    // WB7b
    if(state.match(null,
                    [WordBreakProperty.Hebrew_Letter],
                    [WordBreakProperty.Double_Quote],
                    [WordBreakProperty.Hebrew_Letter])) {
      continue;
    }
    // WB7c
    if(state.match([WordBreakProperty.Hebrew_Letter],
                    [WordBreakProperty.Double_Quote],
                    [WordBreakProperty.Hebrew_Letter],
                    null)) {
      continue;
    }
    // Do not break within sequences of digits, or digits adjacent to letters.
    // e.g., "3a" or "A3"
    // WB8
    if(state.match(null, [WordBreakProperty.Numeric], [WordBreakProperty.Numeric], null)) {
      continue;
    }
    // WB9
    if(state.match(null, SET_AHLETTER, [WordBreakProperty.Numeric], null)) {
      continue;
    }
    // WB10
    if(state.match(null, [WordBreakProperty.Numeric], SET_AHLETTER, null)) {
      continue;
    }
    // Do not break within sequences, such as 3.2, 3,456.789
    // WB11
    const SET_ALL_MIDNUM = [WordBreakProperty.MidNum].concat(SET_MIDNUMLETQ);
    if(state.match([WordBreakProperty.Numeric], SET_ALL_MIDNUM, [WordBreakProperty.Numeric], null)) {
      continue;
    }
    // WB12
    if(state.match(null, [WordBreakProperty.Numeric], SET_ALL_MIDNUM, [WordBreakProperty.Numeric])) {
      continue;
    }
    // WB13: Do not break between Katakana
    if(state.match(null, [WordBreakProperty.Katakana], [WordBreakProperty.Katakana], null)) {
      continue;
    }
    // Do not break from extenders (e.g., U+202F NARROW NO-BREAK SPACE)
    // WB13a
    const SET_NUM_KAT_LET = [WordBreakProperty.Katakana, WordBreakProperty.Numeric].concat(SET_AHLETTER);
    if(state.match(null, SET_NUM_KAT_LET, [WordBreakProperty.ExtendNumLet], null)) {
      continue;
    }
    if(state.match(null, [WordBreakProperty.ExtendNumLet], [WordBreakProperty.ExtendNumLet], null)) {
      continue;
    }
    // WB13b
    if(state.match(null, [WordBreakProperty.ExtendNumLet], SET_NUM_KAT_LET, null)) {
      continue;
    }

    // WB15 & WB16:
    // Do not break within emoji flag sequences. That is, do not break between
    // regional indicator (RI) symbols if there is an odd number of RI
    // characters before the break point.
    if (state.right === WordBreakProperty.Regional_Indicator) {
      // Emoji flags are actually composed of TWO scalar values, each being a
      // "regional indicator". These indicators correspond to Latin letters. Put
      // two of them together, and they spell out an ISO 3166-1-alpha-2 country
      // code. Since these always come in pairs, NEVER split the pairs! So, if
      // we happen to be inside the middle of an odd numbered of
      // Regional_Indicators, DON'T SPLIT IT!
      nConsecutiveRegionalIndicators += 1;
      if ((nConsecutiveRegionalIndicators % 2) == 1) {
        continue;
      }
    } else {
      nConsecutiveRegionalIndicators = 0;
    }
    // WB999: Otherwise, break EVERYWHERE (including around ideographs)
    boundaries.push(rightPos);
  } while (rightPos < text.length);

  return boundaries;

  ///// Internal utility functions /////
}

/**
 * Returns the position of the start of the next scalar value. This jumps
 * over surrogate pairs.
 *
 * If asked for the character AFTER the end of the string, this always
 * returns the length of the string.
 */
export function positionAfter(text: string, pos: number): number {
  if (pos >= text.length) {
    return text.length;
  } else if (isStartOfSurrogatePair(text[pos])) {
    return pos + 2;
  }
  return pos + 1;
}

function isStartOfSurrogatePair(character: string) {
  let codeUnit = character.charCodeAt(0);
  return codeUnit >= 0xD800 && codeUnit <= 0xDBFF;
}

/**
 * Return the Word_Break property value for a character.
 * Note that
 * @param character a scalar value
 */
function property(character: string, options?: DefaultWordBreakerOptions): WordBreakProperty {
  // If there is a customized mapping for the character, prioritize that.
  if(options?.propertyMapping) {
    let propName = options.propertyMapping(character);
    if(propName) {
      return propertyVal(propName, options);
    }
  }

  // This MUST be a scalar value.
  // TODO: remove dependence on character.codepointAt()?
  let codepoint = character.codePointAt(0) as number;

  return searchForProperty(codepoint, 0, WORD_BREAK_PROPERTY.length - 1);
}

function propertyVal(propName: string, options?: DefaultWordBreakerOptions) {
  const matcher = (name: string) => name.toLowerCase() == propName.toLowerCase()

  const customIndex = options?.customProperties?.findIndex(matcher) ?? -1;
  return customIndex != -1 ? -customIndex - 1 : propertyMap.findIndex(matcher);
}

/**
 * Binary search for the word break property of a given CODE POINT.
 *
 * The auto-generated data.ts master array defines a **character range**
 * lookup table.  If a character's codepoint is equal to or greater than
 * the I.Start value for an entry and exclusively less than the next entry,
 * it falls in the first entry's range bucket and is classified accordingly
 * by this method.
 */
function searchForProperty(codePoint: number, left: number, right: number): WordBreakProperty {
  // All items that are not found in the array are assigned the 'Other' property.
  if (right < left) {
    return WordBreakProperty.Other;
  }

  let midpoint = left + ~~((right - left) / 2);
  let candidate = WORD_BREAK_PROPERTY[midpoint];

  let nextRange = WORD_BREAK_PROPERTY[midpoint + 1];
  let startOfNextRange = nextRange ? nextRange[I.Start] : Infinity;

  if (codePoint < candidate[I.Start]) {
    return searchForProperty(codePoint, left, midpoint - 1);
  } else if (codePoint >= startOfNextRange) {
    return searchForProperty(codePoint, midpoint + 1, right);
  }

  // We found it!
  return candidate[I.Value];
}
