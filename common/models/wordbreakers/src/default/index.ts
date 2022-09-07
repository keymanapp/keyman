// Include the word-breaking data here:
/// <reference path="./data.ts" />
namespace wordBreakers {
  /**
   * Word breaker based on Unicode Standard Annex #29, Section 4.1:
   * Default Word Boundary Specification.
   *
   * @see http://unicode.org/reports/tr29/#Word_Boundaries
   * @see https://github.com/eddieantonio/unicode-default-word-boundary/tree/v12.0.0
   */
  export function default_(text: string): Span[] {
    let boundaries = findBoundaries(text);
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

      if (isNonSpace(span.text)) {
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

  // Utilities //
  import WordBreakProperty = wordBreakers.data.WordBreakProperty;
  import WORD_BREAK_PROPERTY = wordBreakers.data.WORD_BREAK_PROPERTY;
  import I = wordBreakers.data.I;

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
   * Provides a useful presentation for wordbreaker's context for use in word-breaking rules.
   *
   * @see https://unicode.org/reports/tr29/#Word_Boundary_Rules
   */
  export class BreakerContext {
    private  readonly text: string;

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
    readonly lookahead:  WordBreakProperty;

    /**
     * Initializes the word-breaking context at the start of the word-breaker's boundary-detection
     * algorithm.
     * @param text          The text to be word-broken
     * @param lookaheadPos  The position corresponding to `lookahead`.
     */
    constructor(text: string, lookaheadPos: number);
    /**
     * Used internally by the boundary-detection algorithm during context-shifting operations.
     * @param text
     * @param lookbehind
     * @param left
     * @param right
     * @param lookahead
     */
    constructor(text: string,
                lookbehind: WordBreakProperty,
                left:       WordBreakProperty,
                right:      WordBreakProperty,
                lookahead:  WordBreakProperty);
    constructor(text: string,
                prop1:  WordBreakProperty | number,
                prop2?: WordBreakProperty,
                prop3?: WordBreakProperty,
                prop4?: WordBreakProperty) {
      this.text = text;

      if(arguments.length == 2) {
        this.lookahead = this.wordbreakPropertyAt(prop1);// prop1;
      } else /*if(arguments.length == 5)*/ {
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
      return new BreakerContext(this.text, this.left, this.right, this.lookahead, newLookahead);
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
      return new BreakerContext(this.text, this.lookbehind, this.left, this.lookahead, newLookahead);
    }

    /**
     * Used for WB4:  when ignoring characters after an intervening linebreak, it's
     * `lookahead` that gets replaced without shifting the other tracked properties.
     * @param lookahead
     * @returns
     */
    public ignoringLookahead(lookaheadPos: number) {
      let newLookahead = this.wordbreakPropertyAt(lookaheadPos);
      return new BreakerContext(this.text, this.lookbehind, this.left, this.right, newLookahead);
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
      return property(this.text[pos]);
    }
  }

  /**
   * Returns true when the chunk does not solely consist of whitespace.
   *
   * @param chunk a chunk of text. Starts and ends at word boundaries.
   */
  function isNonSpace(chunk: string): boolean {
    return !Array.from(chunk).map(property).every(wb => (
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
  function findBoundaries(text: string): number[] {
    // WB1 and WB2: no boundaries if given an empty string.
    if (text.length === 0) {
      // There are no boundaries in an empty string!
      return [];
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
    let state = new BreakerContext(text, lookaheadPos);
    // Count RIs to make sure we're not splitting emoji flags:
    let nConsecutiveRegionalIndicators = 0;

    do  {
      // Shift all positions, one scalar value to the right.
      rightPos = lookaheadPos;
      lookaheadPos = positionAfter(lookaheadPos);
      // Shift all properties, one scalar value to the right.
      state = state.next(lookaheadPos);

      // Break at the start and end of text, unless the text is empty.
      // WB1: Break at start of text...
      if (state.left === WordBreakProperty.sot) {
        boundaries.push(rightPos);
        continue;
      }
      // WB2: Break at the end of text...
      if (state.right === WordBreakProperty.eot) {
        boundaries.push(rightPos);
        break; // Reached the end of the string. We're done!
      }
      // WB3: Do not break within CRLF:
      if (state.left === WordBreakProperty.CR && state.right === WordBreakProperty.LF)
        continue;
      // WB3b: Otherwise, break after...
      if (state.left === WordBreakProperty.Newline ||
          state.left === WordBreakProperty.CR ||
          state.left === WordBreakProperty.LF) {
        boundaries.push(rightPos);
        continue;
      }
      // WB3a: ...and before newlines
      if (state.right === WordBreakProperty.Newline ||
          state.right === WordBreakProperty.CR ||
          state.right === WordBreakProperty.LF) {
        boundaries.push(rightPos);
        continue;
      }

      // TODO: WB3c is not implemented, due to its complex, error-prone
      // implementation, requiring a ginormous regexp, and the fact that
      // the only thing it does is prevent big emoji sequences from being
      // split up, like 🧚🏼‍♂️
      // https://www.unicode.org/Public/emoji/12.0/emoji-zwj-sequences.txt

      // WB3d: Keep horizontal whitespace together
      if (state.left === WordBreakProperty.WSegSpace && state.right == WordBreakProperty.WSegSpace)
        continue;

      // WB4: Ignore format and extend characters
      // This is to keep grapheme clusters together!
      // See: Section 6.2: https://unicode.org/reports/tr29/#Grapheme_Cluster_and_Format_Rules
      // N.B.: The rule about "except after sot, CR, LF, and
      // Newline" already been by WB1, WB2, WB3a, and WB3b above.
      while (state.right === WordBreakProperty.Format ||
            state.right === WordBreakProperty.Extend ||
            state.right === WordBreakProperty.ZWJ) {
        // Continue advancing in the string, as if these
        // characters do not exist. DO NOT update left and
        // lookbehind however!
        [rightPos, lookaheadPos] = [lookaheadPos, positionAfter(lookaheadPos)];
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
      while (state.lookahead === WordBreakProperty.Format ||
            state.lookahead === WordBreakProperty.Extend ||
            state.lookahead === WordBreakProperty.ZWJ) {
        // Continue advancing in the string, as if these
        // characters do not exist. DO NOT update left and right,
        // however!
        lookaheadPos = positionAfter(lookaheadPos);
        state = state.ignoringLookahead(lookaheadPos);
      }

      // WB5: Do not break between most letters.
      if (isAHLetter(state.left) && isAHLetter(state.right))
        continue;
      // Do not break across certain punctuation
      // WB6: (Don't break before apostrophes in contractions)
      if (isAHLetter(state.left) && isAHLetter(state.lookahead) &&
        (state.right === WordBreakProperty.MidLetter || isMidNumLetQ(state.right)))
        continue;
      // WB7: (Don't break after apostrophes in contractions)
      if (isAHLetter(state.lookbehind) && isAHLetter(state.right) &&
        (state.left === WordBreakProperty.MidLetter || isMidNumLetQ(state.left)))
        continue;
      // WB7a
      if (state.left === WordBreakProperty.Hebrew_Letter && state.right === WordBreakProperty.Single_Quote)
        continue;
      // WB7b
      if (state.left === WordBreakProperty.Hebrew_Letter && state.right === WordBreakProperty.Double_Quote &&
          state.lookahead === WordBreakProperty.Hebrew_Letter)
        continue;
      // WB7c
      if (state.lookbehind === WordBreakProperty.Hebrew_Letter && state.left === WordBreakProperty.Double_Quote &&
          state.right === WordBreakProperty.Hebrew_Letter)
        continue;
      // Do not break within sequences of digits, or digits adjacent to letters.
      // e.g., "3a" or "A3"
      // WB8
      if (state.left === WordBreakProperty.Numeric && state.right === WordBreakProperty.Numeric)
        continue;
      // WB9
      if (isAHLetter(state.left) && state.right === WordBreakProperty.Numeric)
        continue;
      // WB10
      if (state.left === WordBreakProperty.Numeric && isAHLetter(state.right))
        continue;
      // Do not break within sequences, such as 3.2, 3,456.789
      // WB11
      if (state.lookbehind === WordBreakProperty.Numeric && state.right === WordBreakProperty.Numeric &&
        (state.left === WordBreakProperty.MidNum || isMidNumLetQ(state.left)))
        continue;
      // WB12
      if (state.left === WordBreakProperty.Numeric && state.lookahead === WordBreakProperty.Numeric &&
          (state.right === WordBreakProperty.MidNum || isMidNumLetQ(state.right)))
        continue;
      // WB13: Do not break between Katakana
      if (state.left === WordBreakProperty.Katakana && state.right === WordBreakProperty.Katakana)
        continue;
      // Do not break from extenders (e.g., U+202F NARROW NO-BREAK SPACE)
      // WB13a
      if ((isAHLetter(state.left) ||
          state.left === WordBreakProperty.Numeric ||
          state.left === WordBreakProperty.Katakana ||
          state.left === WordBreakProperty.ExtendNumLet) &&
          state.right === WordBreakProperty.ExtendNumLet)
        continue;
      // WB13b
      if ((isAHLetter(state.right) ||
        state.right === WordBreakProperty.Numeric ||
        state.right === WordBreakProperty.Katakana) && state.left === WordBreakProperty.ExtendNumLet)
        continue;

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

    /**
     * Returns the position of the start of the next scalar value. This jumps
     * over surrogate pairs.
     *
     * If asked for the character AFTER the end of the string, this always
     * returns the length of the string.
     */
    function positionAfter(pos: number): number {
      if (pos >= text.length) {
        return text.length;
      } else if (isStartOfSurrogatePair(text[pos])) {
        return pos + 2;
      }
      return pos + 1;
    }

    // Word_Break rule macros
    // See: https://unicode.org/reports/tr29/#WB_Rule_Macros
    function isAHLetter(prop: WordBreakProperty): boolean {
      return prop === WordBreakProperty.ALetter ||
            prop === WordBreakProperty.Hebrew_Letter;
    }

    function isMidNumLetQ(prop: WordBreakProperty): boolean {
      return prop === WordBreakProperty.MidNumLet ||
            prop === WordBreakProperty.Single_Quote;
    }
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
  export function property(character: string): WordBreakProperty {
    // This MUST be a scalar value.
    // TODO: remove dependence on character.codepointAt()?
    let codepoint = character.codePointAt(0) as number;
    return searchForProperty(codepoint, 0, WORD_BREAK_PROPERTY.length - 1);
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
}

// We cannot export a member whose name is a reserved word when
// implementing a namespace, BUT we can manually make the
// assignment and **declare** it as part of the namespace.
wordBreakers['default'] = wordBreakers.default_;
wordBreakers['unicodeProperty'] = wordBreakers.property;
declare namespace wordBreakers {
  export { default_ as default };
  export { property as unicodeProperty };
}
