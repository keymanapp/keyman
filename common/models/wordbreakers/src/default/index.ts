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

    // All non-empty strings have at least TWO boundaries at the start and end of
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
    let lookbehind: WordBreakProperty;
    let left = WordBreakProperty.sot;
    let right = WordBreakProperty.sot;
    let lookahead = wordbreakPropertyAt(0);
    // Count RIs to make sure we're not splitting emoji flags:
    let nConsecutiveRegionalIndicators = 0;

    do  {
      // Shift all positions, one scalar value to the right.
      rightPos = lookaheadPos;
      lookaheadPos = positionAfter(lookaheadPos);
      // Shift all properties, one scalar value to the right.
      [lookbehind, left, right, lookahead] =
        [left, right, lookahead, wordbreakPropertyAt(lookaheadPos)];

      // Break at the start and end of text, unless the text is empty.
      // WB1: Break at start of text...
      if (left === WordBreakProperty.sot) {
        boundaries.push(rightPos);
        continue;
      }
      // WB2: Break at the end of text...
      if (right === WordBreakProperty.eot) {
        boundaries.push(rightPos);
        break; // Reached the end of the string. We're done!
      }
      // WB3: Do not break within CRLF:
      if (left === WordBreakProperty.CR && right === WordBreakProperty.LF)
        continue;
      // WB3b: Otherwise, break after...
      if (left === WordBreakProperty.Newline ||
          left === WordBreakProperty.CR ||
          left === WordBreakProperty.LF) {
        boundaries.push(rightPos);
        continue;
      }
      // WB3a: ...and before newlines
      if (right === WordBreakProperty.Newline ||
          right === WordBreakProperty.CR ||
          right === WordBreakProperty.LF) {
        boundaries.push(rightPos);
        continue;
      }

      // TODO: WB3c is not implemented, due to its complex, error-prone
      // implementation, requiring a ginormous regexp, and the fact that
      // the only thing it does is prevent big emoji sequences from being
      // split up, like 🧚🏼‍♂️
      // https://www.unicode.org/Public/emoji/12.0/emoji-zwj-sequences.txt

      // WB3d: Keep horizontal whitespace together
      if (left === WordBreakProperty.WSegSpace && right == WordBreakProperty.WSegSpace)
        continue;

      // WB4: Ignore format and extend characters
      // This is to keep grapheme clusters together!
      // See: Section 6.2: https://unicode.org/reports/tr29/#Grapheme_Cluster_and_Format_Rules
      // N.B.: The rule about "except after sot, CR, LF, and
      // Newline" already been by WB1, WB2, WB3a, and WB3b above.
      while (right === WordBreakProperty.Format ||
            right === WordBreakProperty.Extend ||
            right === WordBreakProperty.ZWJ) {
        // Continue advancing in the string, as if these
        // characters do not exist. DO NOT update left and
        // lookbehind however!
        [rightPos, lookaheadPos] = [lookaheadPos, positionAfter(lookaheadPos)];
        [right, lookahead] = [lookahead, wordbreakPropertyAt(lookaheadPos)];
      }
      // In ignoring the characters in the previous loop, we could
      // have fallen off the end of the string, so end the loop
      // prematurely if that happens!
      if (right === WordBreakProperty.eot) {
        boundaries.push(rightPos);
        break;
      }
      // WB4 (continued): Lookahead must ALSO ignore these format,
      // extend, ZWJ characters!
      while (lookahead === WordBreakProperty.Format ||
            lookahead === WordBreakProperty.Extend ||
            lookahead === WordBreakProperty.ZWJ) {
        // Continue advancing in the string, as if these
        // characters do not exist. DO NOT update left and right,
        // however!
        lookaheadPos = positionAfter(lookaheadPos);
        lookahead = wordbreakPropertyAt(lookaheadPos);
      }

      // WB5: Do not break between most letters.
      if (isAHLetter(left) && isAHLetter(right))
        continue;
      // Do not break across certain punctuation
      // WB6: (Don't break before apostrophes in contractions)
      if (isAHLetter(left) && isAHLetter(lookahead) &&
        (right === WordBreakProperty.MidLetter || isMidNumLetQ(right)))
        continue;
      // WB7: (Don't break after apostrophes in contractions)
      if (isAHLetter(lookbehind) && isAHLetter(right) &&
        (left === WordBreakProperty.MidLetter || isMidNumLetQ(left)))
        continue;
      // WB7a
      if (left === WordBreakProperty.Hebrew_Letter && right === WordBreakProperty.Single_Quote)
        continue;
      // WB7b
      if (left === WordBreakProperty.Hebrew_Letter && right === WordBreakProperty.Double_Quote &&
          lookahead === WordBreakProperty.Hebrew_Letter)
        continue;
      // WB7c
      if (lookbehind === WordBreakProperty.Hebrew_Letter && left === WordBreakProperty.Double_Quote &&
          right === WordBreakProperty.Hebrew_Letter)
        continue;
      // Do not break within sequences of digits, or digits adjacent to letters.
      // e.g., "3a" or "A3"
      // WB8
      if (left === WordBreakProperty.Numeric && right === WordBreakProperty.Numeric)
        continue;
      // WB9
      if (isAHLetter(left) && right === WordBreakProperty.Numeric)
        continue;
      // WB10
      if (left === WordBreakProperty.Numeric && isAHLetter(right))
        continue;
      // Do not break within sequences, such as 3.2, 3,456.789
      // WB11
      if (lookbehind === WordBreakProperty.Numeric && right === WordBreakProperty.Numeric &&
        (left === WordBreakProperty.MidNum || isMidNumLetQ(left)))
        continue;
      // WB12
      if (left === WordBreakProperty.Numeric && lookahead === WordBreakProperty.Numeric &&
          (right === WordBreakProperty.MidNum || isMidNumLetQ(right)))
        continue;
      // WB13: Do not break between Katakana
      if (left === WordBreakProperty.Katakana && right === WordBreakProperty.Katakana)
        continue;
      // Do not break from extenders (e.g., U+202F NARROW NO-BREAK SPACE)
      // WB13a
      if ((isAHLetter(left) ||
          left === WordBreakProperty.Numeric ||
          left === WordBreakProperty.Katakana ||
          left === WordBreakProperty.ExtendNumLet) &&
          right === WordBreakProperty.ExtendNumLet)
        continue;
      // WB13b
      if ((isAHLetter(right) ||
        right === WordBreakProperty.Numeric ||
        right === WordBreakProperty.Katakana) && left === WordBreakProperty.ExtendNumLet)
        continue;

      // WB15 & WB16:
      // Do not break within emoji flag sequences. That is, do not break between
      // regional indicator (RI) symbols if there is an odd number of RI
      // characters before the break point.
      if (right === WordBreakProperty.Regional_Indicator) {
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

    /**
     * Return the value of the Word_Break property at the given string index.
     * @param pos position in the text.
     */
    function wordbreakPropertyAt(pos: number) {
      if (pos < 0) {
        return WordBreakProperty.sot; // Always "start of string" before the string starts!
      } else if (pos >= text.length) {
        return WordBreakProperty.eot; // Always "end of string" after the string ends!
      } else if (isStartOfSurrogatePair(text[pos])) {
        // Surrogate pairs the next TWO items from the string!
        return property(text[pos] + text[pos + 1]);
      }
      return property(text[pos]);
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
  function property(character: string): WordBreakProperty {
    // This MUST be a scalar value.
    // TODO: remove dependence on character.codepointAt()?
    let codepoint = character.codePointAt(0) as number;
    return searchForProperty(codepoint, 0, WORD_BREAK_PROPERTY.length - 1);
  }

  /**
   * Binary search for the word break property of a given CODE POINT.
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
declare namespace wordBreakers {
  export { default_ as default };
}
