/**
 * Types and interfaces that must be know both by the lexical model compiler,
 * and by the runtime.
 */

/**
 * A simple word breaking function takes a phrase, and splits it into "words",
 * for whatever definition of "word" is usable for the language model.
 *
 * For example:
 *
 *   getText(breakWordsEnglish("Hello, world!")) == ["Hello", "world"]
 *   getText(breakWordsCree("ᑕᐻ ᒥᔪ ᑮᓯᑲᐤ ᐊᓄᐦᐨ᙮")) == ["ᑕᐻ", "ᒥᔪ ᑮᓯᑲᐤ""", "ᐊᓄᐦᐨ"]
 *   getText(breakWordsJapanese("英語を話せますか？")) == ["英語", "を", "話せます", "か"]
 *
 * Not all language models take in a configurable word breaking function.
 *
 * @returns an array of spans from the phrase, in order as they appear in the
 *          phrase, each span which representing a word.
 */
interface WordBreakingFunction {
  // invariant: span[i].end <= span[i + 1].start
  // invariant: for all span[i] and span[i + 1], there does not exist a span[k]
  //            where span[i].end <= span[k].start AND span[k].end <= span[i + 1].start
  (phrase: string): Span[];
}

/**
 * A span of text in a phrase. This is usually meant to represent words from a
 * pharse.
 */
interface Span {
  // invariant: start < end (empty spans not allowed)
  readonly start: number;
  // invariant: end > end (empty spans not allowed)
  readonly end: number;
  // invariant: length === end - start
  readonly length: number;
  // invariant: text.length === length
  // invariant: each character is BMP UTF-16 code unit, or is a high surrogate
  // UTF-16 code unit followed by a low surrogate UTF-16 code unit.
  readonly text: string;
}
/**
 * Options for various punctuation to use in suggestions.
 */
interface LexicalModelPunctuation {
  /**
   * The quotes that appear in "keep" suggestions, e.g., keep what the user
   * typed verbatim.
   *
   * The keep suggestion is often the leftmost one, when suggested.
   *
   * [ “Hrllo” ] [ Hello ] [ Heck ]
   */
  readonly quotesForKeepSuggestion: {
    /**
     * What will appear on the opening side of the quote.
     * (left side for LTR scripts; right side for RTL scripts)
     *
     * Default: `“`
     */
    readonly open: string;
    /**
     * What will appear on the closing side of the quote.
     * (right side for LTR scripts; left side for RTL scripts)
     *
     * Default: `”`
     */
    readonly close: string;
  };
  /**
   * What punctuation or spacing to insert after every complete word
   * prediction. This can be set to the empty string when the script does not
   * use spaces to separate words.
   *
   * Default: ` `
   */
  readonly insertAfterWord: string;
}
