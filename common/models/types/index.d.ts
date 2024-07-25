/**
 * TypeScript interfaces and types required in both within the LMLayer, and for
 * tools that create lexical models.
 */

/****************************** Lexical Models ******************************/

/**
 * A JavaScript string with the restriction that it must only
 * contain Unicode scalar values.
 *
 * This means that any lone high surrogate must be paired with
 * a low surrogate, if it exists. Lone surrogate code units are
 * forbidden.
 *
 * See also: https://developer.mozilla.org/en-US/docs/Web/API/USVString
 */
declare type USVString = string;

declare type CasingForm = 'lower' | 'initial' | 'upper';

/**
 * Represents one lexical entry and its probability..
 */
type TextWithProbability = {
  /**
   * A lexical entry (word) offered by the model.
   *
   * Note:  not the search-term keyed part.  This will match the actual, unkeyed form.
   */
  text: string;

  /**
   * The probability of the lexical entry, directly based upon its frequency.
   *
   * A real-number weight, from 0 to 1.
   */
  p: number;
}

/**
 * Used to facilitate edit-distance calculations by allowing the LMLayer to
 * efficiently search the model's lexicon in a Trie-like manner.
 */
declare interface LexiconTraversal {
  /**
   * Provides an iterable pattern used to search for words with a 'keyed' prefix matching
   * the current traversal state's prefix when a new character is appended.  Iterating
   * across `children` provides 'breadth' to a lexical search.
   *
   * For an example with English, if the current traversal state corresponds to 'th',
   * children() may return an iterator with states corresponding 'e'
   * (for 'the', 'then', 'there'), 'a' (for 'than', 'that'), etc.
   *
   * @param char      A full character (a UTF-16 code point, which may be comprised of two
   *                  code units) corresponding to the child node, appended to the current
   *                  node's prefix to produce the child node's prefix.
   * <p>
   *                  Example: if the current traversal's represented prefix is 'th',
   *                  char = 'e' would indicate a child node with prefix = 'the'.
   * @param traversal a LexiconTraversal starting from (or "rooted at") the child node.  Use
   *                  of the returned object provides 'depth' to a lexical search.
   * <p>
   *                  Example:
   *
   * - Suppose our current LexiconTraversal represents a prefix of 'th'.
   * - If `char` = 'e', the child represents a prefix of 'the'.
   * - Then `traversal` allows traversing the part of the lexicon prefixed by 'the'.
   */
  children(): {char: USVString, p: number, traversal: () => LexiconTraversal}[];

  /**
   * Allows direct access to the traversal state that results when appending one
   * or more codepoints encoded in UTF-16 to the current traversal state's prefix.
   * This allows bypassing iteration among all legal child Traversals.
   *
   * If such a traversal state is not supported, returns `undefined`.
   *
   * Note: traversals navigate and represent the lexicon in its "keyed" state,
   * as produced by use of the search-term keying function defined for the model.
   * That is, if a model "keys" `è` to `e`, there will be no `è` child.
   * @param char
   */
  child(char: USVString): LexiconTraversal | undefined;

  /**
   * Any entries directly keyed by the currently-represented lookup prefix.  Entries and
   * children may exist simultaneously, but `entries` must always exist when no children are
   * available in the returned `children()` iterable.
   *
   * Examples for English:
   * - search prefix of 'th':  [] - an empty array.  'th' is not a valid English word.
   * - search prefix of 'the': ['the'].  'then' and 'there' may also exist within the lexicon,
   *   but they most directly belong to deeper traversal states.
   *
   * May contain multiple children if a lexical model performs 'keying' operations, such as
   * may result from stripping accent markers from Spanish or French.  In this case, all entries
   * transformed to the same 'key' should be listed by their key's traversal node.
   *
   * Example using French "accent homographs", where keying operations strip accents:
   *
   * - prefix of 'acre': ['acre', 'âcre']
   * - prefix of 'crepe': ['crêpe', 'crêpé']
   * - other examples:  https://www.thoughtco.com/french-accent-homographs-1371072
   */
  entries: TextWithProbability[];

  // Note:  `p`, not `maxP` - we want to see the same name for `this.entries.p` and `this.p`
  /**
   * Gives the probability of the highest-frequency lexical entry that is either a member or
   * descendent of the represented trie `Node`.
   */
  p: number;
}

/**
 * The model implementation, within the Worker.
 */
declare interface LexicalModel {
  /**
   * Processes `config` messages, configuring the newly-loaded model based on the host
   * platform's capability restrictions.
   *
   * This allows the model to configure its suggestions according to what the platform
   * allows the host to actually perform - for example, if post-caret deletions are not
   * supported, no suggestions requiring this feature should be produced by the model.
   *
   * Returns a `Configuration` object detailing the capabilities the model plans to
   * actually utilize, which must be as restrictive or more restrictive than those
   * indicated within the provided `Capabilities` object.
   * @param capabilities
   */
  configure(capabilities: Capabilities): Configuration;

  /**
   * Indicates that the language represented by the lexical model has syntactic casing
   * behaviors.  Setting this to true will allow the predictive text engine to
   * perform casing-based corrections and predictions.
   *
   * If set to `false`, the default behavior for the `toKey` method will not perform
   * casing modifications and will thus yield case-sensitive results.  This will not occur
   * if `undefined` for backward-compatibility reasons.
   */
  readonly languageUsesCasing?: boolean;

  /**
   * Represents casing-related syntactical behaviors of the language represented by
   * this lexical model, modifying input text to follow the specified casing pattern.
   *
   * Implementations may assume that the text represents a single word / 'token' from the
   * context.
   *
   * Patterns:
   * - lower: all case-sensitive characters should be lowercased.  Example:  "text123"
   * - upper: all case-sensitive characters should be uppercased.  Example:  "TEXT123"
   * - initial:  only the word-initial character should be uppercased.
   * @param form
   * @param text
   */
  applyCasing?(form: CasingForm, text: string): string

  /**
   * Indicates a mapping function used by the model to simplify lookup operations
   * within the lexicon.  This is expected to result in a many-to-one mapping, transforming
   * the input text into a common, simplified 'index'/'key' form shared by all
   * text forms that a person might reasonably interpret as "the same".
   *
   * Example usages:
   * - converting any upper-case characters into lowercase.
   *   - For English, 'CAT' and 'Cat' might be keyed as 'cat', since users expect all
   *     three to be treated as the same word.
   * - removing accent marks that may be difficult to type on standard keyboard layouts
   *   - For French, users may wish to type "jeune" instead of "jeûne" when lazy or
   *     if accent marks cannot be easily input.
   *
   * Providing a function targetted for your language can greatly improve a user's experience
   * using your dictionary.
   * @param text The original input text.
   * @returns The 'keyed' form of that text.
   */
  toKey?(text: USVString): USVString;

  /**
   * Generates predictive suggestions corresponding to the state of context after the proposed
   * transform is applied to it.
   *
   * This method should NOT attempt to perform any form of correction; this is modeled within a
   * separate component of the LMLayer predictive engine.  That is, "th" + "e" should not be
   * have "this" for a suggestion ("e" has been 'corrected' to "i"), while "there" would be
   * a reasonable prediction.
   *
   * However, addition of diacritics to characters (which may transform the underlying char code
   * when Unicode-normalized) is permitted.  For example, "pur" + "e" may reasonably predict
   * "purée", where "e" has been transformed to "é" as part of the suggestion.  When possible,
   * it is recommended to accomplish this by defining a `toKey` (`searchTermToKey` in model
   * source) instead.
   *
   * When both prediction and correction are permitted, said component (the `ModelCompositor`) will
   * generally call this method once per 'likely' generated corrected state of the context,
   * utilizing the results to compute an overall likelihood across all possible suggestions.
   * @param transform A Transform corresponding to a recent input keystroke
   * @param context A depiction of the context to which `transform` is applied.
   * @returns A probability distribution (`Distribution<Suggestion>`) on the resulting `Suggestion`
   * space for use in determining the most optimal overall suggestions.
   */
  predict(transform: Transform, context: Context): Distribution<Suggestion>;

  /**
   * Punctuation and presentational settings that the underlying lexical model
   * expects to be applied at higher levels. e.g., the ModelCompositor.
   *
   * @see LexicalModelPunctuation
   */
  readonly punctuation?: LexicalModelPunctuation;

  /**
   * Returns the wordbreaker function defined for the model (if it exists).  This
   * wordbreaker should operate on a plain JS `string`, fully tokenizing it into
   * separate words according to the syntactical rules of the modeled language.
   *
   * Needed to support many of the enhancements in 14.0, as enhanced wordbreaking /
   * tokenization is necessary for properly tracking possible "fat finger" inputs
   * and intermediate calculations (increasing prediction quality) and preventing
   * their misuse when starting new words.
   */
  wordbreaker?: WordBreakingFunction;

  /**
   * Performs a wordbreak operation given the current context state, returning whatever word
   * or word fragment exists that starts before the caret but after the most recent whitespace
   * preceding the caret.  If no such text exists, the empty string is returned.
   *
   * This function is designed for use in generating display text for 'keep' `Suggestions`
   * and display text for reverting any previously-applied `Suggestions`.
   *
   * ------------------
   *
   * **NOTE:  _Deprecated_** and replaced by `wordbreaker` in 14.0.  You may still wish
   * to implement this function by reusing your `wordbreaker` definition if the model
   * may see use on Keyman 12.0 or 13.0, generally by returning `wordbreaker(context.left)`.
   *
   * As this function only tokenizes a single word from the context, it is insufficient for
   * supporting many of the predictive-text enhancements introduced in Keyman 14.  Its
   * intermediate calculations are tracked on a per-word basis and the increased detail
   * provided by `wordbreaker` helps with stability, validating the engine's use of
   * the current context.
   *
   * @param context
   * @deprecated
   */
  wordbreak?(context: Context): USVString;

  /**
   * Lexical models _may_ provide a LexiconTraversal object usable to enhance
   * prediction and correction results.  The returned object represents the
   * unfiltered lexicon (with an empty prefix).
   */
  traverseFromRoot?(): LexiconTraversal;
}

/**
 * Describes how to change a buffer at the cursor position.
 * first, you delete the specified amount amount from the left
 * and right, then you insert the provided text.
 */
declare interface Transform {
  /**
   * Facilitates use of unique identifiers for tracking the Transform and
   * any related data from its original source, as the reference cannot be
   * preserved across WebWorker boundaries.
   *
   * This is *separate* from any LMLayer-internal identification values.
   */
  id?: number;

  /**
   * The Unicode scalar values (i.e., characters) to be inserted at the
   * cursor position.
   *
   * Corresponds to `s` in com.keyman.KeyboardInterface.output.
   */
  insert: USVString;

  /**
   * The number of code units to delete to the left of the cursor.
   *
   * Corresponds to `dn` in com.keyman.KeyboardInterface.output.
   */
  deleteLeft: number;

  /**
   * The number of code units to delete to the right of the cursor.
   * Not available on all platforms.
   */
  deleteRight?: number;
}

/**
 * A concrete suggestion
 */
declare interface Suggestion {
  /**
   * Indicates the externally-supplied id of the Transform that prompted
   * the Suggestion.  Automatically handled by the LMLayer; models should
   * not handle this field.
   */
  transformId?: number;

  /**
   * A unique identifier for the Suggestion itself, not shared with any others -
   * even for Suggestions sourced from the same Transform.
   *
   * The lm-layer is responsible for setting this field, not models.
   */
  id?: number;

  /**
   * The suggested update to the buffer. Note that this transform should
   * be applied AFTER the instigating transform, if any.
   */
  readonly transform: Transform;

  /**
   * A string to display the suggestion to the typist.
   * This should aid the typist understand what the transform
   * will do to their text.
   *
   * When suggesting a word, `displayAs` should be that entire word.
   */
  displayAs: string;

  /**
   * A single metalabel data describing the relation of the suggestion
   * to the input text.  Ex:  'keep', 'emoji', 'correction', etc.
   */
  tag?: SuggestionTag;

  /**
   * Set to true if this suggestion is a valid auto-accept target.
   */
  autoAccept?: boolean
}

interface Reversion extends Suggestion {
  tag: 'revert';
}

interface Keep extends Suggestion {
  tag: 'keep';

  /**
   * Notes whether or not the Suggestion may actually be suggested by the model.
   * Should be `false` if the model does not actually predict the current text.
   */
  matchesModel: boolean;
}

/**
 * A tag indicating the nature of the current suggestion.
 *
 * Tags include:
 *  * 'keep' -- suggest the word as what was typed in the first place.
 *     This tends to be presented as the leftmost suggestion.
 *     @see LexicalModelPunctuation.quotesForKeepSuggestion
 *  * 'correction' -- this suggests a correction to the current phrase
 *  * 'emoji' -- replaces whatever is typed in with an appropriate emoji.
 *    This tends to be presented as the rightmost suggestion.
 *
 * If left undefined, the consumers will assume this is a prediction.
 */
type SuggestionTag = undefined | 'keep' | 'revert' | 'correction' | 'emoji';

/**
 * The text and environment surrounding the insertion point (text cursor).
 */
declare interface Context {
  /**
   * Up to maxLeftContextCodeUnits code units of Unicode scalar value
   * (i. e., characters) to the left of the insertion point in the
   * buffer. If there is nothing to the left of the buffer, this is
   * an empty string.
   */
  readonly left: USVString;

  /**
   * Up to maxRightContextCodeUnits code units of Unicode scalar value
   * (i. e., characters) to the right of the insertion point in the
   * buffer. If there is nothing to the right of the buffer, this is
   * an empty string.
   *
   * This property may be missing entirely.
   */
  readonly right?: USVString;

  /**
   * Whether the insertion point is at the start of the buffer.
   */
  readonly startOfBuffer: boolean;

  /**
   * Whether the insertion point is at the end of the buffer.
   */
  readonly endOfBuffer: boolean;

  /**
   * The casing form to use based on the current layer of the touch layout.
   */
  readonly casingForm?: CasingForm;
}

/**
 * Represents members of a probability distribution over potential outputs
 * from ambiguous text sequences.  Designed for use with fat-finger correction
 * and similar typing ambiguities.
 */
interface ProbabilityMass<T> {
  /**
   * An individual sample from a Distribution over the same type.
   */
  readonly sample: T;

  /**
   * The probability mass for this member of the distribution,
   * calculated devoid of any language-modeling influences.
   */
  p: number;
}

declare type Distribution<T> = ProbabilityMass<T>[];

/**
 * A type augmented with an optional probability.
 */
type Outcome<T> = T & {
  /**
   * [optional] probability of this outcome.
   */
  p?: number;
};

/**
 * A type augmented with a probability.
 */
type WithOutcome<T> = T & {
  /**
   * Probability of this outcome.
   */
  p: number;
};



/******************************** Messaging ********************************/

/**
 * Describes the capabilities of the keyboard's platform.
 * This includes upper bounds for how much text will be sent on each
 * prediction, as well as what operations the keyboard is allowed to do on the
 * underlying buffer.
 */
declare interface Capabilities {
  /**
   * The maximum amount of UTF-16 code points that the keyboard will provide to
   * the left of the cursor, as an integer.
   */
  readonly maxLeftContextCodePoints: number,

  /**
   * The maximum amount of code points that the keyboard will provide to the
   * right of the cursor, as an integer. The value 0 or the absence of this
   * rule implies that the right contexts are not supported.
   */
  readonly maxRightContextCodePoints?: number,

  /**
   * Whether the platform supports deleting to the right. The absence of this
   * rule implies false.
   */
  readonly supportsDeleteRight?: false,
}

/**
 * Configuration of the LMLayer, sent back to the keyboard.
 */
declare interface Configuration {
  /**
   * How many UTF-16 code units maximum to send as the context to the
   * left of the cursor ("left" in the Unicode character stream).
   *
   * Affects the `context` property sent in `predict` messages.
   *
   * While the left context MUST NOT bisect surrogate pairs, they MAY
   * bisect graphical clusters.
   */
  leftContextCodePoints: number;
  /** deprecated; use `leftContextCodePoints` instead! */
  leftContextCodeUnits?: number,

  /**
   * How many UTF-16 code units maximum to send as the context to the
   * right of the cursor ("right" in the Unicode character stream).
   *
   * Affects the `context` property sent in `predict` messages.
   *
   * While the right context MUST NOT bisect surrogate pairs, they MAY
   * bisect graphical clusters.
   */
  rightContextCodePoints: number;
  /** deprecated; use `leftContextCodePoints` instead! */
  rightContextCodeUnits?: number,

  /**
   * Whether or not the model appends characters to Suggestions for
   * wordbreaking purposes.  (These characters need not be whitespace
   * or actual wordbreak characters.)
   *
   * If not specified, this will be auto-detected based on the model's
   * punctuation properties (if they exist).
   */
  wordbreaksAfterSuggestions?: boolean
}


/****************************** Word breaking ******************************/

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
declare interface WordBreakingFunction {
  // invariant: span[i].end <= span[i + 1].start
  // invariant: for all span[i] and span[i + 1], there does not exist a span[k]
  //            where span[i].end <= span[k].start AND span[k].end <= span[i + 1].start
  (phrase: string): Span[];
}

declare interface CasingFunction {
  (caseToApply: CasingForm, text: string, defaultApplyCasing?: CasingFunction): string;
}

/**
 * A span of text in a phrase. This is usually meant to represent words from a
 * pharse.
 */
declare interface Span {
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


/********************************** OTHER **********************************/

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

  /**
   * Whether or not the model's language is typically displayed in RTL form.
   *
   * Default: false (or undefined)
   */
  readonly isRTL?: boolean;
}
