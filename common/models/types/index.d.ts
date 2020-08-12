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

declare interface LexiconTraversal {
  children(): Generator<{key: USVString, traversal: () => LexiconTraversal}>;
  entries: USVString[];
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
   * Generates predictive suggestions corresponding to the state of context after the proposed
   * transform is applied to it.  This transform may correspond to a 'correction' of a recent 
   * keystroke rather than one actually received.
   * 
   * This method should NOT attempt to perform any form of correction; this is modeled within a
   * separate component of the LMLayer predictive engine.  That is, "th" + "e" should not be
   * have "this" for a suggestion ("e" has been 'corrected' to "i"), while "there" would be 
   * a reasonable prediction.  
   * 
   * However, addition of diacritics to characters (which may transform the underlying char code 
   * when Unicode-normalized) is permitted.  For example, "pur" + "e" may reasonably predict
   * "purée", where "e" has been transformed to "é" as part of the suggestion.
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
   * Performs a wordbreak operation given the current context state, returning whatever word
   * or word fragment exists that starts before the caret but after the most recent whitespace
   * preceding the caret.  If no such text exists, the empty string is returned.
   * 
   * This function is designed for use in generating display text for 'keep' `Suggestions`
   * and display text for reverting any previously-applied `Suggestions`.
   * @param context 
   */
  wordbreak(context: Context): USVString;

  /**
   * Punctuation and presentational settings that the underlying lexical model
   * expects to be applied at higher levels. e.g., the ModelCompositor.
   * 
   * @see LexicalModelPunctuation
   */
  readonly punctuation?: LexicalModelPunctuation;

  /**
   * Lexical models _may_ provide a LexiconTraversal object usable to enhance 
   * prediction and correction results.  The returned object represents the
   * unfiltered lexicon (with an empty prefix).
   */
  getRootTraversal?(): LexiconTraversal;
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
type SuggestionTag = undefined | 'keep' | 'correction' | 'emoji';


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
