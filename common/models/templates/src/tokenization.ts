// While we _could_ define this within @keymanapp/models-wordbreakers instead, it's probably
// better to leave that package as _just_ the wordbreakers.

export interface Tokenization {
  /**
   * An array of tokens to the left of the caret.  If the caret is in the middle of a token,
   * only the part to the left of the caret is included.
   */
  left: USVString[],

  /**
   * An array of tokens to the right of the caret.  If the caret is in the middle of a token,
   * only the part to the right of the caret is included.
   */
  right: USVString[],

  /**
   * A flag indicating whether or not the caret's position in the context caused a token
   * to be split.  If `true`, the last entry of `left` is from the same token as the first
   * entry of `right`.
   */
  caretSplitsToken: boolean
}

export function tokenize(wordBreaker: WordBreakingFunction, context?: Partial<Context>): Tokenization {
  context = context || {
    left: undefined,
    startOfBuffer: undefined,
    endOfBuffer: undefined
  };

  let leftSpans  = wordBreaker(context.left || '') || [];
  let rightSpans = wordBreaker(context.right || '') || [];

  let leftTail: Span;
  if(leftSpans.length > 0) {
    leftTail = leftSpans[leftSpans.length - 1];
  }

  // Handle any desired special handling for directly-pre-caret scenarios - where for this
  // _specific_ context, we should not make a token division where one normally would exist otherwise.
  //
  // One notable example:  word-final apostrophe is tokenized separate from preceding text, but
  // word-internal apostrophe is treated as part of the same word (i.e, English contractions).
  // But, if the user is editing text and the caret is directly after a caret, there's a notable
  // chance they may in the middle of typing a contraction. Refer to
  // https://github.com/keymanapp/keyman/issues/6572.
  if(leftSpans.length > 1) {
    const leftTailBase = leftSpans[leftSpans.length - 2];

    // If the final two pre-caret spans are adjacent - without intervening whitespace...
    if(leftTailBase.end == leftTail!.start) {
      // Ideal:  if(leftTailBase is standard-char-class && leftTail is single-quote-class)
      // But we don't have character class access here; it's all wordbreaker-function internal.
      // Upon inspection of the wordbreaker data definitions... the single-quote-class is ONLY "'".
      // So... we'll just be lazy for now and append the `'`.
      if(leftTail!.text == "'") {
        let mergedSpan: Span = {
          text: leftTailBase.text + leftTail!.text,
          start: leftTailBase.start,
          end: leftTail!.end,
          length: leftTailBase.length + leftTail!.length
        };

        leftSpans.pop(); // leftTail
        leftSpans.pop(); // leftTailBase
        leftSpans.push(mergedSpan);
        leftTail = mergedSpan; // don't forget to update the `leftTail` Span!
      }
    }
  }

  // With any 'detokenization' cases already handled, we may now begin to build the return object.
  let tokenization: Tokenization = {
    left: leftSpans.map(span => span.text),
    right: rightSpans.map(span => span.text),

    // A default initialization of the value.
    caretSplitsToken: false
  };

  // Now the hard part - determining whether or not the caret caused a token split.
  if(leftSpans.length > 0 && rightSpans.length > 0) {
    let rightHead = rightSpans[0];

    // If tokenization includes all characters on each side of the caret,
    // we have a good candidate for a caret-splitting scenario.
    let leftSuffixWordbreak = leftTail!.end != context.left!.length;
    let rightPrefixWordbreak = rightHead.start != 0;

    if(leftSuffixWordbreak || rightPrefixWordbreak) {
      // Bypass the final test, as we already know the caret didn't split a token.
      // (The tokenization process already removed characters between the two.)
      return tokenization;
    }

    // Worth note - some languages don't use wordbreaking characters.  So, a final check:
    //
    // Does the wordbreaker split a merge of the 'two center' tokens?
    // If not, then the caret is responsible for the split.
    if(wordBreaker(leftTail!.text + rightHead.text).length == 1) {
      tokenization.caretSplitsToken = true;
    }
  }

  return tokenization;
}

/**
 * Get the last word of the phrase before the caret or nothing.
 * @param fullLeftContext the entire left context of the string.
 */
export function getLastPreCaretToken(wordBreaker: WordBreakingFunction, context: Context): string {
  let tokenization = tokenize(wordBreaker, context);
  if (tokenization.left.length > 0) {
    return tokenization.left.pop() as string;
  }

  return '';
}

// While it is currently identical to getLastWord, this may change in the future.
// It's best not to write ourselves into a corner on this one, as disambiguating later
// would likely be pretty painful.
export function wordbreak(wordBreaker: WordBreakingFunction, context: Context): USVString {
  return getLastPreCaretToken(wordBreaker, context);
}