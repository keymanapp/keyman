// While we _could_ define this within @keymanapp/models-wordbreakers instead, it's probably
// better to leave that package as _just_ the wordbreakers.

import { WordBreakingFunction, USVString, Context } from '@keymanapp/common-types';

export interface Token {
  text: string,
  isWhitespace?: boolean
}

export interface Tokenization {
  /**
   * An array of tokens to the left of the caret.  If the caret is in the middle of a token,
   * only the part to the left of the caret is included.
   */
  left: Token[],

  /**
   * An array of tokens to the right of the caret.  If the caret is in the middle of a token,
   * only the part to the right of the caret is included.
   */
  right: Token[],

  /**
   * A flag indicating whether or not the caret's position in the context caused a token
   * to be split.  If `true`, the last entry of `left` is from the same token as the first
   * entry of `right`.
   */
  caretSplitsToken: boolean
}

export function tokenize(
  wordBreaker: WordBreakingFunction,
  context: Partial<Context>,
  options?: {
    /** Characters to rejoin to preceding tokens if found immediately pre-caret. */
    rejoins?: string[]
  }
): Tokenization {
  // The Unicode word-breaker algorithm looks for places where it's "safe" to
  // split a word across lines, operating upon _completed_ words.  There are
  // some cases where, if placed mid-word, it would add a boundary that does not
  // exist at the end of the word.  The single-quote character is one such
  // location - it's hard to tell if `can'` is the end of a quote or the prefix
  // to `can't`.  So, if `'` is immediately pre-caret, we "rejoin" it.
  const rejoins = options?.rejoins || ["'"];
  context = context || {
    left: undefined,
    startOfBuffer: undefined,
    endOfBuffer: undefined
  };

  const leftSpans  = wordBreaker(context.left  || '') || [];
  const rightSpans = wordBreaker(context.right || '') || [];

  const tokenization: Tokenization = {
    left: [],
    right: [],
    caretSplitsToken: false
  }

  // New step 1:  process left-context.
  let currentIndex = 0;
  while(leftSpans.length > 0) {
    const nextSpan = leftSpans[0];
    if(Math.max(nextSpan.start, currentIndex) != currentIndex) {
      const nextIndex = Math.max(currentIndex, nextSpan.start);
      // Implicit whitespace span!
      tokenization.left.push({
        text: context.left!.substring(currentIndex, nextIndex),
        isWhitespace: true
      });
      currentIndex = nextIndex;
    } else {
      leftSpans.shift();
      // Explicit non-whitespace span.
      tokenization.left.push({
        text: nextSpan.text
      });
      currentIndex = Math.max(currentIndex, nextSpan.end);
    }
  }

  // Detect any pre-caret whitespace after the final pre-caret non-whitespace
  // token
  //
  // Note:  the default wordbreaker won't need this code, as it emits a `''`
  // after final whitespace.
  if(context.left != null && currentIndex != context.left.length) {
    const nextIndex = Math.max(currentIndex, context.left!.length);
    tokenization.left.push({
      text: context.left.substring(currentIndex, nextIndex),
      isWhitespace: true
    });
    currentIndex = nextIndex;
  }

  // New step 2: handle any rejoins needed.

  // Handle any desired special handling for directly-pre-caret scenarios - where for this
  // _specific_ context, we should not make a token division where one normally would exist otherwise.
  //
  // One notable example:  word-final apostrophe is tokenized separate from preceding text, but
  // word-internal apostrophe is treated as part of the same word (i.e, English contractions).
  // But, if the user is editing text and the caret is directly after a caret, there's a notable
  // chance they may in the middle of typing a contraction. Refer to
  // https://github.com/keymanapp/keyman/issues/6572.
  let leftTokenCount = tokenization.left.length;
  if(leftTokenCount > 1) {
    const leftTailBase = tokenization.left[leftTokenCount - 2];
    const leftTail = tokenization.left[leftTokenCount - 1];

    // If the final two pre-caret spans are adjacent - without intervening whitespace...
    if(!leftTailBase.isWhitespace && !leftTail.isWhitespace) {
      // Ideal:  if(leftTailBase is standard-char-class && leftTail is single-quote-class)
      // But we don't have character class access here; it's all wordbreaker-function internal.
      // Upon inspection of the wordbreaker data definitions... the single-quote-class is ONLY "'".
      // So... we'll just be lazy for now and append the `'`.
      if(rejoins.indexOf(leftTail!.text) != -1) {
        tokenization.left.pop(); // leftTail
        tokenization.left.pop(); // leftTailBase
        tokenization.left.push({
          text: leftTailBase.text + leftTail.text
        });
        leftTokenCount--;
      }
    }
  }

  // New step 3: right-context tokenization + token split detection

  // context.right starts from index 0;  it's an 'index reset'.
  currentIndex = 0;
  // Set a flag for special "first token" processing.
  let firstRightToken = true;

  // Note:  is MOSTLY "WET" with the left-span loop, though the
  // `caretSplitsToken` check is additional.
  while(rightSpans.length > 0) {
    const nextSpan = rightSpans[0];
    if(Math.max(nextSpan.start, currentIndex) != currentIndex) {
      const nextIndex = Math.max(currentIndex, nextSpan.start);
      // Implicit whitespace span!
      tokenization.right.push({
        text: context.right!.substring(currentIndex, nextIndex),
        isWhitespace: true
      });
      currentIndex = nextIndex;
    } else {
      const leftTail = tokenization.left[leftTokenCount-1];
      if(leftTail) {
        // If the first non-whitespace token to the right is non-whitespace,
        // and the last token to the left is non-whitespace, the caret may
        // be splitting a token.
        if(firstRightToken && !leftTail.isWhitespace) {
          if(wordBreaker(leftTail!.text + nextSpan.text).length == 1) {
            tokenization.caretSplitsToken = true;
          }
        }
      }

      // Explicit non-whitespace span.
      rightSpans.shift();
      tokenization.right.push({
        text: nextSpan.text
      });
      currentIndex = Math.max(currentIndex, nextSpan.end);
    }

    // We've always processed the "first right token" after the first iteration.
    // Do not run the caret-split check on any future iterations.
    firstRightToken = false;
  }

  // Detect any pre-caret whitespace after the final pre-caret non-whitespace
  // token
  //
  // Note:  the default wordbreaker won't need this code, as it emits a `''`
  // after final whitespace.
  //
  // Also note:  is pretty much WET with the similar check after the
  // leftSpan loop.
  if(context.right && currentIndex != context.right.length) {
    const nextIndex = Math.max(currentIndex, context.right!.length);
    tokenization.right.push({
      text: context.right.substring(currentIndex, nextIndex),
      isWhitespace: true
    });
    currentIndex = nextIndex;
  }

  return tokenization;
}

/**
 * Get the last word of the phrase before the caret or nothing.
 * If the last 'token' before the caret is whitespace, returns `''`.
 * @param fullLeftContext the entire left context of the string.
 */
export function getLastPreCaretToken(wordBreaker: WordBreakingFunction, context: Context): string {
  let tokenization = tokenize(wordBreaker, context);
  if (tokenization.left.length > 0) {
    const lastToken = tokenization.left.pop();
    if(lastToken!.isWhitespace) {
      return '';
    } else {
      return lastToken!.text;
    }
  }

  return '';
}

// While it is currently identical to getLastWord, this may change in the future.
// It's best not to write ourselves into a corner on this one, as disambiguating later
// would likely be pretty painful.
export function wordbreak(wordBreaker: WordBreakingFunction, context: Context): USVString {
  return getLastPreCaretToken(wordBreaker, context);
}
