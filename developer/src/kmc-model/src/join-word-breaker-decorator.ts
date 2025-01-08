import { LexicalModelTypes } from '@keymanapp/common-types';
import Span = LexicalModelTypes.Span;
import WordBreakingFunction = LexicalModelTypes.WordBreakingFunction;

/**
 * Returns a word breaker that joins spans of an existing word breaker.
 * Spans are joined if they are connected by a delimiter.
 *
 * @param breaker The word breaker whose results will be decorated.
 * @param joiners What delimiters should be used to join spans.
 */
export function decorateWithJoin(breaker: WordBreakingFunction, joiners: string[]): WordBreakingFunction {
  // Make a copy so that if the original array is accidentally mutated, it
  // won't affect the joiner.
  const delimiters = joiners.concat();

  return function (input: string): Span[] {
    let originalSpans = breaker(input);

    // Implements a finite-state transducer (FST) where:
    //  - Transductions are pushed onto a stack
    //  - There are three states:
    //    - empty stack (initial state)
    //    - unjoined
    //    - joined
    // - all three states are accepting states
    // - there is NO backtracking on the input
    //   (hence the for-loop over the input tape)
    // - each state is a JavaScript callback (function)
    let state = emptyStack;
    let stack: Span[] = [];
    for (let span of originalSpans) {
      state = state(span);
    }

    return stack;

    /******************* States *******************/
    function emptyStack(span: Span) {
      stack.push(span);

      if (isJoiner(span)) {
        return joined;
      } else {
        return unjoined
      }
    }

    function unjoined(span: Span) {
      // NB: stack has at least one span in it
      if (isJoiner(span)) {
        if (spansAreBackToBack(lastFrom(stack), span)) {
          concatLastSpanInStackWith(span);
        } else {
          // Spans are non-contiguous, so don't join them!
          stack.push(span);
        }
        return joined;

      } else {
        // Span cannot be joined
        stack.push(span);
        return unjoined;
      }
    }

    function joined(span: Span) {
      // NB: stack has at least one span in it
      if (!spansAreBackToBack(lastFrom(stack), span)) {
        // Spans are non-contiguous and cannot be joined:
        stack.push(span);
        return unjoined;
      }

      // Spans are contiguous
      concatLastSpanInStackWith(span);
      if (isJoiner(span)) {
        return joined;
      } else {
        return unjoined;
      }
    }

    /****************** Helpers ******************/
    function concatLastSpanInStackWith(span: Span) {
      let lastIndex = stack.length - 1;

      let top = stack[lastIndex];
      let joinedSpan = concatenateSpans(top, span);
      stack[lastIndex] = joinedSpan;
    }
  }

  function isJoiner(span: Span) {
    return includes(delimiters, span.text);
  }

  /**
   * Returns true when the spans are contiguous.
   * Order matters when calling this function!
   */
  function spansAreBackToBack(former: Span, latter: Span): boolean {
    return former.end === latter.start;
  }

  function concatenateSpans(former: Span, latter: Span) {
    if (latter.start !== former.end) {
      throw new Error(`Cannot concatenate non-contiguous spans: ${JSON.stringify(former)}/${JSON.stringify(latter)}`);
    }

    return {
      start: former.start,
      end: latter.end,
      length: former.length + latter.length,
      text: former.text + latter.text
    };
  }

  /**
   * When Array.prototype.include() doesn't exist:
   */
  function includes<T>(haystack: T[], needle: T): boolean {
    for (let item of haystack) {
      if (item === needle)
        return true;
    }
    return false;
  }

  /**
   * Get the last element from the array.
   */
  function lastFrom<T>(array: T[]): T | undefined {
    return array[array.length - 1];
  }
}
