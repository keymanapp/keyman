namespace wordBreakers {
  /**
   * Returns a word breaker that joins spans of an existing word breaker.
   * Spans are joined if they are connected by a delimiter.
   *
   * @param breaker The word breaker whose results will be decorated.
   * @param joiners What delimiters should be used to join spans.
   */
  export function join_(breaker: WordBreakingFunction, joiners: string[]): WordBreakingFunction {
    // Make a copy so that if the original array is accidentally mutated, it
    // won't affect the joiner.
    const delimiters = joiners.concat();

    return function (input: string): Span[] {
      let originalSpans = breaker(input);

      let state = emptyStack;
      let stack: Span[] = [];

      for (let span of originalSpans) {
        state = state(span);
      }

      return stack;

      function emptyStack(span: Span) {
        stack.push(span);
        if (includes(delimiters, span.text)) {
          return joined;
        } else {
          return unjoined
        }
      }

      function unjoined(span: Span) {
        // NB: stack has at least one span in it
        if (includes(delimiters, span.text)) {
          if (spansAreBackToBack(lastFrom(stack), span)) {
            concatLastSpanInStackWith(span);
          } else {
            stack.push(span);
          }
          return joined;
        } else {
          stack.push(span);
          return unjoined;
        }
      }

      function joined(span: Span) {
        // NB: stack has at least one span in it
        if (!spansAreBackToBack(lastFrom(stack), span)) {
          stack.push(span);
          return unjoined;
        }

        concatLastSpanInStackWith(span);
        if (includes(delimiters, span.text)) {
          return joined;
        } else {
          return unjoined;
        }
      }

      function concatLastSpanInStackWith(span: Span) {
        let lastIndex = stack.length - 1;

        let top = stack[lastIndex];
        let joinedSpan = concatenateSpans(top, span);
        stack[lastIndex] = joinedSpan;
      }
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
}