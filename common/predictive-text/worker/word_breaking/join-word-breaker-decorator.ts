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

      const enum State {
        UNINITIALIZED, // stack is empty
        UNJOINED, // stack always has at least one span
        JOINED // stack always has at least one span
      }

      let state: State = State.UNINITIALIZED;
      let stack: Span[] = [];

      for (let span of originalSpans) {
        switch (state) {
        case State.UNINITIALIZED:
          state = uninitializedState(span);
          break;

        case State.UNJOINED:
          state = unjoinedState(span);
          break;

        case State.JOINED:
          state = joinedState(span);
          break;
        }
      }

      return stack;

      function appendToTopOfStack(span: Span) {
        let top = stack.pop();
        let joinedSpan = concatenateSpans(top, span);
        stack.push(joinedSpan);
      }

      function uninitializedState(span: Span) {
        stack.push(span);
        if (includes(delimiters, span.text)) {
          return State.JOINED;
        } else {
          return State.UNJOINED;
        }
      }

      function unjoinedState(span: Span) {
        if (includes(delimiters, span.text)) {
          // well, now we should join them!
          if (spansAreBackToBack(lastFrom(stack), span)) {
            appendToTopOfStack(span);
          } else {
            stack.push(span);
          }
          return State.JOINED;
        } else {
          stack.push(span);
          return State.UNJOINED;
        }
      }

      function joinedState(span: Span) {
        if (!spansAreBackToBack(lastFrom(stack), span)) {
          stack.push(span);
          return State.UNJOINED;
        }

        appendToTopOfStack(span);

        if (includes(delimiters, span.text)) {
          return State.JOINED;
        } else {
          return State.UNJOINED;
        }
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