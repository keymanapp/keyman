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
      let previous: Span | undefined;
      let shouldJoinNextSpan = false;

      let originalResults = breaker(input);
      let results: Span[] = [];

      for (let current of originalResults) {
        if (shouldJoinNextSpan) {
          previous = {
            start: previous.start,
            end: current.end,
            length: previous.length + current.length,
            text: previous.text + current.text
          }
          shouldJoinNextSpan = false;
          continue;
        }

        // No join candidate? Now it is!
        if (!previous) {
          previous = current;
          shouldJoinNextSpan = false;
          continue;
        }

        // Should we bother joining these spans?
        if (!includes(delimiters, current.text)) {
          // We can't join them
          results.push(previous);
          previous = current;
          shouldJoinNextSpan = false;
          continue;
        }

        // LET'S JOIN THE TWO SPANS!
        previous = {
          start: previous.start,
          end: current.end,
          length: previous.length + current.length,
          text: previous.text + current.text
        };
        shouldJoinNextSpan = true;
      }

      // Add the leftover span.
      if (previous) {
        results.push(previous);
      }

      return results;
    }

    /**
     * When Array.prototype.include() doesn't exist:
     */
    function includes<T>(haystack: T[], needle: T) {
      for (let item of haystack) {
        if (item === needle)
          return true;
      }
      return false;
    }
  }
}