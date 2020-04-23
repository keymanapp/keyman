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
      let joinCandidate: Span | undefined;
      let shouldJoinNext = false;

      let originalResults = breaker(input);
      let results: Span[] = [];

      for (let span of originalResults) {
        // We should join these spans!
        if (shouldJoinNext) {
          joinCandidate = {
            start: joinCandidate.start,
            end: span.end,
            length: joinCandidate.length + span.length,
            text: joinCandidate.text + span.text
          }
          shouldJoinNext = false;
          continue;
        }

        // No join candidate? Now it is!
        if (!joinCandidate) {
          joinCandidate = span;
          shouldJoinNext = false;
          continue;
        }

        // Should we bother joining these spans?
        if (!includes(delimiters, span.text)) {
          // We can't join them
          results.push(joinCandidate);
          joinCandidate = span;
          shouldJoinNext = false;
          continue;
        }

        // LET'S JOIN THE TWO SPANS!
        joinCandidate = {
          start: joinCandidate.start,
          end: span.end,
          length: joinCandidate.length + span.length,
          text: joinCandidate.text + span.text
        };
        shouldJoinNext = true;
      }

      // Add the leftover span.
      if (joinCandidate) {
        results.push(joinCandidate);
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