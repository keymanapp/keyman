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
      let originalResults = breaker(input);

      // Stores indices of spans that should be concatenated.
      // Contiguous indices in will be joined.
      let joinIndices: number[] = [];
      // Figure out where there are spans to join.
      originalResults.forEach((span, index) => {
        if (includes(delimiters, span.text)) {
          joinIndices.push(index - 1);
          joinIndices.push(index);
          joinIndices.push(index + 1);
        }
      });

      // Clean up any invalid indices
      if (joinIndices[0] < 0) {
        joinIndices.shift();
      }
      if (joinIndices[joinIndices.length - 1] >= joinIndices.length) {
        joinIndices.pop();
      }

      // Deduplicated join indices
      joinIndices = joinIndices.reduce((arr, value) => {
        if (value !== arr[arr.length - 1]) {
          arr.push(value)
        }
        return arr;
      }, []);

      // Now let's find contiguous ranges
      let contiguousRanges: number[][] = []
      let currentContiguousRange: number[] | undefined;
      let nextJoin: number | undefined = joinIndices.shift();
      originalResults.forEach((_, index) => {
        if (index === nextJoin) {
          if (currentContiguousRange === undefined) {
            currentContiguousRange = [];
            contiguousRanges.push(currentContiguousRange);
          }
          currentContiguousRange.push(index);
          nextJoin = joinIndices.shift();
        } else {
          // A non-contiguous range
          contiguousRanges.push([index]);
          currentContiguousRange = undefined;
        }
      });

      return contiguousRanges.map(range => {
        if (range.length === 1) {
          return originalResults[range[0]]
        } else {
          let spansToJoin = range.map(i => originalResults[i]);
          let currentSpan = spansToJoin.shift();
          while (spansToJoin.length > 0) {
            let nextSpan = spansToJoin.shift();
            currentSpan = concatenateSpans(currentSpan, nextSpan)
          }
          return currentSpan;
        }
      })
    }

    function concatenateSpans(former: Span, latter: Span) {
      if (latter.start !== former.end) {
        throw new Error(`Cannot concatenate non-contiguous spans: ${former}/${latter}`);
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
    function includes<T>(haystack: T[], needle: T) {
      for (let item of haystack) {
        if (item === needle)
          return true;
      }
      return false;
    }
  }
}