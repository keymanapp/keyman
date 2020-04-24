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
      let messyJoinRanges: number[][] = [];

      // Figure out where there are spans to join.
      originalResults.forEach((span, index) => {
        if (includes(delimiters, span.text)) {
          messyJoinRanges.push([index - 1, index, index + 1]);
        }
      });

      // Clean up any invalid indices pushed above.
      if (messyJoinRanges.length > 0) {
        if (messyJoinRanges[0][0] < 0) {
          messyJoinRanges[0].shift();
        }
        if (lastFrom(lastFrom(messyJoinRanges)) >= originalResults.length) {
          lastFrom(messyJoinRanges).pop();
        }
      }

      // Join together ranges.
      let lastRange: number[] | undefined;
      let joinRanges: number[][] = []
      for (let range of messyJoinRanges) {
        if (lastRange == undefined) {
          lastRange = range;
          joinRanges.push(range);
          continue;
        }

        let last = lastFrom(lastRange);
        if (last === range[0]) {
          // exclude the first element:
          range.shift();
          for (let v of range) {
            lastRange.push(v);
          }
        } else {
          lastRange = range;
          joinRanges.push(range);
        }
      }
      
      let contiguousRanges: number[][] = [];
      let insideRange = false;
      let currentJoin = joinRanges.shift();
      originalResults.forEach((_, index) => {
        if (insideRange) {
          if (index === lastFrom(currentJoin)) {
            insideRange = false;
            currentJoin = joinRanges.shift();
          }
        } else {
          if (currentJoin && index === currentJoin[0]) {
            insideRange = true;
            contiguousRanges.push(currentJoin);
          } else {
            contiguousRanges.push([index]);
          }
        }
      })

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
      });
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
    function includes<T>(haystack: T[], needle: T) {
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