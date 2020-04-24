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

      // Stores ranges of indices of spans that should be concatenated.
      let messyJoinRanges: number[][] = createMessyJoinRanges(originalResults);
      let joinRanges = mergeOverlappingRanges(messyJoinRanges);

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

    /**
     * Returns ranges of indices of results that should be merged.
     * Note! These WILL be overlapping! 
     */
    function createMessyJoinRanges(results: Span[]): number[][] {
      let messyJoinRanges: number[][] = [];

      // Figure out where there are spans to join.
      results.forEach((span, index) => {
        if (includes(delimiters, span.text)) {
          messyJoinRanges.push([index - 1, index, index + 1]);
        }
      });

      // Some indices push above are out of range.
      // Get rid of them!
      if (messyJoinRanges.length > 0) {
        if (messyJoinRanges[0][0] < 0) {
          messyJoinRanges[0].shift();
        }
        if (lastFrom(lastFrom(messyJoinRanges)) >= results.length) {
          lastFrom(messyJoinRanges).pop();
        }
      }

      return messyJoinRanges;
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
     * Given ranges like this that may overlap:
     * 
     *  [[1, 2, 3], [3, 4, 5], [6, 7, 8]]
     * 
     * Returns this:
     * 
     *  [[1, 2, 3, 4, 5], [6, 7, 8]]
     */
    function mergeOverlappingRanges(messyRanges: number[][]): number[][] {
      let lastRange: number[] | undefined;
      let ranges: number[][] = [];
      for (let range of messyRanges) {
        if (lastRange == undefined) {
          lastRange = range;
          ranges.push(range);
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
          ranges.push(range);
        }
      }
      return ranges;
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