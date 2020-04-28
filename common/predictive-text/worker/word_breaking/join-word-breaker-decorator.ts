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
      let joinRanges = createJoinRanges(originalSpans);
      let contiguousRanges = fillInGapsInRanges(joinRanges, originalSpans.length);
      return concatenateSpansFromRanges(contiguousRanges, originalSpans);
    }

    /**
     * Returns ranges of indices of results that should be merged.
     *
     * e.g., if joining on "-":
     *
     *  ["-", "yâhk", "ê", "-", "nitawi", "-", "kotiskâwêyâhk", "ni", "-"]
     *    0     1      2    3    4         5    6                7     8[:-]
     *
     * will return:
     *
     *  [[0, 1], [2, 3, 4, 5, 6], [7, 8]]
     *
     * Those indices correspond to spans that should be joined.
     */
    function createJoinRanges(results: Span[]): number[][] {
      let messyJoinRanges: number[][] = [];

      // Figure out where there are spans to join.
      results.forEach((span, index) => {
        if (!includes(delimiters, span.text)) {
          return;
        }

        let window = [];
        if (results[index - 1] && results[index - 1].end === results[index].start) {
          window.push(index - 1);
        }
        window.push(index);

        if (results[index + 1] && results[index].end === results[index + 1].start) {
          window.push(index + 1);
        }

        messyJoinRanges.push(window);
      });

      // Merge overlapping regions:
      return mergeOverlappingRanges(messyJoinRanges);
    }

    /**
     * Given an array of ranges of indices that may have "gaps",
     * e.g., not covering all indices less than {limit},
     * fills in those gaps with "singleton" ranges.
     */
    function fillInGapsInRanges(joinRanges: number[][], limit: number) {
      let contiguousRanges: number[][] = [];
      let insideRange = false;
      let currentJoin = joinRanges.shift();
      for (let index = 0; index < limit; index++) {
        if (insideRange) {
          if (index === lastFrom(currentJoin)) {
            // exit the range
            insideRange = false;
            currentJoin = joinRanges.shift();
          }
        } else {
          if (currentJoin && index === currentJoin[0]) {
            // enter the range
            insideRange = true;
            contiguousRanges.push(currentJoin);
          } else {
            // fill in the gap!
            contiguousRanges.push([index]);
          }
        }
      }
      return contiguousRanges;
    }

    /**
     * Given an array of ranges of indices, and the corresponding spans,
     * concatenates spans according to the ranges of indices.
     */
    function concatenateSpansFromRanges(ranges: number[][], originalSpans: Span[]): Span[] {
      return ranges.map(range => {
        if (range.length === 1) {
          return originalSpans[range[0]];
        }

        // We need to concatenate two or more spans:
        let spansToJoin = range.map(i => originalSpans[i]);
        let currentSpan = spansToJoin.shift();
        while (spansToJoin.length > 0) {
          let nextSpan = spansToJoin.shift();
          currentSpan = concatenateSpans(currentSpan, nextSpan);
        }
        return currentSpan;
      });
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
        if (last >= range[0]) {
          // Remove the overlapping elements:
          while (range.length > 0 && last >= range[0]) {
            range.shift();
            last = lastFrom(lastRange);
          }

          // Extend the last range with what remains.
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