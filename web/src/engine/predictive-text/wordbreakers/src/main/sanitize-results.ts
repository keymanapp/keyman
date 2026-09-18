/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by jahorton on 2026-09-18
 *
 * This file defines a wordbreaker wrapper that corrects custom wordbreaker
 * whitespace handling and span indexing issues.
 */

import { LexicalModelTypes } from '@keymanapp/common-types';

import Span = LexicalModelTypes.Span;
import WordBreakingFunction = LexicalModelTypes.WordBreakingFunction;

/**
 * Acts as a wrapper that helps the provided wordbreaker report tokens for _all_
 * text in range, not just the parts that aren't whitespace.
 *
 * @param phrase
 */
export function sanitizeResults(breaker: WordBreakingFunction): WordBreakingFunction {
  const wrappedBreaker: WordBreakingFunction = (str) => {
    const sourceSpans = breaker(str);
    const finalSpans: Span[] = [];
    let currentIndex = 0;

    // Note:  spans are based on code units, not code points.
    while(currentIndex <= str.length) {
      let nextSpan = sourceSpans[0];

      // If we're out of spans, then all remaining text must be whitespace.  We don't
      // return that, but we do return one final empty-token Span
      if(!nextSpan) {
        const missingFinalSpan: Span = {
          start: str.length,
          end: str.length,
          length: 0,
          text: ''
        };
        finalSpans.push(missingFinalSpan);

        break;
      } else {
        // ait.mnw.mon's custom breaker does not specify the length property!
        if(nextSpan.length === undefined) {
          nextSpan = {
            ...nextSpan,
            length: nextSpan.end - nextSpan.start
          };
        }
      }

      // Remove any context-starting & non-final empty spans from the detected-spans list.
      // Empty contexts should still report one of them!
      if(
        // If it's an empty span at the start of context...
        (nextSpan.length == 0 && nextSpan.start == 0) &&
        // and there are more remaining spans (or text that results in a new span)
        (sourceSpans.length > 1 || str.length > 0)
      ) {
        // then remove the empty span.
        sourceSpans.shift();
        continue;
      }

      // Naive approaches, like with ait.mnw.mon, may not properly index the spans at all times!
      // We can improve the indexing as follows.
      if(nextSpan.start < currentIndex) {
        const trueStart = str.indexOf(nextSpan.text, currentIndex);

        const replacementSpan: Span = {
          start: trueStart,
          end: trueStart + nextSpan.length,
          length: nextSpan.length,
          text: nextSpan.text
        };

        sourceSpans[0] = replacementSpan;
        nextSpan = replacementSpan;
      }

      // The easy case:  a span already exists!  Easy mode!
      if(nextSpan.start == currentIndex) {
        sourceSpans.shift();
        finalSpans.push(nextSpan);
        currentIndex = nextSpan.end;

        // If the span's end is located at the end of the string, we can terminate now.
        if(nextSpan.end == str.length) {
          break;
        }
        continue;
      }

      // Handling intermediate missing spans
      // const missingSpan: Span = {
      //   start: currentIndex,
      //   end: nextSpan.start,
      //   length: nextSpan.start - currentIndex,
      //   text: str.substring(currentIndex, nextSpan.start)
      // };
      // finalSpans.push(missingSpan);
      currentIndex = nextSpan.start;
    }

    return finalSpans;
  };

  return wrappedBreaker;
}
