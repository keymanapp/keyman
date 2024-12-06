import { LexicalModelTypes } from '@keymanapp/common-types';
import Span = LexicalModelTypes.Span;
import WordBreakingFunction = LexicalModelTypes.WordBreakingFunction;
import { OverrideScriptDefaults } from "./lexical-model.js";
import { ModelCompilerError, ModelCompilerMessages } from "./model-compiler-messages.js";

export function decorateWithScriptOverrides(breaker: WordBreakingFunction, option: OverrideScriptDefaults) {
  if (option !== 'break-words-at-spaces') {
    throw new ModelCompilerError(ModelCompilerMessages.Error_UnsupportedScriptOverride({option}));
  }

  /**
   * Matches if when a span contains a Southeast-Asian letter or mark anywhere.
   * This makes it a candidate for joining.
   *
   * See: tools/create-override-script-regexp.ts for how this RegExp was
   * generated.
   *
   * Last updated for Unicode 13.0.0.
   */
  const HAS_SOUTHEAST_ASIAN_LETTER = /[\u0E01-\u0E3A\u0E40-\u0E4E\u0E81\u0E82\u0E84\u0E86-\u0E8A\u0E8C-\u0EA3\u0EA5\u0EA7-\u0EBD\u0EC0-\u0EC4\u0EC6\u0EC8-\u0ECD\u0EDC-\u0EDF\u1000-\u103F\u1050-\u108F\u109A-\u109D\u1780-\u17D3\u17D7\u17DC\u17DD\u30A1-\u30FA\u30FC-\u30FF]/;

  return function enhancedBreaker(phrase: string): Span[] {
    let originalSpans = breaker(phrase);

    if (originalSpans.length === 0) {
      return [];
    }

    let outputSpans = [originalSpans.shift()];
    for (let currentSpan of originalSpans) {
      let previousSpan = lastFrom(outputSpans);

      if (spansAreBackToBack(previousSpan, currentSpan) &&
          hasSouthEastAsianLetter(previousSpan) &&
          hasSouthEastAsianLetter(currentSpan)
      ) {
        // previous span SHOULD be joined with current!
        outputSpans[outputSpans.length - 1] = concatenateSpans(previousSpan, currentSpan);
      } else {
        outputSpans.push(currentSpan);
      }
    }

    return outputSpans;
  }

  function hasSouthEastAsianLetter(span: Span) {
    return HAS_SOUTHEAST_ASIAN_LETTER.test(span.text);
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
   * Get the last element from the array.
   */
  function lastFrom<T>(array: T[]): T | undefined {
    return array[array.length - 1];
  }
}
