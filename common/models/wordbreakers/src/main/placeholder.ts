import { Span } from '@keymanapp/common-types';

/**
 * A **VERY** dumb word breaker that simply splits at words. Do not use this
 * word breaker!
 *
 * @param phrase The phrase in which to break words.
 * @deprecated Use a word breaker tailored to your language instead!
 */
export default function placeholder(phrase: string): Span[] {
  let nextStart = 0;
  return phrase.split(/\s+/).map(utterance => {
    // XXX: The indices are NOT accurate to the original phrase!
    let span = {
      start: nextStart,
      end: nextStart + utterance.length,
      text: utterance,
      length: utterance.length
    };
    nextStart = span.end;
    return span;
  });
}
