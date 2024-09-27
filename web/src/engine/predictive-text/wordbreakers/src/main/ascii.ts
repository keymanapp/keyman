import { Span } from '@keymanapp/common-types';

/**
 * Splits ASCII words.
 *
 * @param phrase
 */
export default function ascii(phrase: string): Span[] {
  let matchWord = /[A-Za-z0-9']+/g;
  let words: Span[] = [];
  let match: RegExpExecArray | null;
  while ((match = matchWord.exec(phrase)) !== null) {
    words.push(new RegExpDerivedSpan(match[0], match.index));
  }

  return words;
}

/**
 * A concrete span class that derives its properties from the result of
 * RegExp.exec() array.
 */
class RegExpDerivedSpan implements Span {
  readonly text: string;
  readonly start: number;

  constructor(text: string, start: number) {
    this.text = text;
    this.start = start;
  }

  get length(): number {
    return this.text.length;
  }

  get end(): number {
    return this.start + this.text.length;
  }
}
