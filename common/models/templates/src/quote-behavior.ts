import { LexicalModelPunctuation } from '@keymanapp/common-types';

export enum QuoteBehavior {
  noQuotes  = "no-quotes",
  useQuotes = "use-quotes",
  default   = "default-quotes"
}

// For an "enum/namespace merge".  See https://www.typescriptlang.org/docs/handbook/declaration-merging.html#merging-namespaces-with-classes
export namespace QuoteBehavior {
  /**
   * Applies the specified quote behavior to the provided `Transform`, mutating it as appropriate.
   * @param transform    The Transform to be mutated
   * @param punctuation  The active `LexicalModelPunctuation` settings
   * @param defaultTo    The default quote behavior to use (in case the current value is `.default`)
   */
  export function apply(behavior: QuoteBehavior, text: string, punctuation: LexicalModelPunctuation, defaultTo: QuoteBehavior): string {
    if(defaultTo == QuoteBehavior.default || !defaultTo) {
      throw "Specified quote behavior may be ambiguous - default behavior not specified (may not be .default)";
    }

    if(behavior == QuoteBehavior.default) {
      behavior = defaultTo;
    }

    switch(behavior) {
      case QuoteBehavior.noQuotes:
        return text;
      case QuoteBehavior.useQuotes:
        let {open, close} = punctuation.quotesForKeepSuggestion;

        // This part's simple enough, at least.
        return open + text + close;
      default:
        throw "Unsupported quote behavior state detected; implementation missing!";
    }
  }
}

export default QuoteBehavior;
