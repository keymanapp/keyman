/// <reference types="@keymanapp/lm-message-types" />

/*
 * Copyright (c) 2018 National Research Council Canada (author: Eddie A. Santos)
 * Copyright (c) 2018 SIL International
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */


/**
 * @file dummy-model.ts
 *
 * Defines the Dummy model, which is used for testing the
 * prediction API exclusively.
 */

interface Wordform2Key {
  (wordform: string): string;
}

export interface DummyOptions {
  toKey?: Wordform2Key,
  futureSuggestions?: Outcome<Suggestion>[][];
  punctuation?: LexicalModelPunctuation;
  applyCasing?: (form: CasingForm, text: string) => string;
  wordbreaker?: WordBreakingFunction;
  languageUsesCasing?: boolean;
}

/**
 * The Dummy Model that returns nonsensical, but predictable results.
 */
export class DummyModel implements LexicalModel {
  configuration: Configuration;
  punctuation?: LexicalModelPunctuation;
  toKey?: Wordform2Key;
  applyCasing?: (form: CasingForm, text: string) => string;
  wordbreaker?: WordBreakingFunction;
  languageUsesCasing?: boolean;

  private _futureSuggestions: Outcome<Suggestion>[][];

  constructor(options?: DummyOptions) {
    options = options || {};
    // Create a shallow copy of the suggestions;
    // this class mutates the array.
    this._futureSuggestions = options.futureSuggestions
      ? options.futureSuggestions.slice() : [];

    if (options.punctuation) {
      this.punctuation = options.punctuation;
    }

    if (options.toKey) {
      this.toKey = options.toKey;
    }

    if (options.wordbreaker) {
      this.wordbreaker = options.wordbreaker;
    }

    if (options.applyCasing) {
      this.applyCasing = options.applyCasing;
    }

    if (options.languageUsesCasing) {
      this.languageUsesCasing = options.languageUsesCasing;
    }
  }

  configure(capabilities: Capabilities): Configuration {
    this.configuration = {
      leftContextCodePoints: capabilities.maxLeftContextCodePoints,
      rightContextCodePoints: capabilities.maxRightContextCodePoints
    };

    return this.configuration;
  }

  predict(transform: Transform, context: Context, injectedSuggestions?: Outcome<Suggestion>[]): Distribution<Suggestion> {
    let makeUniformDistribution = function(suggestions: Outcome<Suggestion>[]): Distribution<Suggestion> {
      let distribution: Distribution<Suggestion> = [];

      for(let s of suggestions) {
        distribution.push({sample: s, p: s.p !== undefined ? s.p : 1});  // For a dummy model, this is sufficient.  The uniformness is all that matters.
      }

      return distribution;
    }

    if (injectedSuggestions) {
      return makeUniformDistribution(injectedSuggestions);
    }

    let currentSet = this._futureSuggestions.shift();

    if(!currentSet) {
      return [];
    } else {
      return makeUniformDistribution(currentSet);
    }
  }
};

export default DummyModel;