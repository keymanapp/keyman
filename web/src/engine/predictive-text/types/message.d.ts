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

import { LexicalModelTypes } from '@keymanapp/common-types';
import Configuration = LexicalModelTypes.Configuration;
import Reversion = LexicalModelTypes.Reversion;
import Suggestion = LexicalModelTypes.Suggestion;
import USVString = LexicalModelTypes.USVString;

/**
 * Tokens are signed 31-bit integers!
 */
type Token = number;

/**
 * The valid outgoing message kinds.
 */
type OutgoingMessageKind = 'error' | 'ready' | 'suggestions' | 'currentword' | 'postaccept' | 'postrevert';
type OutgoingMessage = ErrorMessage | ReadyMessage | SuggestionMessage | CurrentWordMessage | PostAcceptMessage | PostRevertMessage;

interface ErrorMessage {
  message: 'error';
  error?: any;
  log: string;
}

/**
 * Tells the keyboard that the LMLayer is ready. Provides
 * negotiated configuration.
 */
interface ReadyMessage {
  message: 'ready';
  configuration: Configuration;
}

/**
 * Sends the keyboard an ordered list of suggestions.
 */
interface SuggestionMessage {
  message: 'suggestions';

  /**
   * Opaque, unique token that pairs this suggestions message
   * with the predict message that initiated it.
   */
  token: Token;

  /**
   * Ordered array of suggestions, most probable first, least
   * probable last.
   */
  suggestions: Suggestion[];
}

/**
 * Returns the results of a 'wordbreak' request:  the current left-of-caret word.
 */
interface CurrentWordMessage {
  message: 'currentword';

  /**
   * Opaque, unique token that pairs this message
   * with the wordbreak message that initiated it.
   */
  token: Token;

  /**
   * Contains the 'current word' left of the caret given the Context
   * of its source message - the 'wordbreak' message with matching Token value.
   */
  word: USVString;
}

/**
 * Returns the results of a 'wordbreak' request:  a 'reversion' Suggestion and an
 * array of new, word-initial Suggestions.
 */
interface PostAcceptMessage {
  message: 'postaccept';

  /**
   * Opaque, unique token that pairs this message
   * with the accept message that initiated it.
   */
  token: Token;

  /**
   * A 'Reversion' that will return the context to its prior state.
   */
  reversion: Reversion;
}

interface PostRevertMessage {
  message: 'postrevert';

  /**
   * Opaque, unique token that pairs this message
   * with the revert message that initiated it.
   */
  token: Token;

  /**
   * The original set of Suggestions returned that included the
   * reverted Suggestion
   */
  suggestions: Suggestion[];
}

/**
 * Describes what kind of model to instantiate.
 */
type ModelDescription = DummyModelDescription | WordListModelDescription;
interface DummyModelDescription {
  type: 'dummy';
  futureSuggestions?: Suggestion[][];
}
interface WordListModelDescription {
  type: 'wordlist';
  /**
   * Words to predict, one word per entry in the array.
   */
  wordlist: string[];
}
