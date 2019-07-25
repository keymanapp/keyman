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
 * @file worker-interfaces.ts
 *
 * Interfaces and types required internally in the worker code.
 */

/// <reference path="../message.d.ts" />

/**
 * The signature of self.postMessage(), so that unit tests can mock it.
 */
type PostMessage = typeof DedicatedWorkerGlobalScope.prototype.postMessage;
type ImportScripts = typeof DedicatedWorkerGlobalScope.prototype.importScripts;


/**
 * The valid incoming message kinds.
 */
type IncomingMessageKind = 'config' | 'load' | 'predict' | 'unload' | 'wordbreak';
type IncomingMessage = ConfigMessage | LoadMessage | PredictMessage | UnloadMessage | WordbreakMessage;

/**
 * The structure of a config message.  It should include the platform's supported
 * capabilities.
 */
interface ConfigMessage {
  message: 'config';

  /**
   * The platform's supported capabilities.
   */
  capabilities: Capabilities;
}

/**
 * The structure of an initialization message. It should include the model (either in
 * source code or parameter form), as well as the keyboard's capabilities.
 */
interface LoadMessage {
  message: 'load';

  /**
   * The model's compiled JS file.
   */
  model: string;
}

/**
 * Message to suggestion text.
 */
interface PredictMessage {
  message: 'predict';

  /**
   * Opaque, unique token that pairs this predict message with its suggestions.
   */
  token: Token;

  /**
   * How the input event will transform the buffer.
   * If this is not provided, then the prediction is not
   * assumed to be associated with an input event (for example,
   * when a user starts typing on an empty text field).
   *
   * TODO: test for absent transform!
   */
  transform?: Transform | Distribution<Transform>;

  /**
   * The context (text to the left and text to right) at the
   * insertion point/text cursor, at the moment before the
   * transform is applied to the buffer.
   */
  context: Context;
}

interface UnloadMessage {
  message: 'unload'
}

/**
 * Message used to request the last pre-cursor word in the context.
 */
interface WordbreakMessage {
  message: 'wordbreak';

  /**
   * Opaque, unique token that pairs this wordbreak message with its return message.
   */
  token: Token;

  /**
   * The context (text to the left and text to right) at the
   * insertion point/text cursor.
   */
  context: Context;
}


/**
 * Represents a state in the LMLayer.
 */
interface LMLayerWorkerState {
  /**
   * Informative property. Name of the state. Currently, the LMLayerWorker can only
   * be the following states:
   */
  name: 'unconfigured' | 'modelless' | 'ready';
  handleMessage(payload: IncomingMessage): void;
}

/**
 * The model implementation, within the Worker.
 */
interface WorkerInternalModel {
  configure(capabilities: Capabilities): Configuration;
  predict(transform: Transform, context: Context): Distribution<Suggestion>;
  wordbreak(context: Context): USVString;

  /**
   * Punctuation and presentational settings that the underlying lexical model
   * expects to be applied at higher levels. e.g., the ModelCompositor.
   * 
   * @see LexicalModelPunctuation
   */
  readonly punctuation?: LexicalModelPunctuation;
}

/**
 * Constructors that return worker internal models.
 */
interface WorkerInternalModelConstructor {
  /**
   * WorkerInternalModel instances are all given the keyboard's
   * capabilities, plus any parameters they require.
   */
  new(...modelParameters: any[]): WorkerInternalModel;
}

/**
 * A simple word breaking function takes a phrase, and splits it into "words",
 * for whatever definition of "word" is usable for the language model.
 *
 * For example:
 *
 *   getText(breakWordsEnglish("Hello, world!")) == ["Hello", "world"]
 *   getText(breakWordsCree("ᑕᐻ ᒥᔪ ᑮᓯᑲᐤ ᐊᓄᐦᐨ᙮")) == ["ᑕᐻ", "ᒥᔪ ᑮᓯᑲᐤ""", "ᐊᓄᐦᐨ"]
 *   getText(breakWordsJapanese("英語を話せますか？")) == ["英語", "を", "話せます", "か"]
 *
 * Not all language models take in a configurable word breaking function.
 *
 * @returns an array of spans from the phrase, in order as they appear in the
 *          phrase, each span which representing a word.
 */
interface WordBreakingFunction {
  // invariant: span[i].end <= span[i + 1].start
  // invariant: for all span[i] and span[i + 1], there does not exist a span[k]
  //            where span[i].end <= span[k].start AND span[k].end <= span[i + 1].start
  (phrase: USVString): Span[];
}

/**
 * A span of text in a phrase. This is usually meant to reprent words from a
 * pharse.
 */
interface Span {
  // invariant: start < end (empty spans not allowed)
  readonly start: number;
  // invariant: end > end (empty spans not allowed)
  readonly end: number;
  // invariant: length === end - start
  readonly length: number;
  // invariant: text.length === length
  // invariant: each character is BMP UTF-16 code unit, or is a high surrogate
  // UTF-16 code unit followed by a low surrogate UTF-16 code unit.
  readonly text: string;
}

/**
 * Options for various punctuation to use in suggestions.
 */
interface LexicalModelPunctuation {
  /**
   * The quotes that appear in "keep" suggestions, e.g., keep what the user
   * typed verbatim.
   * 
   * The keep suggestion is often the leftmost one, when suggested.
   * 
   * [ “Hrllo” ] [ Hello ] [ Heck ]
   */
  readonly quotesForKeepSuggestion: {
    /**
     * What will appear on the opening side of the quote.
     * (left side for LTR scripts; right side for RTL scripts)
     *
     * Default: `“`
     */
    readonly open: string;
    /**
     * What will appear on the closing side of the quote.
     * (right side for LTR scripts; left side for RTL scripts)
     *
     * Default: `”`
     */
    readonly close: string;
  };
  /**
   * What punctuation or spacing to insert after every complete word
   * prediction. This can be set to the empty string when the script does not
   * use spaces to separate words.
   * 
   * Default: ` `
   */
  readonly insertAfterWord: string;
}