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
  /**
   * Processes `config` messages, configuring the newly-loaded model based on the host
   * platform's capability restrictions.
   * 
   * This allows the model to configure its suggestions according to what the platform
   * allows the host to actually perform - for example, if post-caret deletions are not
   * supported, no suggestions requiring this feature should be produced by the model.
   * 
   * Returns a `Configuration` object detailing the capabilities the model plans to
   * actually utilize, which must be as restrictive or more restrictive than those 
   * indicated within the provided `Capabilities` object.
   * @param capabilities 
   */
  configure(capabilities: Capabilities): Configuration;

  /**
   * Generates predictive suggestions corresponding to the state of context after the proposed
   * transform is applied to it.  This transform may correspond to a 'correction' of a recent 
   * keystroke rather than one actually received.
   * 
   * This method should NOT attempt to perform any form of correction; this is modeled within a
   * separate component of the LMLayer predictive engine.  That is, "th" + "e" should not be
   * have "this" for a suggestion ("e" has been 'corrected' to "i"), while "there" would be 
   * a reasonable prediction.  
   * 
   * However, addition of diacritics to characters (which may transform the underlying char code 
   * when Unicode-normalized) is permitted.  For example, "pur" + "e" may reasonably predict
   * "purée", where "e" has been transformed to "é" as part of the suggestion.
   * 
   * When both prediction and correction are permitted, said component (the `ModelCompositor`) will 
   * generally call this method once per 'likely' generated corrected state of the context, 
   * utilizing the results to compute an overall likelihood across all possible suggestions.
   * @param transform A Transform corresponding to a recent input keystroke
   * @param context A depiction of the context to which `transform` is applied.
   * @returns A probability distribution (`Distribution<Suggestion>`) on the resulting `Suggestion` 
   * space for use in determining the most optimal overall suggestions.
   */
  predict(transform: Transform, context: Context): Distribution<Suggestion>;

  /**
   * Performs a wordbreak operation given the current context state, returning whatever word
   * or word fragment exists that starts before the caret but after the most recent whitespace
   * preceding the caret.  If no such text exists, the empty string is returned.
   * 
   * This function is designed for use in generating display text for 'keep' `Suggestions`
   * and display text for reverting any previously-applied `Suggestions`.
   * @param context 
   */
  wordbreak(context: Context): USVString;

  /**
   * Punctuation and presentational settings that the underlying lexical model
   * expects to be applied at higher levels. e.g., the ModelCompositor.
   * 
   * @see LexicalModelPunctuation
   */
  readonly punctuation?: LexicalModelPunctuation;

  /**
   * Represents the complete set of characters that may comprise a string usable
   * for lookup of words within the corresponding lexical model.
   * 
   * Each entry is a pair:  [character: string, frequency: number], where
   * frequency is the frequency of that character across the backing wordlist.
   * (Subject to change - also weighted by the weights of each word the character
   * appears within.)
   */
  characterSet?: [string, number][]; 
}

/**
 * Constructors that return worker internal models.
 */
interface WorkerInternalModelConstructor {
  /**
   * LexicalModel instances are all given the keyboard's
   * capabilities, plus any parameters they require.
   */
  new(...modelParameters: any[]): LexicalModel;
}
