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
type IncomingMessageKind = 'config' | 'load' | 'predict' | 'unload' | 'wordbreak' | 'accept';
type IncomingMessage = ConfigMessage | LoadMessage | PredictMessage | UnloadMessage | WordbreakMessage | AcceptMessage;

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

interface AcceptMessage {
  message: 'accept';

  /**
   * Opaque, unique token that pairs this accept message with its return message.
   */
  token: Token;

  /**
   * The Suggestion being accepted.  The ID must be assigned.
   */
  suggestion: Suggestion & {id: number};

  /**
   * The context (text to the left and text to right) at the
   * insertion point/text cursor, at the moment the Suggestion
   * was generated.
   */
  context: Context;

  /**
   * A Transform representing any text manipulations applied to 
   * the Context after the `suggestion` was generated.  
   * 
   * Necessary, as Suggestions are generated without applying their
   * triggering keystroke to the Context.  (The current context is 
   * thus likely to differ.)
   */
  postTransform?: Transform;
}

/**
 * The LMLayer can be in one of the following states. The LMLayer can only produce predictions in the 'ready' state.
 */
type LMLayerWorkerState = LMLayerWorkerUnconfiguredState | LMLayerWorkerModellessState | LMLayerWorkerReadyState;

/**
 * Represents the unconfigured state of the LMLayer.
 */
interface LMLayerWorkerUnconfiguredState {
  name: 'unconfigured';
  handleMessage(payload: IncomingMessage): void;
}

/**
 * Represents the pre-model-load state of the LMLayer.
 */
interface LMLayerWorkerModellessState {
  name: 'modelless';
  handleMessage(payload: IncomingMessage): void;
}

/**
 * Represents the 'ready' state of the LMLayer.
 */
interface LMLayerWorkerReadyState {
  name: 'ready';
  handleMessage(payload: IncomingMessage): void;
  compositor: ModelCompositor;
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
