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
type IncomingMessageKind = 'initialize' | 'predict' | 'unload';
type IncomingMessage = InitializeMessage | PredictMessage | UnloadMessage;

/**
 * The structure of an initialization message. It should include the model (either in
 * source code or parameter form), as well as the keyboard's capabilities.
 */
interface InitializeMessage {
  message: 'initialize';

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
  transform?: Transform;

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
 * Represents a state in the LMLayer.
 */
interface LMLayerWorkerState {
  /**
   * Informative property. Name of the state. Currently, the LMLayerWorker can only
   * be the following states:
   */
  name: 'uninitialized' | 'ready';
  handleMessage(payload: IncomingMessage): void;
}

/**
 * The model implementation, within the Worker.
 */
interface WorkerInternalModel {
  getCapabilities(): Capabilities;
  predict(transform: Transform, context: Context): Suggestion[];
}

/**
 * Constructors that return worker internal models.
 */
interface WorkerInternalModelConstructor {
  /**
   * WorkerInternalModel instances are all given the keyboard's
   * capabilities, plus any parameters they require.
   */
  new(capabilities: Capabilities, ...modelParameters: any[]): WorkerInternalModel;
}

interface WorkerInternalWordBreaker {
  break(text: string): string[]; // 
}