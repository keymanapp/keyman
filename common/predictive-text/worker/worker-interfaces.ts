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

/**
 * The valid outgoing message types.
 */
type OutgoingMessageKind = 'ready' | 'suggestions';


/**
 * The structure of an initialization message. It should include the model (either in
 * source code or parameter form), as well as the keyboard's capabilities.
 */
interface InitializeMessage {
  /**
   * The model type, and all of its parameters.
   */
  model: ModelDescription;
  /**
   * The configuration that the keyboard can offer to the model.
   */
  capabilities: Capabilities;
}

/**
 * The structure of the message back to the keyboard.
 */
interface ReadyMessage {
  configuration: Configuration;
}
interface PredictMessage {
}

/**
 * The model implementation, within the Worker.
 */
interface WorkerInternalModel {
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
