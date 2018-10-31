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
 * Prototype LMLayer.
 *
 * The real LMLayer will be far better engineered!
 */

// https://www.typescriptlang.org/docs/handbook/triple-slash-directives.html#-reference-lib-

type Weight = number;

// TODO: In a DedicatedWorkerGlobalScope.
interface LMLayerWorkerGlobalScope extends DedicatedWorkerGlobalScope {
  registerModel(factory: ModelFactory): void;
}

interface RequestedConfiguration {
  // TODO
}

interface Configuration {
  /**
   * TODO: ...
   */
  leftContextCodeUnits: number;
}

 /**
  * TODO: I still have to define the internal API for models.
  */
interface Model {
  readonly configuration: Configuration;
  predict(...args: any[]): InternalSuggestion[];
}

/**
 * TODO: ...
 */
interface InternalSuggestion {
  transform: Transform;
  displayAs?: string;
  weight: Weight;
}

/**
 * The function that handles messages from the keyboard.
 */
let onMessage = onMessageWhenUninitialized;

type ModelFactory = (c: RequestedConfiguration) => Model;

/**
 * When defined by registerModel(), you can call this function to instantiate
 * a new model.
 */
let createModel: ModelFactory | undefined;

/**
 * Model definition files must call registerModel() once in order to register
 * a function that returns an initialized Model instance to the LMLayer. The
 * LMLayer may then use the registered Model instance to perform predictions.
 */
(self as LMLayerWorkerGlobalScope).registerModel = function registerModel(modelFactory: ModelFactory) {
  createModel = modelFactory;
};

/**
 * Handles messages from the keyboard.
 *
 * Does some error checking, then delegates to onMessage().
 */
(self as DedicatedWorkerGlobalScope).onmessage = function (event) {
  const {message} = event.data;
  if (!message) {
    throw new Error(`Message did not have a 'message' attribute: ${event.data}`);
  }

  /* Delegate to the current onMessage() handler. */
  return onMessage(event);
};

/**
 * Handles message when uninitialized.
 *
 * Responds only to the `initialize` message.
 */
function onMessageWhenUninitialized(event: MessageEvent) {
  const {message} = event.data as Message;

  if (message !== 'initialize') {
    throw new Error('invalid message');
  }
  const {configuration} = event.data;

  // Import the model.
  let model = loadModel(event.data.model, configuration);
  transitionToReadyState(model);

  // Ready! Send desired configuration.
  cast('ready', { configuration: model.configuration });
}

/**
 * Call this to transition to the 'ready' state.
 *
 * Sets the onMessage handler to the ready handler, with a model.
 */
function transitionToReadyState(model: Model) {
  /**
   * Responds to `predict` messages with a `suggestions` message.
   */
  onMessage = function onMessageWhenReady(event) {
    const {message, token, transform, context} = event.data;

    if (message !== 'predict') {
      throw new Error('invalid message');
    }

    // XXX: induce the other end to reject the promise, because of a
    // token/message mismatch. This is for testing purposes.
    if (token === null) {
      // @ts-ignore
      cast('invalid', {token});
      return;
    }

    let rawSuggestions = model.predict(context, transform);

    // Sort in-place according to weight, ascending.
    rawSuggestions.sort((a, b) => a.weight - b.weight);

    // Convert the internal suggestion format to the one required by the keyboard.
    let suggestions: Suggestion[] = rawSuggestions.map((internal) => {
      let displayAs = internal.displayAs;

      // Try to make up a display string.
      if (displayAs === null || displayAs === undefined) {
        displayAs = internal.transform.insert;
      }

      return {
        transform: internal.transform,
        displayAs
      };
    });

    cast('suggestions', { token, suggestions });
  };
}

/**
 * Send a message to the keyboard.
 */
function cast(message: MessageKind, parameters: {}) {
  postMessage({message, ...parameters });
}

/**
 * Loads the model from a separate file.
 */
function loadModel(path : string , configuration: RequestedConfiguration) {
  importScripts(path);
  /**
   * The model MUST call registerModel() which ultimately defines
   * createModel() to a function.
   */
  if (createModel === undefined) {
    throw new Error('Did not register a model!');
  }

  return createModel(configuration);
}
