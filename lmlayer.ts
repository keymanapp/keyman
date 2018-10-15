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
  * TODO: ...
  */
interface Model {
  readonly configuration: Configuration;
  predict(...args: any): InternalSuggestion[];
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

/**
 * HACK: on Node.JS + tiny-worker, ensure code imported with importScripts()
 * has access to the same things defined on self in this file.
 */
if (typeof global !== 'undefined') {
  let globalPrototype = Object.getPrototypeOf(global);
  Object.setPrototypeOf(self, globalPrototype);
  Object.setPrototypeOf(global, self);
}

/*global importScripts*/
