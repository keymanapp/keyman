/**
 * Prototype LMLayer.
 *
 * The real LMLayer will be far better engineered!
 */

/**
 * The function that handles messages from the keyboard.
 */
let onMessage = onMessageWhenUninitialized;

/**
 * When defined by registerModel(), you can call this function to instantiate
 * a new model.
 */
let createModel;

/**
 * Model definition files must call registerModel() once in order to register
 * a function that returns an initialized Model instance to the LMLayer. The
 * LMLayer may then use the registered Model instance to perform predictions.
 */
self.registerModel = function registerModel(modelFactory /* (c: Configuration) => Model */) {
  createModel = modelFactory;
};

/**
 * Handles messages from the keyboard.
 *
 * Does some error checking, then delegates to onMessage().
 */
self.onmessage = function (event) {
  const {message} = event.data;
  if (!message) {
    throw new Error('Message did not have a `message` attribute', event.data);
  }

  /* Delegate to the current onMessage() handler. */
  return onMessage(event);
};

/**
 * Handles message when uninitialized.
 *
 * Responds only to the `initialize` message.
 */
function onMessageWhenUninitialized(event) {
  const {message, configuration} = event.data;

  if (message !== 'initialize') {
    throw new Error('invalid message');
  }

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
function transitionToReadyState(model) {
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
      cast('invalid', {token});
      return;
    }

    let rawSuggestions = model.predict(context, transform);

    // Sort in-place according to weight, ascending.
    rawSuggestions.sort((a, b) => a.weight - b.weight);

    // Convert the internal suggestion format to the one required by the keyboard.
    let suggestions = rawSuggestions.map((internal) => {
      let displayAs = internal.displayAs;

      // Try to make up a display string.
      if (displayAs === null || displayAs === undefined) {
        displayAs = internal.transform.insert;
      }

      return { displayAs, ...internal.transform };
    });

    cast('suggestions', { token, suggestions });
  };
}

/**
 * Send a message to the keyboard.
 */
function cast(message, parameters) {
  postMessage({message, ...parameters });
}

/**
 * Loads the model from a separate file.
 */
function loadModel(path /* : string */, configuration /* : Configuration */) {
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
