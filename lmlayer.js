/**
 * Prototype LMLayer.
 *
 * The real LMLayer will be far better engineered!
 */

let model;
let createModel;

/**
 * Model definition files must call registerModel() once in order to register
 * an function that returns an initialized Model instance to the LMLayer. The
 * LMLayer may then use the registered Model instance to perform predictions.
 */
self.registerModel = function registerModel(modelFactory /* (c: Configuration) => Model */) {
  createModel = modelFactory;
};

/**
 * Handles messages from the keyboard.
 */
self.onmessage = function (event) {
  const {message} = event.data;

  if (message === 'initialize') {
    onMessageUninitialized(event);
  } else if (message === 'predict') {
    onMessageWhenReady(event);
  } else {
    throw new Error('invalid message');
  }
};

/**
 * Handles message when uninitialzed.
 */
function onMessageUninitialized(event) {
  const {message} = event.data;
  
  if (message !== 'initialize') {
    throw new Error('invalid message');
  }

  // import the model.
  loadModelClass();
  if (createModel === undefined) {
    throw new Error('Did not register a model!');
  }

  model = createModel(event.data.configuration);
  // Ready! Send desired configuration.
  cast('ready', { configuration: model.configuration });
}

/**
 * Handles messages when ready to predict.
 */
function onMessageWhenReady(event) {
  const {message, token} = event.data;

  if (message !== 'predict') {
    throw new Error('invalid message');
  }

  // XXX: induce the other end to reject the promise, because of a
  // token/message mismatch. This is for testing purposes.
  if (token === null) {
    cast('invalid', {token});
    return;
  }

  // TODO: rip contexts out of message.
  let rawSuggestions = model.predict();

  // Sort in-place according to weight.
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

/**
 * Send a message to the keyboard.
 */
function cast(message, parameters) {
  postMessage({message, ...parameters });
}

/**
 * Load the models into the current namespace.
 */
function loadModelClass(_path /* string */) {
  importScripts('./models/en-x-test-derek.js');
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
