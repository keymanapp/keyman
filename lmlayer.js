/**
 * Prototype LMLayer.
 *
 * The real LMLayer will be far better engineered!
 */

 let model;

  
self.onmessage = function (event) {
  const {message, token} = event.data;

  // Import the model.

  if (message === 'initialize') {
    const Model = loadModelClass();
    model = new Model(event.data.configuration);
    // Ready! Send desired configuration.
    cast('ready', { configuration: model.configuration });
  } else if (message === 'predict') {
    // XXX: cause the other end to reject the promise, because of a
    // token/message mismatch. This is for testing purposes.
    if (token === null) {
      cast('invalid', {token});
      return;
    }

    let rawSuggestions = model.predict();
    cast('suggestions', {
      token, suggestions: rawSuggestions
    });
  } else {
    throw new Error('invalid message');
  }
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
function loadModelClass() {
  importScripts('./models/en-x-test-derek.js');
  return global.Model;
}