/**
 * Prototype LMLayer.
 *
 * The real LMLayer will be far better engineered!
 */

self.onmessage = function (event) {
  const {message, token} = event.data;

  if (message === 'predict') {
    // XXX: cause the other end to reject the promise, because of a
    // token/message mismatch. This is for testing purposes.
    if (token === null) {
      cast('invalid', {token});
      return;
    }

    cast('suggestions', {
      token,
      suggestions: [
        { insert: 'Derek', deleteLeft: 1, deleteRight: 0 },
      ]
    });
  } else {
    throw new Error('invalid message');
  }
};

// Ready! Send desired configuration.
cast('ready', {
  configuration: {
    leftContextCodeUnits: 32
  }
});

/**
 * Send a message to the keyboard.
 */
function cast(message, parameters) {
  postMessage({message, ...parameters });
}
