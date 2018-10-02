/**
 * Prototype LMLayer.
 *
 * The real LMLayer will be far better engineered!
 */

self.onmessage = function (event) {
  const {method, token} = event.data;

  if (method === 'predict') {
    // XXX: cause the other end to reject the promise, because of a
    // token/method mismatch. This is for testing purposes.
    if (token === null) {
      postMessage({ method: 'invalid', token });
      return;
    }

    postMessage({
      method: 'suggestions',
      token,
      suggestions: [
        { insert: 'Derek', deleteLeft: 1, deleteRight: 0 },
      ]
    });
    return;
  } else {
    throw new Error('invalid message');
  }
};
