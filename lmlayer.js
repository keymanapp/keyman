/**
 * Prototype LMLayer.
 */

self.onmessage = function (event) {
  const {method, token} = event.data;
  if (method === 'predict') {
    postMessage({
      kind: 'suggestions',
      token,
      suggestions: [
        { insert: 'Derek', deleteLeft: 1, deleteRight: 0 },
      ]
    });
  } else {
    throw new Error('invalid message');
  }
};
