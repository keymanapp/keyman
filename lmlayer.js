/**
 * Prototype LMLayer.
 */

self.onmessage = function (event) {
  const {kind, token} = event.data;
  if (kind === 'predict') {
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
