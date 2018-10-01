const Worker = require('tiny-worker');

exports.LMLayer = class LMLayer {
  constructor() {
    this._worker = new Worker('lmlayer.js');
    this._worker.onmessage = this._onmessage.bind(this);
    this._promises = new Map();
    this._currentToken = 0;
  }

  /**
   * Sends a context, transform, and token to the LMLayer.
   */
  predictWithContext({transform, context, _token}) {
    let token = this._currentToken++;

    return new Promise((resolve, reject) => {
      if (this._promises.has(token)) {
        reject(`Existing request with token ${token}`);
      }
      this._promises.set(token, resolve);

      this._worker.postMessage({
        kind: 'predict',
        token,
        transform,
        context
      });
    });
  }

  /**
   * Handles the wrapped worker's onmessage events.
   */
  _onmessage(event) {
    const {kind, token} = event.data;

    let accept = this._promises.get(token);

    if (!accept) {
      throw new Error(`No promise associated with token: ${token}`);
    }

    if (kind === 'suggestions') {
      this._promises.delete(token);
      accept(event.data);
    } else {
      throw new Error(`Unknown message kind: ${kind}`);
    }
  }
};
