const Worker = require('tiny-worker');

/**
 * Encapuslates the underlying Web Worker through asynchronous calls.
 */
exports.LMLayer = class LMLayer {
  constructor() {

    // Worker state
    this._worker = new Worker('lmlayer.js');
    this._worker.onmessage = this._onmessage.bind(this);

    // Keep track of individual requests to make a nice async/await API.
    this._promises = new Map();

    // Keep track of tokens.
    this._currentToken = Number.MIN_SAFE_INTEGER;
  }

  /**
   * Sends a context, transform, and token to the LMLayer.
   */
  predictWithContext({transform, context, _token}) {
    let token = this._nextToken();

    return new Promise((resolve, reject) => {
      if (this._promises.has(token)) {
        reject(`Existing request with token ${token}`);
      }
      this._promises.set(token, resolve);

      this._cast('predict', {
        token, transform, context
      });
    });
  }


  /**
   * Throw an asynchronous method call (a "cast") over to the Web Worker.
   */
  _cast(method, payload) {
    this._worker.postMessage({ method, ...payload });
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

  /**
   * Returns the next token. Note: mutates state.
   */
  _nextToken() {
    let token = this._currentToken++;
    if (!Number.isSafeInteger(token)) {
      throw new RangeError('Ran out of usable tokens');
    }
    return token;
  }
};
