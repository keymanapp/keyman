/*
 * Copyright (c) 2018 National Research Council Canada (author: Eddie A. Santos)
 * Copyright (c) 2018 SIL International
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

 type PostMessage = typeof DedicatedWorkerGlobalScope.prototype.postMessage;
 type OutgoingMessageKind = 'ready' | 'suggestions';

 type Message = InitializeMessage
             | ReadyMessage
             | PredictMessage;

interface InitializeMessage {
  message: 'initialize';
  /**
   * Source code of the model.
   * TODO: write a description of what this source code should look like.
   */
  model: string;
  configuration: {
    /**
     * Whether the platform supports deleting to the right.
     * The absence of this rule implies false.
     */
    supportsDeleteRight?: boolean;

    /**
     * The maximum amount of UTF-16 code units that the keyboard will
     * provide to the left of the cursor.
     */
    maxLeftContextCodeUnits: number;

    /**
     * The maximum amount of code units that the keyboard will provide to
     * the right of the cursor. The absence of this setting
     * implies that right contexts are unsupported and will
     * never be supplied.
     */
    maxRightContextCodeUnits?: number;
  }
}

interface ReadyMessage {
  message: 'ready';
  configuration: {
    /**
     * How many UTF-16 code units maximum to send as the context to the
     * left of the cursor ("left" in the Unicode character stream).
     *
     * Affects the `context` property sent in `predict` messages.
     *
     * While the left context MUST NOT bisect surrogate pairs, they MAY
     * bisect graphical clusters.
     */
    leftContextCodeUnits: number,

    /**
     * How many UTF-16 code units maximum to send as the context to the
     * right of the cursor ("right" in the Unicode character stream).
     *
     * Affects the `context` property sent in `predict` messages.
     *
     * While the left context MUST NOT bisect surrogate pairs, they MAY
     * bisect graphical clusters.
     */
    rightContextCodeUnits: number,
  };
}

interface PredictMessage {
  message: 'predict';
  // TODO:
  // token: Token;
  // context: Context;
  // transform: Transform;
}

 /**
  * Encapsulates all the state required for the LMLayer's worker thread.
  */
class LMLayerWorker {
  /**
   * By default, it's self.postMessage(), but can be overridden
   * so that this can be tested **outside of a Worker**.
   */
  private _postMessage: PostMessage;

  constructor(options = {
    postMessage: null,
  }) {
    this._postMessage = options.postMessage || postMessage;
  }

  /**
   * A function that can be set as self.onmessage (the Worker
   * message handler).
   * NOTE! You must bind it to a specific instance, e.g.:
   * 
   *   // Do this!
   *   self.onmessage = worker.onMessage.bind(worker);
   * 
   * Incorrect:
   * 
   *   // Don't do this!
   *   self.onmessage = worker.onMessage;
   * 
   * See: .install();
   */
  onMessage(event: MessageEvent) {
    const {message} = event.data;
    // We must have gotten a message!
    if (!message) {
      throw new Error(`Missing required 'message' attribute: ${event.data}`)
    }

    // ...that message must have been 'initialize'!
    if (message !== 'initialize') {
      throw new Error(`invalid message; expected 'initialize' but got ${message}`);
    }

    let {model, configuration} = this.loadModel(
      event.data.model, event.data.configuration || {}
    );

    // TODO: validate configuration?
    this.cast('ready', { configuration });
  }

  /**
   * Sends back a message structured according to the protocol.
   * @param message A message type.
   * @param payload The message's payload. Can have any properties, except 'message'.
   */
  private cast(message: OutgoingMessageKind, payload: Object) {
    this._postMessage({ message, ...payload });
  }

  /**
   * Loads a model by executing the given source code, and
   * passing in the appropriate configuration.
   *
   * @param modelCode Source code for a function that takes
   *                  configuration, and returns { model, configuration }
   * @param requestedConfiguration Configuration requested from
   *                               the keyboard.
   */
  private loadModel(modelCode, requestedConfiguration) {
    let {model, configuration} = new Function('configuration', modelCode)(requestedConfiguration);
    // Set values correctly
    configuration.leftContextCodeUnits = configuration.leftContextCodeUnits || requestedConfiguration.maxLeftContextCodeUnits;
    configuration.rightContextCodeUnits = requestedConfiguration.maxRightContextCodeUnits ? configuration.rightContextCodeUnits : 0;

    return {model, configuration};
  }

  /**
   * Creates a new instance of the LMLayerWorker, and installs
   * all its functions within the provided Worker scope.
   * 
   * @param scope A global scope to install upon.
   */
  static install(scope: DedicatedWorkerGlobalScope): LMLayerWorker {
    let worker = new LMLayerWorker({ postMessage: scope.postMessage });
    scope.onmessage = worker.onMessage.bind(worker);

    return worker;
  }
}

// Let LMLayerWorker be available both in browser and in Node.
if (typeof module !== 'undefined' && typeof module.exports !== 'undefined') {
  module.exports = LMLayerWorker;
} else {
  //@ts-ignore
  window.LMLayerWorker = LMLayerWorker;
}
