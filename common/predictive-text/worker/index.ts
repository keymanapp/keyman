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
 type ImportScripts = typeof DedicatedWorkerGlobalScope.prototype.importScripts;
 type Message = 'ready' | 'initialize';

 /**
  * Encapsulates all the state required for the LMLayer's worker thread.
  */
class LMLayerWorker {
  /**
   * By default, it's self.postMessage(), but can be overridden
   * so that this can be tested **outside of a Worker**.
   */
  private _postMessage: PostMessage;
  private _importScripts: ImportScripts;

  constructor(options = {
    postMessage: null,
    importScripts: null
  }) {
    this._postMessage = options.postMessage || postMessage;
    this._importScripts = options.importScripts || importScripts;
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
    if (!message) {
      throw new Error(`Missing required 'message' attribute: ${event.data}`)
    }
    this.cast('ready', {
      configuration: {
        // Send a reasonable, but non-configurable amount for now.
        leftContextCodeUnits: 64,
        rightContextCodeUnits: 0,
      }
    });
  }

  /**
   * Sends back a message structured according to the protocol.
   * @param message A message type.
   * @param payload The message's payload. Can have any properties, except 'message'.
   */
  private cast(message: Message, payload: object) {
    this._postMessage({ message, ...payload });
  }

  /**
   * Creates a new instance of the LMLayerWorker, and installs
   * all its functions within the provided Worker scope.
   * 
   * @param scope A global scope to install upon.
   */
  static install(scope: DedicatedWorkerGlobalScope): LMLayerWorker {
    let worker = new LMLayerWorker({ 
      postMessage: scope.postMessage,
      importScripts: scope.importScripts
    });
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
