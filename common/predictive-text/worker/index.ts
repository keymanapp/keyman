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

/**
 * @file index.ts
 * 
 * The main LMLayerWorker class, the top-level class within the Web Worker.
 * The LMLayerWorker handles the keyboard/worker communication
 * protocol, delegating prediction requests to the language
 * model implementations.
 */

/// <reference path="../message.d.ts" />
/// <reference path="models/dummy-model.ts" />
/// <reference path="models/wordlist-model.ts" />

/**
 * Encapsulates all the state required for the LMLayer's worker thread.
 * 
 * Implements the state pattern. There are two states:
 * 
 *  - `uninitialized` (initial state)
 *  - `ready`         (accepting state)
 * 
 * Transitions are initiated by valid messages. Invalid
 * messages are errors, and do not lead to transitions.
 * 
 *         +-----------------+            +---------+
 *         |                 | initialize |         |
 *  +------>  uninitialized  +----------->+  ready  +---+
 *         |                 |            |         |   |
 *         +-----------------+            +----^----+   | predict
 *                                             |        |
 *                                             +--------+
 * 
 * The model and the configuration are ONLY relevant in the `ready` state;
 * as such, they are NOT direct properties of the LMLayerWorker.
 */
class LMLayerWorker {
  /**
   * State pattern. This object handles onMessage().
   * handleMessage() can transition to a different state, if
   * necessary.
   */
  private state: LMLayerWorkerState;

  /**
   * By default, it's self.postMessage(), but can be overridden
   * so that this can be tested **outside of a Worker**.
   */
  private _postMessage: PostMessage;

  /**
   * By default, it's self.importScripts(), but can be overridden
   * so that this can be tested **outside of a Worker**.
   * 
   * To function properly, self.importScripts() must be bound to self
   * before being stored here, else it will fail.
   */
  private _importScripts: ImportScripts;

  private _hostURL: string;

  constructor(options = {
    importScripts: null,
    postMessage: null
  }) {
    this._postMessage = options.postMessage || postMessage;
    this._importScripts = options.importScripts || importScripts;
    this.setupInitialState();
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
    // Ensure the message is tagged with a valid message tag.
    if (!message) {
      throw new Error(`Missing required 'message' property: ${event.data}`)
    }

    // We got a message! Delegate to the current state.
    this.state.handleMessage(event.data as IncomingMessage);
  }

  /**
   * Sends back a message structured according to the protocol.
   * @param message A message type.
   * @param payload The message's payload. Can have any properties, except 'message'.
   */
  private cast(message: OutgoingMessageKind, payload: Object) {
    // Chrome raises "TypeError: invalid invocation" if postMessage is called
    // with any non-default value for `this`, i.e., this won't work:
    //
    //  this._postMessage({ foo: 'bar' });
    //
    // Yank it postMessage() off of `this` so that it's called on the
    // "global" context, and everything works again.
    let postMessage = this._postMessage;
    postMessage({ message, ...payload });
  }

  /**
   * Loads a model by executing the given source code, and
   * passing in the appropriate configuration.
   *
   * @param desc         Type of the model to instantiate and its parameters.
   * @param capabilities Capabilities on offer from the keyboard.
   */
  public loadModel(model: WorkerInternalModel) {
    let capabilities = model.getCapabilities();
    let configuration: Configuration = {
      leftContextCodeUnits: 0,
      rightContextCodeUnits: 0
    };

    // Set reasonable defaults for the configuration.
    if (!configuration.leftContextCodeUnits) {
      configuration.leftContextCodeUnits = capabilities.maxLeftContextCodeUnits;
    }
    if (!configuration.rightContextCodeUnits) {
      configuration.rightContextCodeUnits = capabilities.maxRightContextCodeUnits || 0;
    }

    this.transitionToReadyState(model);
    this.cast('ready', { configuration });
  }

  private loadModelFile(url: string) {
    // The self/global WebWorker method, allowing us to directly import another script file into WebWorker scope.
    // If built correctly, the model's script file will auto-register the model with loadModel() above.
    this._importScripts(url);
  }

  public unloadModel() {
    // Right now, this seems sufficient to clear out the old model.
    // The only existing reference to a loaded model is held by 
    // transitionToReadyState's `handleMessage` closure. (The `model` var)
    this.setupInitialState();
  }
  
  public loadWordBreaker(breaker: WorkerInternalWordBreaker) {
    // TODO:  Actually store it somewhere for future use.  Make sure we can forget it with `unloadModel` as well.
  }

  /**
   * Sets the initial state, i.e., `uninitialized`.
   * This state only handles `initialized` messages, and will
   * transition to the `ready` state once it receives a model
   * description and capabilities.
   */
  private setupInitialState() {
    this.state = {
      name: 'uninitialized',
      handleMessage: (payload) => {
        // ...that message must have been 'initialize'!
        if (payload.message !== 'initialize') {
          throw new Error(`invalid message; expected 'initialize' but got ${payload.message}`);
        }

        // TODO: validate configuration?
        this.loadModelFile(payload.model);
      }
    };
  }

  /**
   * Sets the state to `ready`. This requires a
   * fully-instantiated model. The `ready` state only responds
   * to `predict` message, and is an accepting state.
   *
   * @param model The initialized language model.
   */
  private transitionToReadyState(model: WorkerInternalModel) {
    this.state = {
      name: 'ready',
      handleMessage: (payload) => {
        switch(payload.message) {
          case 'predict':
            let {transform, context} = payload;
            this.cast('suggestions', {
              token: payload.token,
              suggestions: model.predict(transform, context)
            });
            break;
          case 'unload':
            this.unloadModel();
            break;
          default:
          throw new Error(`invalid message; expected one of {'predict'} but got ${payload.message}`);
        }
      }
    };
  }

  /**
   * Creates a new instance of the LMLayerWorker, and installs all its
   * functions within the provided Worker global scope.
   *
   * In production, this is called within the Worker's scope as:
   *
   *    LMLayerWorker.install(self);
   *
   * ...and this will setup onmessage and postMessage() appropriately.
   *
   * During testing, this method is useful to mock an entire global scope,
   *
   *    var fakeScope = { postMessage: ... };
   *    LMLayerWorker.install(fakeScope);
   *    // now we can spy on methods in fakeScope!
   *
   * @param scope A global scope to install upon.
   */
  static install(scope: DedicatedWorkerGlobalScope): LMLayerWorker {
    let worker = new LMLayerWorker({ postMessage: scope.postMessage, importScripts: scope.importScripts.bind(scope) });
    scope.onmessage = worker.onMessage.bind(worker);

    // Ensures that the worker instance is accessible for loaded model scripts.
    // Assists unit-testing.
    scope['LMLayerWorker'] = worker;
    scope['models'] = models;

    return worker;
  }
}

// Let LMLayerWorker be available both in the browser and in Node.
if (typeof module !== 'undefined' && typeof module.exports !== 'undefined') {
  module.exports = LMLayerWorker;
  module.exports['models'] = models;
} else if (typeof self !== 'undefined' && 'postMessage' in self) {
  // Automatically install if we're in a Web Worker.
  LMLayerWorker.install(self as DedicatedWorkerGlobalScope);
} else {
  //@ts-ignore
  window.LMLayerWorker = LMLayerWorker;
}
