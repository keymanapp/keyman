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
/// <reference path="../../../web/source/text/kmwstring.ts" />
/// <reference path="models/dummy-model.ts" />
/// <reference path="word_breaking/ascii-word-breaker.ts" />
/// <reference path="./model-compositor.ts" />

/**
 * Encapsulates all the state required for the LMLayer's worker thread.
 * 
 * Implements the state pattern. There are three states:
 * 
 *  - `unconfigured`  (initial state before configuration)
 *  - `modelless`     (state without model loaded)
 *  - `ready`         (state with model loaded, accepts prediction requests)
 * 
 * Transitions are initiated by valid messages. Invalid
 * messages are errors, and do not lead to transitions.
 * 
 *          +-------------+    load    +---------+
 *   config |             |----------->|         |
 *  +------->  modelless  +            +  ready  +---+
 *          |             |<-----------|         |   |
 *          +-------------+   unload   +----^----+   | predict
 *                                          |        |
 *                                          +--------+
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

  private _platformCapabilities: Capabilities;

  private _hostURL: string;

  private _currentModelSource: string;

  constructor(options = {
    importScripts: null,
    postMessage: null
  }) {
    this._postMessage = options.postMessage || postMessage;
    this._importScripts = options.importScripts || importScripts;
    this.setupConfigState();
  }

  public error(message: string, error?: any) {
    // error isn't a fan of being cloned across the worker boundary.
    this.cast('error', {
      log: message,
      error: (error && error.stack) ? error.stack : undefined
    });
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

    // If last load was for this exact model file, squash the message.
    // (Though not if we've had an unload since.)
    let im = event.data as IncomingMessage;
    if(im.message == 'load') {
      let data = im as LoadMessage;
      if(data.model == this._currentModelSource) {
        // Some JS implementations don't allow web workers access to the console.
        if(typeof console !== 'undefined') {
          console.warn("Duplicate model load message detected - squashing!");
        }
        return;
      } else {
        this._currentModelSource = data.model;
      }
    } else if(im.message == 'unload') {
      this._currentModelSource = null;
    }

    // We got a message! Delegate to the current state.
    this.state.handleMessage(im);
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
  public loadModel(model: LexicalModel) {
    // TODO:  pass _platformConfig to model so that it can self-configure to the platform,
    // returning a Configuration.

    /* Note that this function is typically called from within an `importScripts` call.
     * For meaningful error messages to be successfully logged, we must catch what we can here
     * and pass a message to outside the worker - otherwise a generic "Script error" occurs.
     */
    try {
      let configuration = model.configure(this._platformCapabilities);

      // Handle deprecations.
      if(!configuration.leftContextCodePoints) {
        configuration.leftContextCodePoints = configuration.leftContextCodeUnits;
      }
      if(!configuration.rightContextCodePoints) {
        configuration.rightContextCodePoints = configuration.rightContextCodeUnits;
      }

      // Set reasonable defaults for the configuration.
      if (!configuration.leftContextCodePoints) {
        configuration.leftContextCodePoints = this._platformCapabilities.maxLeftContextCodePoints;
      }
      if (!configuration.rightContextCodePoints) {
        configuration.rightContextCodePoints = this._platformCapabilities.maxRightContextCodePoints || 0;
      }

      this.transitionToReadyState(model);
      this.cast('ready', { configuration });
    } catch (err) {
      this.error("loadModel failed!", err);
    }
  }

  private loadModelFile(url: string) {
    // The self/global WebWorker method, allowing us to directly import another script file into WebWorker scope.
    // If built correctly, the model's script file will auto-register the model with loadModel() above.
    try {
      this._importScripts(url);
    } catch (err) {
      this.error("Error occurred when attempting to load dictionary", err);
    }
  }

  public unloadModel() {
    // Right now, this seems sufficient to clear out the old model.
    // The only existing reference to a loaded model is held by 
    // transitionToReadyState's `handleMessage` closure. (The `model` var)
    this.transitionToLoadingState();
  }

  /**
   * Sets the initial state, i.e., `unconfigured`.
   * This state only handles `config` messages, and will
   * transition to the `modelless` state once it receives
   * the config data from the host platform.
   */
  private setupConfigState() {
    this.state = {
      name: 'unconfigured',
      handleMessage: (payload) => {
        // ... that message must have been 'config'!
        if (payload.message !== 'config') {
          throw new Error(`invalid message; expected 'config' but got ${payload.message}`);
        }

        this._platformCapabilities = payload.capabilities;

        this.transitionToLoadingState();
      }
    }
  }
  
  /**
   * Sets the model-loading state, i.e., `modelless`.
   * This state only handles `load` messages, and will
   * transition to the `ready` state once it receives a model
   * description and capabilities.
   */
  private transitionToLoadingState() {
    this.state = {
      name: 'modelless',
      handleMessage: (payload) => {
        // ...that message must have been 'load'!
        if (payload.message !== 'load') {
          throw new Error(`invalid message; expected 'load' but got ${payload.message}`);
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
   * @param model The loaded language model.
   */
  private transitionToReadyState(model: LexicalModel) {
    this.state = {
      name: 'ready',
      handleMessage: (payload) => {
        switch(payload.message) {
          case 'predict':
            let {transform, context} = payload;
            let compositor = new ModelCompositor(model); // Yeah, should probably use a persistent one eventually.

            let suggestions = compositor.predict(transform, context);

            // Now that the suggestions are ready, send them out!
            this.cast('suggestions', {
              token: payload.token,
              suggestions: suggestions
            });
            break;
          case 'wordbreak':
            let brokenWord = model.wordbreak(payload.context);

            this.cast('currentword', {
              token: payload.token,
              word: brokenWord
            });
            break;
          case 'unload':
            this.unloadModel();
            break;
          default:
          throw new Error(`invalid message; expected one of {'predict', 'unload'} but got ${payload.message}`);
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
    scope['wordBreakers'] = wordBreakers;

    return worker;
  }
}

// Let LMLayerWorker be available both in the browser and in Node.
if (typeof module !== 'undefined' && typeof module.exports !== 'undefined') {
  module.exports = LMLayerWorker;
  module.exports['models'] = models;
  module.exports['wordBreakers'] = wordBreakers;
  /// XXX: export the ModelCompositor for testing.
  module.exports['ModelCompositor'] = ModelCompositor;
} else if (typeof self !== 'undefined' && 'postMessage' in self) {
  // Automatically install if we're in a Web Worker.
  LMLayerWorker.install(self as any); // really, 'as typeof globalThis', but we're currently getting TS errors from use of that.
} else {
  //@ts-ignore
  window.LMLayerWorker = LMLayerWorker;
}
