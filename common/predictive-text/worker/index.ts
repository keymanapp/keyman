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

 /**
  * Encapsulates all the state required for the LMLayer's worker thread.
  */
class LMLayerWorker {
  /**
   * All of the bundled model implementations will add themselves here:
   * Note: the models will add themselves by including this
   * file using a triple-slash directive:
   * /// <reference path="path/to/this/file.ts" />
   * then adding the constructor to this static object:
   * LMLayerWorker.models.MyModelImplementation = class {}
   */
  static models: {[key: string]: WorkerInternalModelConstructor} = {};

  private model?: WorkerInternalModel;

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

    // TODO: state pattern
    // TODO: update worker-communication-protocol document.
    if (message === 'predict' && this.model) {
      let {transform, context} = event.data;
      this.cast('suggestions', {
        suggestions: this.model.predict(transform, context)
      })
      return;
    }

    // ...that message must have been 'initialize'!
    if (message !== 'initialize') {
      throw new Error(`invalid message; expected 'initialize' but got ${message}`);
    }

    let payload: InitializeMessage = event.data;

    let {model, configuration} = this.loadModel(
      // TODO: validate configuration, and provide valid configuration in tests.
      payload.model, payload.capabilities
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
   * @param modelCode Source code for a function that takes
   *                  configuration, and returns { model, configuration }
   * @param capabilities Capabilities on offer from the keyboard.
   */
  private loadModel(modelCode: any, capabilities: Capabilities) {
    let model = null;
    let configuration: Configuration = {
      leftContextCodeUnits: 0,
      rightContextCodeUnits: 0
    };

    if (typeof modelCode === 'string') {
      console.warn("Deprecated: model defined as a string.")
      // Deprecated! The model should not be source code.
      let result = new Function('configuration', modelCode)(capabilities);
      model = result.model;
      configuration = result.configuration;
    } else if (modelCode.type === 'dummy') {
      this.model = new LMLayerWorker.models.DummyModel(capabilities, {
        futureSuggestions: modelCode.futureSuggestions
      });
    } else {
      throw new Error('Invalid model');
    }
    // TODO: when model is object with kind 'wordlist' or 'fst'

    // Set reasonable defaults for the configuration.
    if (!configuration.leftContextCodeUnits) {
      configuration.leftContextCodeUnits = capabilities.maxLeftContextCodeUnits;
    }
    if (!configuration.rightContextCodeUnits) {
      configuration.rightContextCodeUnits = capabilities.maxRightContextCodeUnits || 0;
    }

    return {model, configuration};
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
    let worker = new LMLayerWorker({ postMessage: scope.postMessage });
    scope.onmessage = worker.onMessage.bind(worker);

    return worker;
  }
}

// Let LMLayerWorker be available both in the browser and in Node.
if (typeof module !== 'undefined' && typeof module.exports !== 'undefined') {
  module.exports = LMLayerWorker;
} else if (typeof self !== 'undefined' && 'postMessage' in self) {
  // Automatically install if we're in a Web Worker.
  LMLayerWorker.install(self as DedicatedWorkerGlobalScope);
} else {
  //@ts-ignore
  window.LMLayerWorker = LMLayerWorker;
}
