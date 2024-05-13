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

/// <reference types="@keymanapp/lm-message-types" />
import { extendString } from "@keymanapp/web-utils";

extendString();

import * as models from './models/index.js';
import * as correction from './correction/index.js';
import * as wordBreakers from '@keymanapp/models-wordbreakers';

import ModelCompositor from './model-compositor.js';
import { ImportScripts, IncomingMessage, LMLayerWorkerState, LoadMessage, ModelEval, ModelFile, ModelSourceSpec, PostMessage } from './worker-interfaces.js';

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
export default class LMLayerWorker {
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

  // @ts-ignore // We don't reference it directly here, but it's worth noting
  // during debug sessions, especially for unit tests.
  private self: any;

  private _platformCapabilities: Capabilities;

  private _testMode: boolean = false;

  private _currentModelSource: ModelSourceSpec;

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
      let duplicated = false;
      if(this._currentModelSource && data.source.type == this._currentModelSource.type) {
        if(data.source.type == 'file' && data.source.file == (this._currentModelSource as ModelFile).file) {
          duplicated = true;
        } else if(data.source.type == 'raw' && data.source.code == (this._currentModelSource as ModelEval).code) {
          duplicated = true;
        }
      }

      if(duplicated) {
        // Some JS implementations don't allow web workers access to the console.
        if(typeof console !== 'undefined') {
          console.warn("Duplicate model load message detected - squashing!");
        }
        return;
      } else {
        this._currentModelSource = data.source;
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

      // Ensures that default casing rules exist for custom models that request casing rules but don't define them.
      if(model.languageUsesCasing && !model.applyCasing) {
        model.applyCasing = models.defaultApplyCasing;
      }

      let compositor = this.transitionToReadyState(model);
      // This test allows models to directly specify the property without it being auto-overridden by
      // this default.
      if(configuration.wordbreaksAfterSuggestions === undefined) {
        configuration.wordbreaksAfterSuggestions = (compositor.punctuation.insertAfterWord != '');
      }
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
        this._testMode = !!payload.testMode;

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
    let _this = this;
    this.state = {
      name: 'modelless',
      handleMessage: (payload) => {
        // ...that message must have been 'load'!
        if (payload.message !== 'load') {
          throw new Error(`invalid message; expected 'load' but got ${payload.message}`);
        }

        // TODO: validate configuration?
        if(payload.source.type == 'file') {
          _this.loadModelFile(payload.source.file);
        } else {
          let code = payload.source.code;

          // Limits the scope accessible by the code we're about to evaluate; the code may only access
          // global scope and the arguments specified in the constructor below.
          //
          // This is far more encapsulated and likely more secure... and the former point means this is
          // easier to bundle and more optimizable when bundling than direct eval.
          // Reference: https://esbuild.github.io/link/direct-eval
          const modelLoader = new Function('LMLayerWorker', 'models', 'correction', 'wordBreakers', code);
          modelLoader(_this, models, correction, wordBreakers);
        }
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
  private transitionToReadyState(model: LexicalModel): ModelCompositor {
    let compositor = new ModelCompositor(model, this._testMode);
    this.state = {
      name: 'ready',
      handleMessage: (payload) => {
        switch(payload.message) {
          case 'predict':
            var {transform, context} = payload;
            var suggestions = compositor.predict(transform, context);

            // Now that the suggestions are ready, send them out!
            this.cast('suggestions', {
              token: payload.token,
              suggestions: suggestions
            });
            break;
          case 'wordbreak':
            let brokenWord = models.wordbreak(model.wordbreaker || wordBreakers.default, payload.context);

            this.cast('currentword', {
              token: payload.token,
              word: brokenWord
            });
            break;
          case 'unload':
            this.unloadModel();
            break;
          case 'accept':
            var {suggestion, context, postTransform} = payload;
            var reversion = compositor.acceptSuggestion(suggestion, context, postTransform);

            this.cast('postaccept', {
              token: payload.token,
              reversion: reversion
            });
            break;
          case 'revert':
            var {reversion, context} = payload;
            var suggestions: Suggestion[] = compositor.applyReversion(reversion, context);

            this.cast('postrevert', {
              token: payload.token,
              suggestions: suggestions
            });
            break;
          case 'reset-context':
            var {context} = payload;
            compositor.resetContext(context);
            break;
          default:
            throw new Error(`invalid message; expected one of {'predict', 'wordbreak', 'accept', 'revert', 'reset-context', 'unload'} but got ${payload.message}`);
        }
      },
      compositor: compositor
    };

    return compositor;
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
    worker.self = scope;

    // Ensures that the worker instance is accessible for loaded model scripts.
    // Assists unit-testing.
    scope['LMLayerWorker'] = worker;
    scope['models'] = models;
    scope['correction'] = correction;
    scope['wordBreakers'] = wordBreakers;

    return worker;
  }
}