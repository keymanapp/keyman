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

/// <reference path="node_modules/es6-shim/es6-shim.min.js" />
/// <reference path="promise-store.ts" />

/**
 * Top-level interface to the Language Modelling layer, or "LMLayer" for short.
 * 
 * The Language Modelling layer provides a way for keyboards to offer prediction and
 * correction functionalities. The LMLayer proper runs within a Web Worker, however,
 * this class is intended to run in the main thread, and automatically spawn a Web
 * Worker, capable of offering predictions.
 * 
 * Since the Worker runs in a different thread, the public methods of this class are
 * asynchronous. Methods of note include:
 * 
 *  - #loadModel() -- loads a specified model file
 *  - #predict() -- ask the LMLayer to offer suggestions (predictions or corrections) for
 *                  the input event
 *  - #unloadModel() -- unloads the LMLayer's currently loaded model, preparing it to
 *                          receive (load) a new model
 * 
 * The top-level LMLayer will automatically starts up its own Web Worker.
 */

namespace com.keyman.text.prediction {
  export class LMLayer {
    /**
     * The underlying worker instance. By default, this is the LMLayerWorker. 
     */
    private _worker: Worker;
    /** Call this when the LMLayer has sent us the 'ready' message! */
    private _declareLMLayerReady: (conf: Configuration) => void;
    private _predictPromises: PromiseStore<Suggestion[]>;
    private _wordbreakPromises: PromiseStore<USVString>;
    private _nextToken: number;
    private capabilities: Capabilities;

    /**
     * Construct the top-level LMLayer interface. This also starts the underlying Worker.
     * 
     * @param uri URI of the underlying LMLayer worker code. This will usually be a blob:
     *            or file: URI. If uri is not provided, this will start the default Worker.
     */
    constructor(capabilities: Capabilities, worker?: Worker) {
      // Either use the given worker, or instantiate the default worker.
      this._worker = worker || DefaultWorker.constructInstance();
      this._worker.onmessage = this.onMessage.bind(this)
      this._declareLMLayerReady = null;
      this._predictPromises = new PromiseStore;
      this._wordbreakPromises = new PromiseStore<USVString>();
      this._nextToken = Number.MIN_SAFE_INTEGER;

      this.sendConfig(capabilities);
    }

    /**
     * Initializes the LMLayer worker with the host platform's capability set.
     * 
     * @param capabilities The host platform's capability spec - a model cannot assume access to more context
     *                     than specified by this parameter.
     */
    private sendConfig(capabilities: Capabilities) {
      this._worker.postMessage({
        message: 'config',
        capabilities: capabilities
      });
    }

    /**
     * Initializes the LMLayer worker with a path to the desired model file.
     */
    loadModel(modelSource: string, loadType: 'file' | 'raw' = 'file'): Promise<Configuration> {
      return new Promise((resolve, _reject) => {
        // Sets up so the promise is resolved in the onMessage() callback, when it receives
        // the 'ready' message.
        this._declareLMLayerReady = resolve;

        let modelSourceSpec: any = {
          type: loadType
        };

        if(loadType == 'file') {
          modelSourceSpec.file = modelSource;
        } else {
          modelSourceSpec.code = modelSource;
        }

        this._worker.postMessage({
          message: 'load',
          source: modelSourceSpec
        });
      });
    }

    /**
     * Unloads the previously-active model from memory, resetting the LMLayer to prep
     * for transition to use of a new model.
     */
    public unloadModel() {
      this._worker.postMessage({
        message: 'unload'
      });
    }

    predict(transform: Transform | Distribution<Transform>, context: Context): Promise<Suggestion[]> {
      let token = this._nextToken++;
      return new Promise((resolve, reject) => {
        this._predictPromises.make(token, resolve, reject);
        this._worker.postMessage({
          message: 'predict',
          token: token,
          transform: transform,
          context: context,
        });
      });
    }

    wordbreak(context: Context): Promise<USVString> {
      let token = this._nextToken++;
      return new Promise((resolve, reject) => {
        this._wordbreakPromises.make(token, resolve, reject);
        this._worker.postMessage({
          message: 'wordbreak',
          token: token,
          context: context
        })
      });
    }

    // TODO: asynchronous close() method.
    //       Worker code must recognize message and call self.close().

    private onMessage(event: MessageEvent): void {
      let payload: OutgoingMessage = event.data;
      if (payload.message === 'error') {
        console.error(payload.log);
        if(payload.error) {
          console.error(payload.error);
        }
      }
      else if (payload.message === 'ready') {
        this._declareLMLayerReady(event.data.configuration);
      } else if (payload.message === 'suggestions') {
        this._predictPromises.keep(payload.token, payload.suggestions);
      } else if (payload.message === 'currentword') {
        this._wordbreakPromises.keep(payload.token, payload.word);
      } else {
        // This branch should never execute, but just in case...
        //@ts-ignore
        throw new Error(`Message not implemented: ${payload.message}`);
      }
    }

    /**
     * Clears out any computational resources in use by the LMLayer, including shutting
     * down any internal WebWorkers.
     */
    public shutdown() {
      this._worker.terminate();
    }

    /**
     * Given a function, this utility returns the source code within it, as a string.
     * This is intended to unwrap the "wrapped" source code created in the LMLayerWorker
     * build process.
     *
     * @param fn The function whose body will be returned.
     */
    static unwrap(fn: Function): string {
      let wrapper = fn.toString();
      let match = wrapper.match(/function[^{]+{((?:.|\r|\n)+)}[^}]*$/);
      return match[1];
    }
  }
}

(function () {
  let ns = com.keyman.text.prediction;

  // Let LMLayer be available both in the browser and in Node.
  if (typeof module !== 'undefined' && typeof module.exports !== 'undefined') {
    module.exports = ns.LMLayer;
    //@ts-ignore
    ns.LMLayer.PromiseStore = ns.PromiseStore;
  } else {
    //@ts-ignore
    window.LMLayer = ns.LMLayer;
  }
}());