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
/// <reference path="lmlayer-interface.ts" />
/// <reference path="virtualized-workerFactory.ts" />

namespace com.keyman.text.prediction {
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

  export class LMLayer extends com.keyman.text.prediction.LMLayerBase {
    /**
     * Construct the top-level LMLayer interface. This also starts the underlying Worker.
     * 
     * @param uri URI of the underlying LMLayer worker code. This will usually be a blob:
     *            or file: URI. If uri is not provided, this will start the default Worker.
     */
    constructor(capabilities: Capabilities) {
      super(capabilities, new VirtualizedWorkerFactory());
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
    //@ts-ignore
    ns.LMLayer.LMLayerBase = ns.LMLayerBase;
  } else {
    //@ts-ignore
    window.LMLayer = ns.LMLayer;
  }
}());