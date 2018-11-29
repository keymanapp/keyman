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
 * A JavaScript string with the restriction that it must only	
 * contain Unicode scalar values.	
 *	
 * This means that any lone high surrogate must be paired with	
 * a low surrogate, if it exists. Lone surrogate code units are	
 * forbidden.	
 *	
 * See also: https://developer.mozilla.org/en-US/docs/Web/API/USVString	
 */	
type USVString = string;

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
 *  - #initialize() -- initialize the LMLayer with a configuration and language model
 *  - #predict() -- ask the LMLayer to offer suggestions (predictions or corrections) for
 *                  the input event
 * 
 * The top-level LMLayer will automatically starts up its own Web Worker.
 */
class LMLayer {
  /**
   * The underlying worker instance. By default, this is the LMLayerWorker. 
   */
  private _worker: Worker;

  /**
   * Construct the top-level LMLayer interface. This also starts the underlying Worker.
   * Make sure to call .initialize() when using the default Worker.
   * 
   * @param uri URI of the underlying LMLayer worker code. This will usually be a blob:
   *            or file: URI. If uri is not provided, this will start the default Worker.
   */
  constructor(uri?: string) {
    this._worker = new Worker(uri || LMLayer.asBlobURI(LMLayerWorkerCode));
  }

  // TODO: asynchronous initialize() method, based on 
  //       https://github.com/eddieantonio/keyman-lmlayer-prototype/blob/f8e6268b03190d08cf5d35f9428cf9150d6d219e/index.ts#L42-L62

  // TODO: asynchronous predict() method, based on 
  //       https://github.com/eddieantonio/keyman-lmlayer-prototype/blob/f8e6268b03190d08cf5d35f9428cf9150d6d219e/index.ts#L64-L80

  // TODO: asynchronous close() method.
  //       Worker code must recognize message and call self.close().

  /**
   * Given a function, this utility returns the source code within it, as a string.
   * This is intended to unwrap the "wrapped" source code created in the LMLayerWorker
   * build process.
   *
   * @param fn The function whose body will be returned.
   */
  static unwrap(fn: Function): string {
      let wrapper = fn.toString();
      let match = wrapper.match(/function[^{]+{((?:.|\n)+)}[^}]*$/);
      return match[1];
  }

  /**
   * Converts the INSIDE of a function into a blob URI that can
   * be passed as a valid URI for a Worker.
   * @param fn Function whose body will be referenced by a URI.
   * 
   * This function makes the following possible:
   * 
   *    let worker = new Worker(LMLayer.asBlobURI(function myWorkerCode () {
   *      postMessage('inside Web Worker')
   *      function onmessage(event) {
   *        // handle message inside Web Worker.
   *      }
   *    }));
   */
  static asBlobURI(fn: Function): string {
    let code = LMLayer.unwrap(fn);
    let blob = new Blob([code], { type: 'text/javascript' });
    return URL.createObjectURL(blob);
  }
}

// Let LMLayer be available both in the browser and in Node.
if (typeof module !== 'undefined' && typeof module.exports !== 'undefined') {
  module.exports = LMLayer;
} else {
  //@ts-ignore
  window.LMLayer = LMLayer;
}
