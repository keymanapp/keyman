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

// Include the code intended to run WITHIN the Web Worker.
// This MUST be compiled before this file is compiled.
/// <reference path="embedded_worker.js" />
declare var LMLayerWorkerCode: Function;

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

// TODO: document
class LMLayer {
  /**
   * The underlying worker instance. By default, this is the LMLayerWorker. 
   */
  private _worker: Worker;

  constructor(uri?: string) {
    this._worker = new Worker(uri || LMLayer.asBlobURI(LMLayerWorkerCode));
  }

  /**
   * Given a function, this utility returns the source code within it.
   * @param fn 
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

// Let LMLayer be available both in browser and in Node.
if (typeof module !== 'undefined' && typeof module.exports !== 'undefined') {
  module.exports = LMLayer;
} else {
  //@ts-ignore
  window.LMLayer = LMLayer;
}
