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

// TODO: document
type WorkerFactory = (uri: string) => Worker;

// TODO: document
class LMLayer {
  // TODO: document
  private _worker: Worker;

  // TODO: document
  constructor(workerFactory: WorkerFactory = (uri) => new Worker(uri)) {
    let blob = new Blob([], { type: 'text/javascript' });
    let uri = URL.createObjectURL(blob);
    this._worker = workerFactory(uri);
  }
}

// Let LMLayerWorker be available both in browser and in Node.
if (typeof module !== 'undefined' && typeof module.exports !== 'undefined') {
  module.exports = LMLayer;
} else {
  //@ts-ignore
  window.LMLayer = LMLayer;
}
