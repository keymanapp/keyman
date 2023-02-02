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

import LMLayerWorker from './index.js';

// Let LMLayerWorker be available both in the browser and in Node.
if (typeof self !== 'undefined' && 'postMessage' in self && 'importScripts' in self) {
  // Automatically install if we're in a Web Worker.
  LMLayerWorker.install(self as any); // really, 'as typeof globalThis', but we're currently getting TS errors from use of that.
} else {
  //@ts-ignore
  window.LMLayerWorker = LMLayerWorker;
}
