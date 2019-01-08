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

/// <reference path="./index.ts" />

/**
 * @file dummy-model.ts
 * 
 * Defines the Dummy model, which is used for testing the
 * prediction API exclusively.
 */

/**
 * The Dummy Model that returns nonsensical, but predictable results. 
 */
LMLayerWorker.models.DummyModel = class DummyModel implements WorkerInternalModel {
  configuration: Configuration;

  constructor(capabilities: Capabilities, options?: any) {
    options = options || {};
    this.configuration = options.configuration || {};
  }

  predict(transform: Transform, context: Context, injectedSuggestions?: Suggestion[]): Suggestion[] {
    if (injectedSuggestions) {
      return injectedSuggestions;
    }
  }
};
