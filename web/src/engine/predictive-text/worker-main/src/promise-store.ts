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

/// <reference types="@keymanapp/lm-message-types" />

import { Token } from '@keymanapp/lm-message-types';

type Resolve<T> = (value?: T | PromiseLike<T>) => void;
type Reject = (reason?: any) => void;
interface PromiseCallbacks<T> {
  resolve: Resolve<T>;
  reject: Reject;
}


/**
 * Associate tokens with promises.
 *
 * First, .make() a promise -- associate a token with resolve/reject callbacks.
 *
 * You can either .keep() a promise -- resolve() and forget it;
 * Or you may also .break() a promise -- reject() and forget it.
 *
 * <T> is the type of resolved value (value yielded successfully by promise).
 */
export default class PromiseStore<T> {
  // IE11 offers partial support for new Map().
  // Assume only .get(), .set(), .has(), .delete(), and .size work.
  // See: http://kangax.github.io/compat-table/es6/#test-Map
  private _promises: Map<Token, PromiseCallbacks<T>>;
  constructor() {
    this._promises = new Map();
  }
  /**
   * How many promises are currently being tracked?
   */
  get length(): number {
    return this._promises.size;
  }
  /**
   * Associate a token with its respective resolve and reject callbacks.
   */
  make(token: Token, resolve: Resolve<T>, reject: Reject): void {
    if (this._promises.has(token)) {
      return reject(`Existing request with token ${token}`);
    }
    this._promises.set(token, { reject, resolve });
  }
  /**
   * Resolve the promise associated with a token (with a value!).
   * Once the promise is resolved, the token is removed..
   */
  keep(token: Token, value: T) {
    let callbacks = this._promises.get(token);
    if (!callbacks) {
      throw new Error(`No promise associated with token: ${token}`);
    }
    let accept = callbacks.resolve;
    this._promises.delete(token);
    return accept(value);
  }
  /**
   * Instantly reject and forget a promise associated with the token.
   */
  break(token: Token, reason?: any): void {
    let callbacks = this._promises.get(token);
    if (!callbacks) {
      throw new Error(`No promise associated with token: ${token}`);
    }
    this._promises.delete(token);
    callbacks.reject(reason);
  }
}