import { timedPromise } from "@keymanapp/web-utils";
import { reportError } from "./reportError.js";

type QueueClosure = () => (Promise<any> | void);

/**
  This class is modeled somewhat after Swift's `DispatchQueue` class, but with
  the twist that each closure may return a `Promise` (in Swift: a `Future`) to
  lock out further closure processing until the `Promise` resolves.
*/
export class AsyncClosureDispatchQueue {
  private queue: QueueClosure[];
  private waitLock: Promise<any>;

  constructor() {
    this.queue = [];
  }

  private async setWaitLock(promise: Promise<any>) {
    this.waitLock = promise;

    try {
      await promise;
    } catch(err) {
      reportError('Async error from queued closure', err);
    }

    this.waitLock = null;
    this.triggerNextEvent();
  }

  private async triggerNextEvent() {
    if(this.queue.length > 0) {
      const functor = this.queue.shift();

      /*
        It is imperative that any errors triggered by the functor do not prevent this method from setting
        the wait lock that will trigger the following event (if it exists).  Failure to do so will
        result in all further queued closures never getting the opportunity to run!
      */
     let result: undefined | Promise<any>;
      try {
        // Is either undefined (return type: void) or is a Promise.
        result = functor() as undefined | Promise<any>;
      } catch (err) {
        reportError('Error from queued closure', err);
      }

      /*
        If the closure returns a Promise, the implication is that the further processing of queued
        functions should be blocked until that Promise is fulfilled.

        If not, we still delay until the microtask queue is complete as our default.
      */
      this.setWaitLock(result ?? timedPromise(0));
    }
  }

  runAsync(closure: QueueClosure) {
    this.queue.push(closure);

    // We only need to trigger events if the queue has no prior entries and there isn't an
    // active wait-lock; for the latter, we'll auto-trigger the next function when it unlocks.
    if(this.queue.length == 1 && !this.waitLock) {
      this.triggerNextEvent();
    }
  }
}