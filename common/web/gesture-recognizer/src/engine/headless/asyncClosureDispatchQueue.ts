import { timedPromise } from "@keymanapp/web-utils";
import { reportError } from "../reportError.js";

export type QueueClosure = () => (Promise<any> | void);

/**
  This class is modeled somewhat after Swift's `DispatchQueue` class, but with
  the twist that each closure may return a `Promise` (in Swift: a `Future`) to
  lock out further closure processing until the `Promise` resolves.
*/
export class AsyncClosureDispatchQueue {
  private queue: QueueClosure[];
  private waitLock: Promise<any>;
  private defaultWaitFactory: () => Promise<any>;

  /**
   *
   * @param defaultWaitFactory A factory returning Promises to use for default
   * delays between tasks.  If not specified, Promises corresponding to
   * setTimeout(0) will be used, allowing the microqueue task to flush between
   * tasks.
   */
  constructor(defaultWaitFactory?: () => Promise<any>) {
    // We only need to trigger events if the queue has no prior entries and there isn't an
    // active wait-lock; for the latter, we'll auto-trigger the next function when it unlocks.
    this.queue = [];

    this.defaultWaitFactory = defaultWaitFactory || (() => { return timedPromise(0) });
  }

  get defaultWait() {
    return this.defaultWaitFactory();
  }

  get ready() {
    return this.queue.length == 0 && !this.waitLock;
  }

  private async triggerNextClosure() {
    if(this.queue.length == 0) {
      return;
    }

    const functor = this.queue.shift();

    // A stand-in so that `ready` doesn't report true while the closure runs.
    this.waitLock = Promise.resolve();

    /*
      It is imperative that any errors triggered by the functor do not prevent this method from setting
      the wait lock that will trigger the following event (if it exists).  Failure to do so will
      result in all further queued closures never getting the opportunity to run!
    */
    let result: undefined | Promise<any>;
    try {
      // Is either undefined (return type: void) or is a Promise.
      result = functor() as undefined | Promise<any>;
      /* c8 ignore start */
    } catch (err) {
      reportError('Error from queued closure', err);
    }
    /* c8 ignore end */

    /*
      Replace the stand-in with the _true_ post-closure wait.

      If the closure returns a Promise, the implication is that the further processing of queued
      functions should be blocked until that Promise is fulfilled.

      If not, we just add a default delay.
    */
    result = result ?? this.defaultWaitFactory();
    this.waitLock = result;

    try {
      await result;
    } catch(err) {
      reportError('Async error from queued closure', err);
    }

    this.waitLock = null;
    // if queue is length zero, auto-returns.
    this.triggerNextClosure();
  }

  runAsync(closure: QueueClosure) {
    // Check before putting the closure on the internal queue; the check is based in part
    // upon the existing queue length.
    const isReady = this.ready;

    this.queue.push(closure);

    // If `!isReady`, the next closure will automatically be triggered when possible.
    if(isReady) {
      this.triggerNextClosure();
    }
  }
}