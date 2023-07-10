import ManagedPromise from "./managedPromise.js";

/**
 * This class represents a cancelable timeout, wrapped in Promise form.
 *
 * It will resolve to `true` when the timer completes unless `resolve` or
 * `reject` is called earlier.  Call `.resolve(false)` for early cancellation
 * or `.resolve(true)` to cancel the timer while resolving the Promise early.
 */
export default class TimeoutPromise extends ManagedPromise<Boolean> {
  private timerHandle: number | NodeJS.Timeout;
  constructor(timeoutInMillis: number) {
    // Helps marshal the internal timer handle to its member field despite being
    // initialized in a closure passed to `super`, which cannot access `this`.
    const timerHandleCapture = {
      val: null as number | NodeJS.Timeout
    };

    super((resolve) => {
      const timerId = setTimeout(() => {
        if(!this.isResolved) {
          resolve(true)
        }
      }, timeoutInMillis);

      // Forwards the timer handle outside of the closure.
      timerHandleCapture.val = timerId;
    });

    // "Lands" the timer handle in its final destination.
    this.timerHandle = timerHandleCapture.val;

    const resolve = this._resolve;
    this._resolve = (val) => {
      // b/c of the mismatch between the return types of DOM's window.setTimeout & Node's version
      clearTimeout(this.timerHandle as any);
      resolve(val);
    }

    // Not a standard use-case; it's just here to ensure that the timeout resource is cleaned up
    // even if `reject` gets used for whatever reason.
    /* c8 ignore next 6 */
    const reject = this._reject;
    this._reject = (val) => {
      // b/c of the mismatch between the return types of DOM's window.setTimeout & Node's version
      clearTimeout(this.timerHandle as any);
      reject(val);
    }
  }
}