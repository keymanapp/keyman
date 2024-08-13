import * as PromiseStatusModule from 'promise-status-async';

/**
 * A custom variant of the promise-status function; this one asserts that rejections should
 * be reported as test errors.  If a rejection occurs, it will ensure that it is reported
 * on the main thread / error output.
 * @param promise
 * @returns
 */
export async function assertingPromiseStatus(promise: Promise<any>) {
  const status = await PromiseStatusModule.promiseStatus(promise);
  if(status == PromiseStatusModule.PROMISE_REJECTED) {
    // Synchronize on the spot - this will cause the calling test to throw and report
    // the reported rejection cause / error.  This is far more informative in error logs
    // than "oh, the promise was rejected" - with no indication as to _why_.
    await promise;
  }
  return status;
}