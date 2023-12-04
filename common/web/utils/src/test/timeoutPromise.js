import { assert } from 'chai';

import { TimeoutPromise, timedPromise } from '@keymanapp/web-utils';

// Set this long enough to allow a bit of delay from OS context-switching (often on the
// order of ~16ms for some OSes) to occur multiple times without breaking these tests.
const INTERVAL = 160;

describe("TimeoutPromise", () => {
  it('standard use', async () => {
    const start = Date.now();
    const promise = new TimeoutPromise(INTERVAL);

    assert.isTrue(await promise.corePromise);

    const end = Date.now();
    // https://github.com/nodejs/node/issues/26578 - setTimeout() may resolve 1ms earlier than requested.
    assert.isAtLeast(end-start, INTERVAL-1);
  });

  it('standard use (simpler format)', async () => {
    const start = Date.now();
    const promise = timedPromise(INTERVAL);

    assert.isTrue(await promise);

    const end = Date.now();
    // https://github.com/nodejs/node/issues/26578 - setTimeout() may resolve 1ms earlier than requested.
    assert.isAtLeast(end-start, INTERVAL-1);
  });

  it('simple early fulfillment', async () => {
    const start = Date.now();
    const promise = new TimeoutPromise(INTERVAL);

    promise.resolve(true);

    assert.isTrue(await promise.corePromise);

    const end = Date.now();
    assert.isAtMost(end-start, INTERVAL-1);  // completes early
  });

  it('early cancellation', async () => {
    const start = Date.now();
    const promise = new TimeoutPromise(INTERVAL);
    const uncancelledPromise = new TimeoutPromise(INTERVAL);

    promise.resolve(false);

    assert.isFalse(await promise.corePromise);

    const end = Date.now();
    assert.isAtMost(end-start, INTERVAL-1);  // completes early

    await uncancelledPromise;

    // The internal timeout function calls `resolve(true)`, but we cancelled.
    // That call is late, post-settle... so it should not change what `then` receives.
    assert.isFalse(await promise.corePromise);
  });

  it('complex early fulfillment', async () => {
    const start = Date.now();
    const promise = new TimeoutPromise(INTERVAL);

    let delayedResolver = new TimeoutPromise(INTERVAL/2);
    delayedResolver.then(() => promise.resolve(true));

    assert.isTrue(await promise.corePromise);

    const end = Date.now();
    assert.isAtMost(end-start, INTERVAL-1);  // completes early
    // https://github.com/nodejs/node/issues/26578 - setTimeout() may resolve 1ms earlier than requested.
    assert.isAtLeast(end-start, INTERVAL/2-1); // but not TOO early
  });

  it('late dual fulfillment does not change first result', async () => {
    const start = Date.now();
    const promise = new TimeoutPromise(INTERVAL);

    assert.isTrue(await promise.corePromise);

    const end = Date.now();
    // https://github.com/nodejs/node/issues/26578 - setTimeout() may resolve 1ms earlier than requested.
    assert.isAtLeast(end-start, INTERVAL-1);

    promise.resolve(false);

    assert.isTrue(await promise);
  });
});