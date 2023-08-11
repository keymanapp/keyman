import { assert } from 'chai'
import { timedPromise } from '@keymanapp/web-utils';

import { QueuedPromisePrioritizer } from "@keymanapp/gesture-recognizer";

describe('QueuedPromisePrioritizer', () => {
  it('single call', async () => {
    const prioritizer = new QueuedPromisePrioritizer();
    await prioritizer.queueWithPriority(1);
  });

  it('three sync', async () => {
    const prioritizer = new QueuedPromisePrioritizer();

    const simpleLog: number[] = [];

    const waiter1 = prioritizer.queueWithPriority(1).then(() => simpleLog.push(1));
    const waiter2 = prioritizer.queueWithPriority(2).then(() => simpleLog.push(2));
    const waiter3 = prioritizer.queueWithPriority(3).then(() => simpleLog.push(3));

    await Promise.all([waiter1, waiter2, waiter3]);

    assert.sameOrderedMembers(simpleLog, [3, 2, 1]);
  });

  // Because JS's default Array.sort isn't a stable-sort.
  it('three equal-priority sync', async () => {
    const prioritizer = new QueuedPromisePrioritizer();

    const simpleLog: number[] = [];

    const waiter1 = prioritizer.queueWithPriority(1).then(() => simpleLog.push(1));
    const waiter2 = prioritizer.queueWithPriority(1).then(() => simpleLog.push(2));
    const waiter3 = prioritizer.queueWithPriority(1).then(() => simpleLog.push(3));

    await Promise.all([waiter1, waiter2, waiter3]);

    assert.sameOrderedMembers(simpleLog, [1, 2, 3]);
  });

  it('three sync, time wait, two sync', async () => {
    const prioritizer = new QueuedPromisePrioritizer();

    const simpleLog: number[] = [];

    const waiter1 = prioritizer.queueWithPriority(1).then(() => simpleLog.push(1));
    const waiter2 = prioritizer.queueWithPriority(-2).then(() => simpleLog.push(-2));
    const waiter3 = prioritizer.queueWithPriority(3).then(() => simpleLog.push(3));

    await timedPromise(10);

    const waiter4 = prioritizer.queueWithPriority(15).then(() => simpleLog.push(15));
    const waiter5 = prioritizer.queueWithPriority(0).then(() => simpleLog.push(0));

    await Promise.all([waiter1, waiter2, waiter3, waiter4, waiter5]);

    assert.sameOrderedMembers(simpleLog, [3, 1, -2, 15, 0]);
  });

  // The most brutal test / niche edge case.
  it('three sync, await-and-call, two sync', async () => {
    const prioritizer = new QueuedPromisePrioritizer();

    const simpleLog: number[] = [];

    const waiter1 = prioritizer.queueWithPriority(1).then(() => simpleLog.push(1));
    const waiter2 = prioritizer.queueWithPriority(-2).then(() => simpleLog.push(-2));
    const waiter3 = prioritizer.queueWithPriority(3).then(() => simpleLog.push(3));

    // Is within its own set; we do have to await full resolution of the Promise.resolve() call, which takes its
    // own slot on the queue.  As the queue takes control at this stage, further registrations (from the `then`)
    // will be treated as asynchronous with the prior set.
    //
    // We must _then_ await its complete fulfillment - our await is actually on the `then` clause.
    const waiter4 = await Promise.resolve().then(() => prioritizer.queueWithPriority(4).then(() => simpleLog.push(4)));

    // As we `awaited`, we get control back from the queue asynchronously relative to the priority-4 call's
    // registration.
    const waiter5 = prioritizer.queueWithPriority(15).then(() => simpleLog.push(15));
    const waiter6 = prioritizer.queueWithPriority(0).then(() => simpleLog.push(0));

    await Promise.all([waiter1, waiter2, waiter3, waiter4, waiter5, waiter6]);

    assert.sameOrderedMembers(simpleLog, [3, 1, -2, 4, 15, 0]);
  });
});