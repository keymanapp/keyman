import { assert } from 'chai'
import { default as sinon, type SinonSpy } from 'sinon';
import { AsyncClosureDispatchQueue, type QueueClosure } from '@keymanapp/gesture-recognizer';
import { ManagedPromise, timedPromise } from '@keymanapp/web-utils';

type ClosureSpy = SinonSpy<[], ReturnType<QueueClosure>>;

describe('AsyncClosureDispatchQueue', () => {
  it('has expected initial state', () => {
    const queue = new AsyncClosureDispatchQueue();
    assert.isTrue(queue.ready);
  });

  it('proper handling of simple closure when queue is empty (default config)', async () => {
    const queue = new AsyncClosureDispatchQueue();

    const fake = sinon.fake();
    queue.runAsync(() => {
      fake(queue.ready);
    });

    assert.isFalse(queue.ready);
    // If the queue was ready, this should be called immediately.
    assert.isTrue(fake.called);

    // Default delay between entries:  a macroqueue task (i.e., setTimeout(0))
    await timedPromise(0);

    assert.isTrue(queue.ready);
    assert.isTrue(fake.called);

    // During the actual closure call, the queue is still awaiting async completion of the closure.
    // Default wait:  a macroqueue task
    assert.isFalse(fake.firstCall.args[0]);
  });

  it('proper handling of Promise-returning closure when queue is empty (default config)', async () => {
    const queue = new AsyncClosureDispatchQueue();

    const lock = new ManagedPromise<void>();
    const fake = sinon.fake();
    queue.runAsync(() => {
      fake(queue.ready);
      return lock.corePromise;
    });

    assert.isFalse(queue.ready);
    // If the queue was ready, this should be called immediately.
    assert.isTrue(fake.called);
    assert.isFalse(fake.firstCall.args[0]);

    await timedPromise(50);

    assert.isFalse(queue.ready);

    lock.resolve();

    // Allow the newly-resolved Promise to chain.
    // ("White-box" info here, but once is enough.)
    await Promise.resolve();

    assert.isTrue(queue.ready);
    assert.isTrue(fake.called);
  });

  it('proper handling of simple closure when queue is not empty (default config)', async () => {
    const queue = new AsyncClosureDispatchQueue();

    const fakeTimers = sinon.useFakeTimers();

    const lock = new ManagedPromise<void>();
    queue.runAsync(() => lock.corePromise);

    const fake = sinon.fake();
    queue.runAsync(() => {
      fake(queue.ready);
    });

    try {
      assert.isFalse(queue.ready);
      assert.isFalse(fake.called);

      // Doesn't matter how long we wait; there's still a pending entry in front of `fake`.
      await fakeTimers.tickAsync(50);

      assert.isFalse(queue.ready);
      assert.isFalse(fake.called);

      // Allow that pending entry to resolve; `fake` should be able to resolve afterward with little issue.
      lock.resolve();
      await fakeTimers.tickAsync(50);

      assert.isTrue(queue.ready);
      assert.isTrue(fake.called);

      // During the actual closure call, the queue is still awaiting async completion of the closure.
      // Default wait:  a macroqueue task
      assert.isFalse(fake.firstCall.args[0]);
    } finally {
      fakeTimers.restore();
    }
  });

  it('complex case 1 - many tasks, all queued at the same time', async () => {
    // Uses the default timeout between events; just making it extra-explicit here.
    const queue = new AsyncClosureDispatchQueue(() => { return timedPromise(0) });

    const buildSet = (n: number) => {
      let set: ClosureSpy[] = [];

      // Deliberately using the same one multiple times - the class gives us a call count.
      let closure = sinon.spy(() => {});
      for(let i=0; i < n; i++) {
        set.push(closure);
      }

      return set;
    }

    const set0 = buildSet(3);
    const lock0 = new ManagedPromise<void>();
    const set1 = buildSet(7);
    const lock1 = new ManagedPromise<void>();
    const set2 = buildSet(5);

    const fakeTimers = sinon.useFakeTimers();

    set0.forEach((entry) => queue.runAsync(entry));
    queue.runAsync(() => {
      return lock0.corePromise;
    });
    set1.forEach((entry) => queue.runAsync(entry));
    queue.runAsync(() => {
      return lock1.corePromise;
    })
    set2.forEach((entry) => queue.runAsync(entry));

    assert.isFalse(queue.ready);

    try {
      // Run set0; it'll stop before set1 due to lock0 not being resolved.
      await fakeTimers.tickAsync(50);

      assert.equal(set0[0].callCount, 3);
      assert.equal(set1[0].callCount, 0);
      assert.equal(set2[0].callCount, 0);

      // Now we run set1; it'll stop before set2 due to lock1 not being resolved.
      lock0.resolve();
      await fakeTimers.tickAsync(50);

      assert.equal(set0[0].callCount, 3);
      assert.equal(set1[0].callCount, 7);
      assert.equal(set2[0].callCount, 0);

      // Now we run set2, flushing out the queue.
      lock1.resolve();
      await fakeTimers.tickAsync(50);

      assert.equal(set0[0].callCount, 3);
      assert.equal(set1[0].callCount, 7);
      assert.equal(set2[0].callCount, 5);

      assert.isTrue(queue.ready);
    } finally {
      fakeTimers.restore();
    }
  });

  it('complex case 2 - queued closure promises "unlocking" out of order', async () => {
    // Uses the default timeout between events; just making it extra-explicit here.
    const queue = new AsyncClosureDispatchQueue(() => { return timedPromise(0) });

    const buildSet = (n: number) => {
      let set: ClosureSpy[] = [];

      // Deliberately using the same one multiple times - the class gives us a call count.
      let closure = sinon.spy(() => {});
      for(let i=0; i < n; i++) {
        set.push(closure);
      }

      return set;
    }

    const fakeTimers = sinon.useFakeTimers();

    const set0 = buildSet(3);
    const lock0 = new ManagedPromise<void>();
    const set1 = buildSet(7);
    const lock1 = new ManagedPromise<void>();
    const set2 = buildSet(5);

    set0.forEach((entry) => queue.runAsync(entry));
    queue.runAsync(() => {
      return lock0.corePromise;
    });
    set1.forEach((entry) => queue.runAsync(entry));
    queue.runAsync(() => {
      return lock1.corePromise;
    })
    set2.forEach((entry) => queue.runAsync(entry));

    assert.isFalse(queue.ready);

    try {
      // Run set0; it'll stop before set1 due to lock0 not being resolved.
      await fakeTimers.tickAsync(50);

      assert.equal(set0[0].callCount, 3);
      assert.equal(set1[0].callCount, 0);
      assert.equal(set2[0].callCount, 0);

      // Now we resolve lock1 - but this isn't what is currently blocking the queue.
      // No new tasks should run.
      lock1.resolve(); /* NOTE:  is being unlocked before lock0, which is earlier! */
      await fakeTimers.tickAsync(50);

      assert.equal(set0[0].callCount, 3);
      assert.equal(set1[0].callCount, 0);
      assert.equal(set2[0].callCount, 0);

      // Now we resolve lock0, allowing both set1 and set2 to complete.
      lock0.resolve();
      await fakeTimers.tickAsync(50);

      assert.equal(set0[0].callCount, 3);
      assert.equal(set1[0].callCount, 7);
      assert.equal(set2[0].callCount, 5);

      assert.isTrue(queue.ready);
    } finally {
      fakeTimers.restore();
    }
  });
});