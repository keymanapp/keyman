import { assert } from 'chai';

import { ManagedPromise } from '@keymanapp/web-utils';

describe("ManagedPromise", () => {
  it('<void> resolution', async () => {
    let promise = new ManagedPromise();

    promise.resolve();

    await promise.corePromise;
  });

  it('<string> resolution', async () => {
    let promise = new ManagedPromise();

    promise.resolve("foobar");

    let text = await promise.corePromise;

    assert.equal(text, "foobar");
  });

  it('reject:  await -> try-catch', async () => {
    let promise = new ManagedPromise();

    promise.reject(new Error("foobar"));

    try {
      await promise.corePromise;
      assert.fail();
    } catch (err) {
      assert.equal(err.message, "foobar");
    }
  });

  it('then()', async () => {
    let promise = new ManagedPromise();

    let thenPromise = promise.then((val) => {
      return {text: val + "bar"}
    });

    promise.resolve("foo");
    let val = await thenPromise;

    assert.deepEqual(val, {text: "foobar"});
  });

  it('reject:  then() -> catch() => finally()', async () => {
    let promise = new ManagedPromise();

    promise.reject(new Error("foobar"));

    let caught = false;
    let final = false;

    await promise.then(
      () => assert.fail()
    ).catch((err) => {
      assert.equal(err.message, "foobar");
      caught = true;
    }).finally(() => {
      final = true;
    });

    assert.isTrue(final);
  });

  it('reject:  direct catch()', async () => {
    let promise = new ManagedPromise();

    promise.reject(new Error("foobar"));

    let caught = false;

    await promise.catch((err) => {
      assert.equal(err.message, "foobar");
      caught = true;
    });

    assert.isTrue(caught);
  });

  it('reject:  direct finally()', async () => {
    let promise = new ManagedPromise();

    promise.reject(new Error("foobar"));

    let final = false;

    let settledPromise = promise.finally(() => {
      final = true;
    });

    try {
      await settledPromise;
      assert.fail();
    } catch (err) {
      // because otherwise the test fails on the pending error!
    }

    assert.isTrue(final);
  });
});