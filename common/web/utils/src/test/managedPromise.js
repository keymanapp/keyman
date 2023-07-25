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

  it('rejection', async () => {
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
});