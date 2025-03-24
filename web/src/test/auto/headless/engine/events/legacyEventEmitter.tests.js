import { assert } from 'chai';
import sinon from 'sinon';

import { LegacyEventEmitter } from 'keyman/engine/events';

// Tests the KeymanEngine's custom event emitter that adheres to our documented API event spec
// from KMW 16.0 and before.

describe("LegacyEventEmitter", () => {
  it('Calls listeners for events', () => {
    const emitter = new LegacyEventEmitter();
    const fakeListener = sinon.fake();

    const eventObj = {
      prop: "some value"
    };

    emitter.addEventListener('event', fakeListener);
    emitter.callEvent('event', eventObj);

    assert.isTrue(fakeListener.calledOnce);
    assert.deepStrictEqual(fakeListener.firstCall.args[0], eventObj);
  });

  /*
   * Future unit-test ideas:
   * - There is explicit handling for preventing an infinite event loop where an event
   *   indirectly retriggers itself down the line.
   * - Adherence to the callEvent return-type spec
   * - Listener returns false => no further listeners get called.
   */
});