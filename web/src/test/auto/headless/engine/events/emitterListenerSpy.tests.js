import { assert } from 'chai';
import sinon from 'sinon';

import { EventEmitter } from 'eventemitter3';
import { EmitterListenerSpy, LegacyEventEmitter } from 'keyman/engine/events';

// Tests the emitter-spy class that may be used to detect registration and
// deregistration of event listeners within Keyman Engine for Web.

describe("EmitterListenerSpy", () => {
  it("EventEmitter", () => {
    const emitter = new EventEmitter();
    const emitterSpy = new EmitterListenerSpy(emitter);
    const fakeEventHandler = sinon.fake();
    const fakeAddListener = sinon.fake();
    const fakeRemoveListener = sinon.fake();

    emitterSpy.on('listeneradded', fakeAddListener);
    emitterSpy.on('listenerremoved', fakeRemoveListener);
    emitter.on('event', fakeEventHandler);

    assert.isTrue(fakeAddListener.calledOnce);
    assert.equal(fakeAddListener.firstCall.args[0], 'event');
    assert.isFalse(fakeRemoveListener.calledOnce);

    emitter.off('event', fakeEventHandler);
    assert.isTrue(fakeRemoveListener.calledOnce);
    assert.equal(fakeRemoveListener.firstCall.args[0], 'event');
  });

  it("LegacyEventEmitter", () => {
    const emitter = new LegacyEventEmitter();
    const emitterSpy = new EmitterListenerSpy(emitter);
    const fakeEventHandler = sinon.fake();
    const fakeAddListener = sinon.fake();
    const fakeRemoveListener = sinon.fake();

    emitterSpy.on('listeneradded', fakeAddListener);
    emitterSpy.on('listenerremoved', fakeRemoveListener);
    emitter.addEventListener('event', fakeEventHandler);

    assert.isTrue(fakeAddListener.calledOnce);
    assert.equal(fakeAddListener.firstCall.args[0], 'event');
    assert.isFalse(fakeRemoveListener.calledOnce);

    emitter.removeEventListener('event', fakeEventHandler);
    assert.isTrue(fakeRemoveListener.calledOnce);
    assert.equal(fakeRemoveListener.firstCall.args[0], 'event');
  });
});