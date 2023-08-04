import { assert } from 'chai';
import sinon from 'sinon';

import { SimpleActivator, TwoStateActivator } from 'keyman/engine/osk';

// Tests the activation-state logic abstraction & implementations used to model and control OSK visibility.

describe("Activators", () => {
  describe("Simple Activator", () => {
    it('default state after construction', () => {
      const activator = new SimpleActivator();

      assert.isTrue(activator.enabled);
      assert.isTrue(activator.conditionsMet);
      assert.isTrue(activator.activate);
    });

    it("set to 'off'", () => {
      const activator = new SimpleActivator();
      activator.enabled = false;

      assert.isFalse(activator.enabled);
      assert.isTrue(activator.conditionsMet);
      assert.isFalse(activator.activate);
    });

    it('activate and enabled act as aliases', () => {
      const activator = new SimpleActivator();

      assert.isTrue(activator.enabled);
      assert.isTrue(activator.activate);

      activator.enabled = false;
      assert.isFalse(activator.activate);

      activator.activate = true;
      assert.isTrue(activator.enabled);
    });

    it('event generation', () => {
      const activator = new SimpleActivator();
      const stub = sinon.fake();

      activator.on('activate', stub);
      assert.equal(stub.callCount, 0);

      // Simple case:  toggling back and forth.
      activator.enabled = false;
      activator.enabled = true;

      assert.equal(stub.callCount, 2);
      assert.isFalse(stub.firstCall.args[0]);
      assert.isTrue(stub.secondCall.args[0]);

      // Less-simple:  repeated toggles to the same state should not raise an event.
      activator.off('activate', stub);
      const stub2 = sinon.fake();
      activator.on('activate', stub2);

      assert.isTrue(activator.enabled);

      activator.enabled = true;
      activator.enabled = true;

      assert.equal(stub2.callCount, 0);

      activator.enabled = false; // +1

      assert.equal(stub2.callCount, 1);
      assert.isFalse(stub.firstCall.args[0]);

      activator.enabled = true;  // +2

      assert.equal(stub2.callCount, 2);
      assert.isTrue(stub.secondCall.args[0]);

      activator.enabled = true;

      assert.equal(stub2.callCount, 2);
    });
  });

  describe("Two-State Activator", () => {
    it('default state after construction', () => {
      const activator = new TwoStateActivator();

      assert.isTrue(activator.enabled);
      assert.isFalse(activator.conditionsMet);
      assert.isFalse(activator.activate);
      assert.isNotOk(activator.activationTrigger);
    });

    it('default state + trigger set', () => {
      const activator = new TwoStateActivator();
      const activateStub = sinon.fake();
      const triggerStub = sinon.fake();
      activator.on('activate', activateStub);
      activator.on('triggerchange', triggerStub);

      activator.activationTrigger = "foo"; // using a string, just 'cause.

      assert.isTrue(activator.enabled);
      assert.isTrue(activator.conditionsMet);
      assert.isTrue(activator.activate);
      assert.isOk(activator.activationTrigger);

      assert.isTrue(activateStub.called);
      assert.isTrue(activateStub.firstCall.args[0]);

      assert.isTrue(triggerStub.called);
      assert.equal(triggerStub.firstCall.args[0], "foo");
    });

    it("'activate' event generation", () => {
      const activator = new TwoStateActivator();
      const stub = sinon.fake();
      activator.on('activate', stub);

      activator.enabled = false;
      activator.activationTrigger = "foo";

      assert.equal(stub.callCount, 0);

      activator.enabled = true;

      assert.equal(stub.callCount, 1);

      activator.activationTrigger = "bar";

      assert.equal(stub.callCount, 1); // Still activated.  Only the trigger changed.

      activator.activationTrigger = null;

      assert.equal(stub.callCount, 2);

      activator.enabled = false;
      activator.enabled = true;
      activator.enabled = false;
      activator.enabled = true;
      activator.enabled = false;

      assert.equal(stub.callCount, 2);

      activator.activationTrigger = "foo";
      activator.activationTrigger = "bar";

      assert.equal(stub.callCount, 2);

      activator.enabled = true;

      assert.equal(stub.callCount, 3);
    });

    it("'triggerchange' event generation", () => {
      const activator = new TwoStateActivator();
      const stub = sinon.fake();
      activator.on('triggerchange', stub);

      activator.enabled = false;

      assert.equal(stub.callCount, 0);

      activator.activationTrigger = "foo";

      assert.equal(stub.callCount, 1);

      activator.enabled = true;

      assert.equal(stub.callCount, 1);

      activator.activationTrigger = "bar";

      assert.equal(stub.callCount, 2);

      activator.activationTrigger = null;

      assert.equal(stub.callCount, 3);

      activator.enabled = false;
      activator.enabled = true;
      activator.enabled = false;
      activator.enabled = true;
      activator.enabled = false;

      assert.equal(stub.callCount, 3);

      activator.activationTrigger = "foo";

      assert.equal(stub.callCount, 4);

      activator.activationTrigger = "bar";

      assert.equal(stub.callCount, 5);

      activator.enabled = true;

      assert.equal(stub.callCount, 5);
    });

    it("'enabled' and 'activationTrigger' do not alias each other", () => {
      const activator = new TwoStateActivator();

      activator.enabled = false;
      activator.activationTrigger = "foo";

      assert.isFalse(activator.enabled);
      assert.equal(activator.activationTrigger, "foo");

      activator.activationTrigger = null;
      activator.enabled = true;

      assert.isTrue(activator.enabled);
      assert.isNull(activator.activationTrigger);

      activator.activationTrigger = false; // data type change here isn't OK in TS-land, though.
      assert.isTrue(activator.enabled);
    });

    it("'activationTrigger' as object", () => {
      const activator = new TwoStateActivator();
      const stub = sinon.fake();
      activator.on('triggerchange', stub);

      const object = {a: {b: {c: "def"}, g: "hi"}, jk: ["l"]};

      activator.activationTrigger = object;

      assert.equal(stub.callCount, 1);
      // The exact object, at all levels, should pass through.
      assert.deepStrictEqual(stub.firstCall.args[0], object);
    });
  });
});