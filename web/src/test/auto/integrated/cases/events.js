import { assert } from '/node_modules/chai/chai.js';

import {
  loadKeyboardFromJSON,
  oskResourceLoadPromise,
  setupKMW,
  teardownKMW
} from "../test_utils.js";
import * as KMWRecorder from '/@keymanapp/keyman/build/tools/testing/recorder/lib/index.mjs';

describe('Event Management', function() {
  this.timeout(testconfig.timeouts.standard);

  before(function() {
    this.timeout(testconfig.timeouts.scriptLoad * 2);
    fixture.setBase('fixtures');
    fixture.load("eventTestConfig.html");

    return setupKMW(null, testconfig.timeouts.scriptLoad).then(() => {
      // We use this keyboard since we only need minimal input functionality for these tests.
      // Smaller is better when dealing with net latency.
      return loadKeyboardFromJSON("/keyboards/test_simple_deadkeys.json", testconfig.timeouts.scriptLoad);
    });
  });

  after(function() {
    teardownKMW();
  });

  it('Keystroke-based onChange event generation', async function() {
    var simple_A = {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0};
    var event = new KMWRecorder.PhysicalInputEventSpec(simple_A);

    var ele = document.getElementById("input");

    ele.onchange = function() {
      ele.onchange = null;
    }

    keyman.setActiveElement(ele);

    let eventDriver = new KMWRecorder.BrowserDriver(ele);
    await eventDriver.simulateEvent(event);

    let focusEvent = new FocusEvent('blur', {relatedTarget: ele});
    ele.dispatchEvent(focusEvent);

    // Asserts that the handler is called.  As the handler clears itself, it will only
    // remain set if it hasn't been called.
    assert.isNull(ele.onchange, '`onchange` handler was not called');
  });

  it('OSK-based onChange event generation', async function() {
    var simple_A = {"type":"osk","keyID":"default-K_A"};
    var event = new KMWRecorder.OSKInputEventSpec(simple_A);

    var ele = document.getElementById("input");

    ele.onchange = function() {
      ele.onchange = null;
    }

    keyman.setActiveElement(ele);

    // Browsers will only start loading OSK resources (the CSS) once both a keyboard and target
    // are set... and that's an async operation.
    await oskResourceLoadPromise();

    // OSK CSS is needed for successful simulation for integration tests involving the gesture engine.
    let eventDriver = new KMWRecorder.BrowserDriver(ele);
    await eventDriver.simulateEvent(event);

    let focusEvent = new FocusEvent('blur', {relatedTarget: ele});
    await ele.dispatchEvent(focusEvent);

    // Asserts that the handler is called.  As the handler clears itself, it will only
    // remain set if it hasn't been called.
    assert.isNull(ele.onchange, '`onchange` handler was not called');
  });

  it('Keystroke-based onInput event generation', async function() {
    var simple_A = {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0};
    var event = new KMWRecorder.PhysicalInputEventSpec(simple_A);

    var ele = document.getElementById("input");
    keyman.setActiveElement(ele);

    var counterObj = {i:0};
    var fin = 3;

    ele.addEventListener("input", function() {
      counterObj.i++;
    });

    let eventDriver = new KMWRecorder.BrowserDriver(ele);
    await eventDriver.simulateEvent(event);
    await eventDriver.simulateEvent(event);
    await eventDriver.simulateEvent(event);

    assert.equal(counterObj.i, fin, "Event handler not called the expected number of times");
  });

  it('OSK-based onInput event generation', async function() {
    var simple_A = {"type":"osk","keyID":"default-K_A"};
    var event = new KMWRecorder.OSKInputEventSpec(simple_A);

    var ele = document.getElementById("input");
    keyman.setActiveElement(ele);

    var counterObj = {i:0};
    var fin = 3;

    ele.addEventListener("input", function() {
      counterObj.i++;
    });

    let eventDriver = new KMWRecorder.BrowserDriver(ele);
    await eventDriver.simulateEvent(event);
    await eventDriver.simulateEvent(event);
    await eventDriver.simulateEvent(event);

    assert.equal(counterObj.i, fin, "Event handler not called the expected number of times");
  });
});