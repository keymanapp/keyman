var assert = chai.assert;

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
    fixture.cleanup();
  });

  buildEvent = (type, ele) => new FocusEvent(type, {relatedTarget: ele});

  doFocus = (ele) => {
    // Sometimes this fails to trigger our event handlers when unit testing, though it DOES set
    // the document's focus correctly.
    ele.focus();

    // If our event handlers didn't trigger, force 'em via synthetic event.
    if(!keyman.domManager.activeElement) {
      ele.dispatchEvent(buildEvent('focus', ele));
    }

    // We assume that one of the two cases above will properly update KMW to the expected state.
    assert.isOk(keyman.domManager.activeElement, "Test initialization failure");
  };

  doBlur = (ele) => {
    // The same behaviors documented in `doFocus` apply here.
    ele.blur();

    if(keyman.domManager.activeElement) {
      ele.dispatchEvent(buildEvent('blur', ele));
    }

    // We assume that one of the two cases above will properly update KMW to the expected state.
    assert.isNotOk(keyman.domManager.activeElement, "Test execution failure");
  }

  it('Keystroke-based onChange event generation', function() {
    var simple_A = {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0};
    var event = new KMWRecorder.PhysicalInputEventSpec(simple_A);

    var ele = document.getElementById("input");
    doFocus(ele);

    ele.onchange = function() {
      ele.onchange = null;
    }

    let eventDriver = new KMWRecorder.BrowserDriver(ele);
    eventDriver.simulateEvent(event);

    doBlur(ele);

    if(ele.onchange) {
      assert.fail("Event did not fire as expected");
    }
  });

  it('OSK-based onChange event generation', function() {
    var simple_A = {"type":"osk","keyID":"default-K_A"};
    var event = new KMWRecorder.OSKInputEventSpec(simple_A);

    var ele = document.getElementById("input");
    doFocus(ele);

    ele.onchange = function() {
      ele.onchange = null;
    }

    let eventDriver = new KMWRecorder.BrowserDriver(ele);
    eventDriver.simulateEvent(event);

    doBlur(ele);

    if(ele.onchange) {
      assert.fail("Event did not fire as expected");
    }
  });

  it('Keystroke-based onInput event generation', function() {
    // The only possibly-relevant browser not implementing InputEvent:  Opera Mini.
    // We no longer consider IE.  So long as we don't test against either, there's
    // no need to condition this test.

    var simple_A = {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0};
    var event = new KMWRecorder.PhysicalInputEventSpec(simple_A);

    var ele = document.getElementById("input");
    doFocus(ele);

    var counterObj = {i:0};
    var fin = 3;

    ele.addEventListener("input", function() {
      counterObj.i++;
    });

    let eventDriver = new KMWRecorder.BrowserDriver(ele);
    eventDriver.simulateEvent(event);
    eventDriver.simulateEvent(event);
    eventDriver.simulateEvent(event);

    if(counterObj.i != fin) {
      assert.fail(`InputEvent only signalled ${counterObj.i} out of ${fin} expected times.`);
    }
  });

  it('OSK-based onInput event generation', function() {
    // The only possibly-relevant browser not implementing InputEvent:  Opera Mini.
    // We no longer consider IE.  So long as we don't test against either, there's
    // no need to condition this test.

    var simple_A = {"type":"osk","keyID":"default-K_A"};
    var event = new KMWRecorder.OSKInputEventSpec(simple_A);

    var ele = document.getElementById("input");
    doFocus(ele);

    var counterObj = {i:0};
    var fin = 3;

    ele.addEventListener("input", function() {
      counterObj.i++;
    });

    let eventDriver = new KMWRecorder.BrowserDriver(ele);
    eventDriver.simulateEvent(event);
    eventDriver.simulateEvent(event);
    eventDriver.simulateEvent(event);

    if(counterObj.i != fin) {
      assert.fail(`InputEvent only signalled ${counterObj.i} out of ${fin} expected times.`);
    }
  });
});