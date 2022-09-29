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
  });

  it('Keystroke-based onChange event generation', function() {
    var simple_A = {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0};
    var event = new KMWRecorder.PhysicalInputEventSpec(simple_A);

    var ele = document.getElementById("input");
    var aliasing = false;

    ele.onchange = function() {
      ele.onchange = null;
    }

    if(ele['kmw_ip']) {
      ele = ele['kmw_ip'];
      aliasing = true;
    }

    // A bit of a force-hack to ensure the element is seen as active for the tests.
    com.keyman.dom['DOMEventHandlers'].states._lastActiveElement = ele;
    com.keyman.dom['DOMEventHandlers'].states._activeElement = ele;

    let eventDriver = new KMWRecorder.BrowserDriver(ele);
    eventDriver.simulateEvent(event);

    var focusEvent = new FocusEvent('blur', {relatedTarget: ele});

    if(focusEvent) {
      ele.dispatchEvent(focusEvent);
    }

    assert.isNull(ele.onchange);
  });

  it('OSK-based onChange event generation', function() {
    var simple_A = {"type":"osk","keyID":"default-K_A"};
    var event = new KMWRecorder.OSKInputEventSpec(simple_A);

    var ele = document.getElementById("input");
    var aliasing = false;

    ele.onchange = function() {
      ele.onchange = null;
    }

    if(ele['kmw_ip']) {
      ele = ele['kmw_ip'];
      aliasing = true;
    }

    // A bit of a force-hack to ensure the element is seen as active for the tests.
    com.keyman.dom['DOMEventHandlers'].states._lastActiveElement = ele;
    com.keyman.dom['DOMEventHandlers'].states._activeElement = ele;

    let eventDriver = new KMWRecorder.BrowserDriver(ele);
    eventDriver.simulateEvent(event);

    var focusEvent;

    if(typeof FocusEvent == 'function') {
      focusEvent = new FocusEvent('blur', {relatedTarget: ele});
    } else {
      focusEvent = document.createEvent("FocusEvent");
      focusEvent.initFocusEvent("blur", true, false, ele.ownerDocument.defaultView, 0, ele);
    }

    if(focusEvent) {
      ele.dispatchEvent(focusEvent);
    }

    assert.isNull(ele.onchange);
  });

  it('Keystroke-based onInput event generation', function() {
    var simple_A = {"type":"key","key":"a","code":"KeyA","keyCode":65,"modifierSet":0,"location":0};
    var event = new KMWRecorder.PhysicalInputEventSpec(simple_A);

    var ele = document.getElementById("input");
    var aliasing = false;

    var counterObj = {i:0};
    var fin = 3;

    ele.addEventListener("input", function() {
      counterObj.i++;
    });

    if(ele['kmw_ip']) {
      ele = ele['kmw_ip'];
      aliasing = true;
    }

    let eventDriver = new KMWRecorder.BrowserDriver(ele);
    eventDriver.simulateEvent(event);
    eventDriver.simulateEvent(event);
    eventDriver.simulateEvent(event);

    assert.equal(counterObj.i, fin, "Event handler not called the expected number of times");
  });

  it('OSK-based onInput event generation', function() {
    var simple_A = {"type":"osk","keyID":"default-K_A"};
    var event = new KMWRecorder.OSKInputEventSpec(simple_A);

    var ele = document.getElementById("input");
    var aliasing = false;

    var counterObj = {i:0};
    var fin = 3;

    ele.addEventListener("input", function() {
      counterObj.i++;
    });

    if(ele['kmw_ip']) {
      ele = ele['kmw_ip'];
      aliasing = true;
    }

    let eventDriver = new KMWRecorder.BrowserDriver(ele);
    eventDriver.simulateEvent(event);
    eventDriver.simulateEvent(event);
    eventDriver.simulateEvent(event);

    assert.equal(counterObj.i, fin, "Event handler not called the expected number of times");
  });
});