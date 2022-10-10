// KeymanWeb test suite - processing of the Karma configuration's client.args parameter.

com = com || {};
com.keyman = com.keyman || {};
com.keyman.karma = com.keyman.karma || {};
com.keyman.karma.DEVICE_DETECT_FAILURE = false;

// If we've set things up to support Device dection without loading KMW...
try {
  let device = new com.keyman.Device();
  device.detect();
} catch (err) {
  // Sets a warning flag that unit-test files can use to disable themselves.
  com.keyman.karma.DEVICE_DETECT_FAILURE = true;
}

// Keyman test suite utility methods

var setupKMW = function(kmwOptions, timeout) {
  var ui;

  if(typeof(kmwOptions) == 'string' || typeof(kmwOptions) == 'undefined' || kmwOptions == null) {
    ui = kmwOptions;

    var kmwOptions = {
      attachType:'auto',
      root:'source',
      resources:'../../../../source'
    };

    if(ui) {
      kmwOptions.ui = ui;
    }
  }

  const kmwPromise = setupScript('source/keymanweb.js', timeout, (scriptEle) => {
    fixture.el.appendChild(scriptEle);
  });

  ui = kmwOptions.ui;

  kmwOptions.attachType = kmwOptions.attachType ? kmwOptions.attachType : 'auto';

  if(!kmwOptions.root) {
    kmwOptions.root = 'source';
  }

  if(!kmwOptions.resources) {
    kmwOptions.resources = '../../../../source';
  }

  let uiPromise;
  if(ui) {
    uiPromise = setupScript('source/kmwui' + ui + '.js', timeout, (scriptEle) => {
      fixture.el.appendChild(scriptEle);
    });

    kmwOptions.ui=ui;
  }

  let compositePromise = kmwPromise;
  if(uiPromise) {
    compositePromise = Promise.all([kmwPromise, uiPromise]);
  }

  return finalPromise = compositePromise.then(() => {
    if(window['keyman']) {
      return window['keyman'].init(kmwOptions);
    } else {
      return Promise.reject();
    }
  });
}

/**
 * Produces a script element tied to a Promise for its eventual load (or failure thereof).
 *
 * The script element is only available via callback due to implementation constraints.
 *
 * @param {*} src       The source script's (relative) path on the test server.
 * @param {*} timeout
 * @param {*} functor   A callback to handle the script element.
 * @returns
 */
var setupScript = function(src, timeout, functor) {
  return new Promise((resolve, reject) => {
    const Lscript = document.createElement('script');
    let hasResolved = false;
    Lscript.charset="UTF-8";        // KMEW-89
    Lscript.type = 'text/javascript';
    Lscript.async = false;

    const timer = window.setTimeout(() => {
      reject("Script load attempt timed out.");
    }, timeout);

    Lscript.onload = Lscript.onreadystatechange = () => {
      window.clearTimeout(timer);
      if(!hasResolved && (Lscript.readyState === undefined || Lscript.readyState == "complete")) {
        hasResolved = true;
        resolve();
      }
    }

    Lscript.onerror = (err) => {
      window.clearTimeout(timer);
      reject(err);
    }

    Lscript.src = src;

    functor(Lscript);
  });
}

var teardownKMW = function() {
  var error = null;
  if(keyman) { // If our setupKMW fails somehow, this guard prevents a second error report on teardown.

    // We want to be SURE teardown works correctly, or we'll get lots of strange errors on other tests.
    // Thus, error-handling on shutdown itself.  It HAS mattered.
    try {
      keyman['shutdown']();
    } catch(err) {
      error = err;
    }

    try {
      var success = delete window["keyman"];
      if(!success) {
        window["keyman"] = undefined;
      }
    } finally {
      if(error) {
        console.log("Error during KMW shutdown!");
        throw error;
      }
    }
  }
}

var loadKeyboardStub = function(stub, timeout, params) {
  var kbdName = "Keyboard_" + stub.id;

  keyman.addKeyboards(stub);
  if(!params || !params.passive) {
    return keyman.setActiveKeyboard(kbdName, stub.languages.id);
  } else if(keyman.getActiveKeyboard() != kbdName) {
    return setupScript(stub.filename, timeout, (ele) => {
      fixture.el.appendChild(ele);
    });
  } else {
    return Promise.resolve();
  }
}

var loadKeyboardFromJSON = function(jsonPath, timeout, params) {
  var stub = fixture.load(jsonPath, true);

  return loadKeyboardStub(stub, timeout, params);
}

function runLoadedKeyboardTest(testDef, device, usingOSK, assertCallback) {
  var inputElem = document.getElementById('singleton');

  let proctor = new KMWRecorder.BrowserProctor(inputElem, device, usingOSK, assertCallback);
  testDef.test(proctor);
}

function runKeyboardTestFromJSON(jsonPath, params, assertCallback, timeout) {
  var testSpec = new KMWRecorder.KeyboardTest(fixture.load(jsonPath, true));
  let device = new com.keyman.Device();
  device.detect();

  return loadKeyboardStub(testSpec.keyboard, timeout).then(() => {
    runLoadedKeyboardTest(testSpec, device.coreSpec, params.usingOSK, assertCallback);
  }).finally(() => {
    keyman.removeKeyboards(testSpec.keyboard.id);
  });
}

function retrieveAndReset(Pelem) {
  let val = Pelem.value;
  Pelem.value = "";

  return val;
}

// Useful for tests related to strings with supplementary pairs.
var toSupplementaryPairString = function(code){
  var H = Math.floor((code - 0x10000) / 0x400) + 0xD800;
  var L = (code - 0x10000) % 0x400 + 0xDC00;

  return String.fromCharCode(H, L);
}

var toEscapedSupplementaryPairString = function(code){
  var H = (Math.floor((code - 0x10000) / 0x400) + 0xD800).toString(16);
  var L = ((code - 0x10000) % 0x400 + 0xDC00).toString(16);

  return "\\u"+H+"\\u"+L;
}

// Defines an object for dynamically adding elements for testing purposes.
// Designed for use with the robustAttachment.html fixture.

var DynamicElements;
var inputCounter = 0;

if(typeof(DynamicElements) == 'undefined') {
  DynamicElements = {};

  DynamicElements.addInput = function() {
    var masterDiv = document.getElementById('DynamicElements');
    var newInput = document.createElement("input");
    var i = inputCounter++;

    newInput.id = 'input' + i;
    newInput.className = 'test';
    newInput.placeholder = "Dynamic area #" + i + "!";

    masterDiv.appendChild(newInput);
    return newInput.id;
  }

  DynamicElements.addText = function () {
    var masterDiv = document.getElementById('DynamicElements');
    var newTextArea = document.createElement("textarea");
    var i = inputCounter++;

    newTextArea.id = 'textarea' + i;
    newTextArea.className = 'test';
    newTextArea.placeholder = "Dynamic area #" + i + "!";

    masterDiv.appendChild(newTextArea);
    return newTextArea.id;
  }

  DynamicElements.addIFrame = function(loadCallback) {
    var masterDiv = document.getElementById('DynamicElements');
    var frame = document.createElement("iframe");
    var i = inputCounter++;

    frame.height = "100";
    frame.id = 'iframe' + i;
    if(loadCallback) {
      frame.addEventListener('load', function() {
        // Give KMW's attachment events a chance to run first.
        window.setTimeout(loadCallback, Math.max(100, testconfig.timeouts.scriptLoad));
      });
    }
    frame.setAttribute("src", "resources/html/iframe.html");

    masterDiv.appendChild(frame);
    return frame.id;
  }

  DynamicElements.addDesignIFrame = function(loadCallback) {
    var masterDiv = document.getElementById('DynamicElements');
    var frame = document.createElement("iframe");
    var i = inputCounter++;

    frame.height = "100";
    frame.id = 'designIFrame' + i;
    frame.src = "resources/html/editableFrame.html";

    if(loadCallback) {
      frame.addEventListener('load', function() {
        loadCallback();
      });
    }

    masterDiv.appendChild(frame);
    return frame.id;
  }

  DynamicElements.addEditable = function() {
    var masterDiv = document.getElementById('DynamicElements');
    var editable = document.createElement("div");
    var i = inputCounter++;

    editable.contentEditable = true;
    editable.textContent = "Edit me!";
    editable.id = 'editable' + i;
    editable.style.width="500px";

    masterDiv.appendChild(editable);
    return editable.id;
  }

  DynamicElements.assertAttached = function(ele, done) {
    var assertion = function() {
      assert.isTrue(keyman.isAttached(ele), "Element tag '" + ele.tagName + "', id '" + ele.id + "' was not attached!");
    }
    if(done) {
      window.setTimeout(function() {
        assertion();
        done();
      }, testconfig.timeouts.eventDelay);
    } else {
      assertion();
    }
  }

  DynamicElements.assertDetached = function(ele, done) {
    var assertion = function() {
      assert.isFalse(keyman.isAttached(ele), "Element tag '" + ele.tagName + "', id '" + ele.id + "' was not detached!");
    }
    if(done) {
      window.setTimeout(function() {
        assertion();
        done();
      }, testconfig.timeouts.eventDelay);
    } else {
      assertion();
    }
  }

  // Is utilized only by the attachmentAPI test case, but it was originally defined as part of the same
  // object as the rest of DynamicElements, which is useful for numerous test cases.
  DynamicElements.init = function() {
    var s_key_json = {"type": "key", "key":"s", "code":"KeyS","keyCode":83,"modifierSet":0,"location":0};
    DynamicElements.keyCommand = new KMWRecorder.PhysicalInputEventSpec(s_key_json);

    DynamicElements.enabledLaoOutput = "ຫ";
    DynamicElements.enabledKhmerOutput = "ស";
    // Simulated JavaScript events do not produce text output.
    DynamicElements.disabledOutput = "";
  }

  DynamicElements.init();
}
