// // KeymanWeb test suite - processing of the Karma configuration's client.args parameter.

import Device from 'keyman/engine/device-detect';
import * as KMWRecorder from '#recorder';

export let DEVICE_DETECT_FAILURE = false;

const loc = document.location;
// config.testFile generally starts with a '/', with the path resembling the actual full local
// filesystem for the drive.
const domain = `${loc.protocol}/${loc.host}`

// If we've set things up to support Device dection without loading KMW...
try {
  let device = new Device();
  device.detect();
} catch (err) {
  // Sets a warning flag that unit-test files can use to disable themselves.
  DEVICE_DETECT_FAILURE = true;
}

// // Keyman test suite utility methods

export function setupKMW(kmwOptions, timeout) {
  let ui;

  if(typeof(kmwOptions) == 'string' || typeof(kmwOptions) == 'undefined' || kmwOptions == null) {
    ui = kmwOptions;

    var kmwOptions = {
      attachType:'auto',
      root:'/',
      // up from 'browser/debug'
      resources:'../../resources'
    };

    if(ui) {
      kmwOptions.ui = ui;
    }
  }

  const kmwPromise = setupScript('web/build/app/browser/debug/keymanweb.js', timeout);

  ui = kmwOptions.ui;

  kmwOptions.attachType = kmwOptions.attachType ? kmwOptions.attachType : 'auto';

  if(!kmwOptions.root) {
    kmwOptions.root = 'web/build/app/browser/debug';
  }

  if(!kmwOptions.resources) {
    kmwOptions.resources = 'web/build/app/resources';
  }

  let uiPromise;
  if(ui) {
    uiPromise = setupScript('web/build/app/ui/debug/kmwui' + ui + '.js', timeout);
    kmwOptions.ui=ui;
  }

  let compositePromise = kmwPromise;
  if(uiPromise) {
    compositePromise = Promise.all([kmwPromise, uiPromise]);
  }

  return compositePromise.then(() => {
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
 * @returns
 */
export function setupScript(src, timeout) {
  return setupScriptInternal(src, timeout);
}

function setupScriptInternal(src, timeout, attemptCount, existingTimer) {
  attemptCount = attemptCount || 1;
  if(attemptCount > 1) {
    console.log("Re-attempting load of script '" + src + "': retry #" + attemptCount);
  }

  let promise = new Promise((resolve, reject) => {
    const Lscript = document.createElement('script');
    let hasResolved = false;
    Lscript.charset="UTF-8";        // KMEW-89
    Lscript.type = 'text/javascript';
    Lscript.async = false;

    const timer = existingTimer !== undefined ? existingTimer : window.setTimeout(() => {
      // May have already been removed if there was an error -> repeat attempt.
      Lscript.parentElement?.removeChild(Lscript);
      hasResolved = true;
      reject(new Error("Script load attempt for '" + src + "' timed out."));
    }, timeout);

    Lscript.onload = Lscript.onreadystatechange = () => {
      Lscript.parentElement.removeChild(Lscript);
      window.clearTimeout(timer);
      if(!hasResolved && (Lscript.readyState === undefined || Lscript.readyState == "complete")) {
        hasResolved = true;

        if(attemptCount > 1) {
          console.log("Successfully loaded '" + src + "' after " + attemptCount + " attempts");
        }
        resolve();
      }
    }

    Lscript.onerror = (err) => {
      // Cleanup the noisy script tag
      Lscript.parentElement.removeChild(Lscript);

      if(hasResolved) {
        return;
      }

      // WARNING: err here is a very basic event object that doesn't actually give us useful info.

      // One common side-effect of BrowserStack instability is that this event will be raised.
      // Sadly, it's hard to know _why_ it's raised due to the point above - no useful info shows
      // up in the logs, and often only one attempt fails.  But that may be key - _often only ONE
      // attempt_ fails.  Therefore... let's enact a multi-retry on the script-load, so long as
      // we don't exceed the timeout as a result!
      if(attemptCount <= 3) {
        window.setTimeout(() => {
          let retryPromise = setupScriptInternal(src, timeout, attemptCount + 1, existingTimer);
          retryPromise.then(resolve).catch(reject);
        }, 20);
      } else {
        window.clearTimeout(timer);
        reject(new Error("KMW script '" + src + "' loading failed with an unknown error."));
      }
    }

    Lscript.src = src;
    document.body.appendChild(Lscript);
  });

  return promise;
}

export function teardownKMW() {
  var error = null;

  // If our setupKMW fails somehow, this guard prevents a second error report on teardown.
  if(window.keyman) {
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

export async function loadKeyboardStub(stub, timeout, params) {
  var kbdName = "Keyboard_" + stub.id;

  // Returning an "error stub" does not actually throw an error.  Makes
  // picking up on related errors in unit-test dev a bit trickier, but
  // we can manually throw the error from here.
  let result = await keyman.addKeyboards(stub);
  for(let i=0; i < result.length; i++) {
    if(result[i].error) {
      throw result[i].error;
    }
  }

  if(!params || !params.passive) {
    return keyman.setActiveKeyboard(kbdName, stub.languages.id);
  } else if(keyman.getActiveKeyboard() != kbdName) {
    return setupScript(stub.filename, timeout);
  } else {
    return Promise.resolve();
  }
}

export async function loadKeyboardFromJSON(jsonPath, timeout, params) {
  const jsonResponse = await fetch(new URL(`${domain}/${jsonPath}`));
  const stub = await jsonResponse.json();

  return loadKeyboardStub(stub, timeout, params);
}

async function runLoadedKeyboardTest(testDef, device, usingOSK, assertCallback) {
  var inputElem = document.getElementById('singleton');

  let proctor = new KMWRecorder.BrowserProctor(inputElem, device, usingOSK, assertCallback);
  await testDef.test(proctor);
}

export async function runKeyboardTestFromJSON(jsonPath, params, assertCallback, timeout) {
  const jsonResponse = await fetch(new URL(`${domain}/${jsonPath}`));
  const testJSON = await jsonResponse.json();

  var testSpec = new KMWRecorder.KeyboardTest(testJSON);
  let device = new Device();
  device.detect();

  return loadKeyboardStub(testSpec.keyboard, timeout).then(() => {
    return runLoadedKeyboardTest(testSpec, device.coreSpec, params.usingOSK, assertCallback);
  }).finally(() => {
    keyman.removeKeyboards(testSpec.keyboard.id);
  });
}

export async function oskResourceLoadPromise() {
  // If the CSS isn't fully loaded, the element positions will not match their expected
  // locations in the keyboard layout and OSK keys won't be triggered properly by the
  // gesture engine.
  const styleManager = keyman.osk['uiStyleSheetManager']; // is private
  await styleManager.allLoadedPromise();
}

// Useful for tests related to strings with supplementary pairs.
export function toSupplementaryPairString(code) {
  var H = Math.floor((code - 0x10000) / 0x400) + 0xD800;
  var L = (code - 0x10000) % 0x400 + 0xDC00;

  return String.fromCharCode(H, L);
}

export function toEscapedSupplementaryPairString(code) {
  var H = (Math.floor((code - 0x10000) / 0x400) + 0xD800).toString(16);
  var L = ((code - 0x10000) % 0x400 + 0xDC00).toString(16);

  return "\\u"+H+"\\u"+L;
}

// Defines an object for dynamically adding elements for testing purposes.
// Designed for use with the robustAttachment.html fixture.

export let DynamicElements;
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
        window.setTimeout(loadCallback, Math.max(100, 5000));
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
      }, 5000);
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
      }, 5000);
    } else {
      assertion();
    }
  }
}