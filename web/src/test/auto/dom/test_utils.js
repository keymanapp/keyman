// // KeymanWeb test suite - processing of the Karma configuration's client.args parameter.

import Device from 'keyman/engine/device-detect';

export let DEVICE_DETECT_FAILURE = false;

// If we've set things up to support Device dection without loading KMW...
try {
  let device = new Device();
  device.detect();
} catch (err) {
  // Sets a warning flag that unit-test files can use to disable themselves.
  DEVICE_DETECT_FAILURE = true;
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
    frame.src = "resources/html/designIframe.html";

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
      }, testconfig.timeouts.eventDelay);
    } else {
      assertion();
    }
  }
}