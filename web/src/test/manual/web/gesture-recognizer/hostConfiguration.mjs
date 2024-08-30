// -- BEGIN:  Code for controlling the layout-simulation elements --

import { GestureRecognizer } from '../../../../../build/engine/gesture-processor/lib/index.mjs';

function updateConfig() {
  let layout = document.config.screen;
  let bounds = document.config.bounds;
  let receiver = document.config.receiver;
  let safe = document.config.safeZone;

  let demoContainer = document.getElementById("demo-container");
  demoContainer.className = layout.value + " " + bounds.value + " " + receiver.value + " " + safe.value;
}

window.addEventListener('load', function(ev) {
  let layoutGroup = document.config.screen;
  let boundsGroup = document.config.bounds;
  let receiverGroup = document.config.receiver;
  let safeGroup = document.config.safeZone;

  for(let entry of layoutGroup) {
    entry.addEventListener('change', updateConfig);
  }

  for(let entry of boundsGroup) {
    entry.addEventListener('change', updateConfig);
  }

  for(let entry of receiverGroup) {
    if(entry.value == 'full') {
      entry.addEventListener('change', function() {
        let topRange = document.getElementById('topOnlyRadio');
        topRange.checked = 'checked';
        updateConfig();
      });
    } else { // Popup will be the 'default' case - we may want to test multiple 'popup' configs.
      entry.addEventListener('change', function() {
        let popupRange = document.getElementById('popupRadio');
        popupRange.checked = 'checked';
        updateConfig();
      });
    }
  }

  for(let entry of safeGroup) {
    entry.addEventListener('change', updateConfig);
  }

  updateConfig();
});

// END: Layout-simulation setup & handling

// START:  Gesture-recognizer integration code!

// This will hold the main gesture-recognizer instance.
// Not great standard practice... but this is a development/debugging page, so we want access to it.
let recognizer;

window.addEventListener('load', function() {
  let recognizerConfig = {
    mouseEventRoot: document.body,
    targetRoot: document.getElementById('target-root'),
    maxRoamingBounds: document.getElementById('roaming-bounds'),
    safeBounds: document.getElementById('safe-zone'),
    paddedSafeBounds: document.getElementById('padded-safe-zone')
  };

  recognizer = new GestureRecognizer({gestures: [], sets: []}, recognizerConfig);

  let recordingArr = [];
  let logElement = document.getElementById('event-log');
  // Erase any logs from before a page reload.
  logElement.value = '';

  let logClearButton = document.getElementById('log-clear-button');
  logClearButton.onclick = function() {
    logElement.value = '';
    recordingArr = []; // erase previously-recorded sequences
  }

  recognizer.on('inputstart', function(input) {
    let touchpoint = input.touchpoints[0];
    recordingArr.push(touchpoint);

    const objectPrinter = function() {
      logElement.value = JSON.stringify(recordingArr, null, 2);
    }

    objectPrinter();

    input.on('update', function() {
      objectPrinter();
    });

    input.on('cancel', function() {
      objectPrinter();
    });

    input.on('end', function() {
      objectPrinter();
    });
  });
});

// END:  Gesture-recognizer integration code.