// This script allows user-interactive use of the user-testing oriented unit-test-resource objects.

import {
  FixtureLayoutConfiguration,
  HostFixtureLayoutController,
  InputSequenceSimulator,
  SequenceRecorder
} from '../../../../build/tools/lib/index.mjs';

const controller = new HostFixtureLayoutController();
let loadPromise = controller.connect();
let recorder;

function presentSavableJSON(text) {
  var blob = new Blob([text], { type: 'text/plain' });
  let a = document.createElement('a');
  a.download = 'recorded.json';
  a.href = window.URL.createObjectURL(blob);
  a.click();
}

loadPromise.then((recognizer) => {
  recorder = new SequenceRecorder(controller);
  updateConfig();

  // DOM-oriented logging setup.
  let logElement = document.getElementById('event-log');
  // Erase any logs from before a page reload.
  logElement.value = '';

  let logClearButton = document.getElementById('log-clear-button');
  logClearButton.onclick = function() {
    logElement.value = '';
    recorder.clear();
  }

  controller.on(HostFixtureLayoutController.CONFIG_CHANGED_EVENT, () => {
    logElement.value = '';
  });

  let logSaveButton = document.getElementById('log-save-button');
  logSaveButton.onclick = function() {
    presentSavableJSON(recorder.recordingsToJSON());
  }

  recognizer.on('inputstart', function(sequence) {
    logElement.value = recorder.recordingsToJSON();

    sequence.on('update', function() {
      logElement.value = recorder.recordingsToJSON();
    });

    sequence.on('cancel', function() {
      logElement.value = recorder.recordingsToJSON();
    });

    sequence.on('end', function() {
      //let stats = sequence.touchpoints[0].path.segments[0].stats;
      logElement.value = recorder.recordingsToJSON();
    });
  });
});

// -- BEGIN:  Code for controlling the layout-simulation elements --

/** User-interactive configuration controls */
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

  document.addEventListener('scroll', updateAlignment);

  for(let entry of safeGroup) {
    entry.addEventListener('change', updateConfig);
  }

  updateConfig();

  // Check sub-pixel alignment of the main fixture element; we can't do exact repros for unit testing
  // if the recorded sequence has sub-pixel coordinates.  This may happen otherwise due to the page's
  // formatting.
  let aligner = document.getElementById('aligner');
  let hostFixture = document.getElementById('host-fixture');

  let clientRect = hostFixture.getBoundingClientRect();
  let roundX = Math.round(clientRect.x);
  let roundY = Math.round(clientRect.y);

  aligner.style.left = roundX - clientRect.x;
  aligner.style.top  = roundY - clientRect.y;
});

function updateConfig() {
  let layout = document.config.screen;     // Referred to radio-group values on the actual host page.
  let bounds = document.config.bounds;
  let receiver = document.config.receiver;
  let safe = document.config.safeZone;

  let layoutSpec = new FixtureLayoutConfiguration(
    layout.value,
    bounds.value,
    receiver.value,
    safe.value
  );

  controller.layoutConfiguration = layoutSpec;

  updateAlignment();
}

function updateAlignment() {
  // Check sub-pixel alignment of the main fixture element; we can't do exact repros for unit testing
  // if the recorded sequence has sub-pixel coordinates.  This may happen otherwise due to the page's
  // formatting.
  let aligner = document.getElementById('aligner');
  let hostFixture = document.getElementById('host-fixture');
  aligner.style.left = 0;
  aligner.style.top = 0;

  let clientRect = hostFixture.getBoundingClientRect();
  let roundX = Math.round(clientRect.x);
  let roundY = Math.round(clientRect.y);

  aligner.style.left = roundX - clientRect.x;
  aligner.style.top  = roundY - clientRect.y;
}

// END: Layout-simulation setup & handling