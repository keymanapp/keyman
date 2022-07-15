// FIXME:  DELETE THIS FILE.  I ONLY COPIED IT HERE FOR EASE OF REFERENCE.

// Refer to gestureHost.css for the different layout CSS classes.

const controller = new Testing.HostFixtureLayoutController();
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
  recorder = new Testing.SequenceRecorder(controller);
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

  for(entry of layoutGroup) {
    entry.addEventListener('change', updateConfig);
  }

  for(entry of boundsGroup) {
    entry.addEventListener('change', updateConfig);
  }

  for(entry of receiverGroup) {
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

  for(entry of safeGroup) {
    entry.addEventListener('change', updateConfig);
  }

  updateConfig();
});

// TODO: Convert to use unit-test-resource's config class.
function updateConfig() {
  let layout = document.config.screen;     // Referred to radio-group values on the actual host page.
  let bounds = document.config.bounds;
  let receiver = document.config.receiver;
  let safe = document.config.safeZone;

  let layoutSpec = new Testing.FixtureLayoutConfiguration(
    layout.value,
    bounds.value,
    receiver.value,
    safe.value
  );

  controller.layoutConfiguration = layoutSpec;
}

// END: Layout-simulation setup & handling