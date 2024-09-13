import * as KMWRecorder from '../../../../build/tools/testing/recorder/lib/index.mjs';
import { DeviceDetector } from '../../../../build/engine/main/lib/index.mjs';

import { getResourcePathPrefix } from './recorder_KeyboardScripts.js';

let RESOURCE_PATH_PREFIX = getResourcePathPrefix();

var ta_inputJSON;
var in_output;
var recorderScribe;

window.focusReceiver = () => {
  var receiver = document.getElementById('receiver');
  receiver.focus();

  if(keyman.config.hostDevice.touchable) {
    // At present, touch doesn't 'focus' properly.
    keyman.setActiveElement(receiver);
    keyman.osk.show(true);
  }
}

window.setElementText = function(ele, text) {
  ele.value = text;
}

window.onUpdateInputRecord = function(json) {
  setElementText(ta_inputJSON, json);
}

window.onResetInputRecord = function() {
  setElementText(ta_inputJSON, "");
  setElementText(in_output, "");
  setElementText(document.getElementById('errorText'), "");
}

window.resetInputRecord = function() {
  recorderScribe.resetInputRecord();
}

window.copyInputRecord = function() {
  try {
    ta_inputJSON.select();

    var res = document.execCommand('copy');
    if(res) {
      in_output.focus();
      return;
    }
  } catch (err) { console.log(err) }
  alert("Unable to copy successfully.");
}

window.saveInputRecord = (constraint) => {
  if(!constraint) {
    var target;
    if(recorderScribe.currentSequence.hasOSKInteraction()) {
      target = keyman.config.hostDevice.formFactor;
    } else {
      target = 'hardware';
    }
    var os_list = getPlatforms();
    var browsers = getBrowsers();
    constraint = new KMWRecorder.Constraint(target, os_list, browsers);
  }

  recorderScribe.saveInputRecord(constraint);
}

window.reviseInputRecord = function() {
  recorderScribe.currentSequence = new KMWRecorder.InputEventSpecSequence(JSON.parse(ta_inputJSON.value));
}

window.onTestDefinitionChanged = function(testDefJSON) {
  var masterJSON = document.getElementById('masterJSON');
  masterJSON.value = testDefJSON;
}

window.setTestDefinition = function(testDef) {
  if(!testDef) {
    testDef = new KMWRecorder.KeyboardTest();
  }
  recorderScribe.testDefinition = testDef;
}

window.copyTestDefinition = function() {
  var masterJSON = document.getElementById('masterJSON');

  try {
    masterJSON.select();

    var res = document.execCommand('copy');
    if(res) {
      in_output.focus();
      return;
    }
  } catch (err) { console.log(err) }
  alert("Unable to copy successfully.");
}

window.onKeyboardChanged = function(kbdProperties) {
  let PInternalName = kbdProperties.internalName;
  let PLgCode = kbdProperties.languageCode;

  // Set the current keyboard on our select element
  var kbdSelect = document.getElementById("KMW_Keyboard");
  if(PInternalName == '') {
    kbdSelect.selectedIndex = 0;
  } else {
    for(var i=1; i < kbdSelect.length; i++) {
      var tag = kbdSelect.item(i);
      if(tag.value == PInternalName + "$$" + PLgCode) {
        kbdSelect.selectedIndex = i;
        break;
      }
    }
  }
}

window.errorUpdate = () => {
  var errorInput = document.getElementById('errorText');
  recorderScribe.errorUpdate(errorInput.value);
}

window.initDevice = () => {
  // From KMW.
  var device = new DeviceDetector();
  device.detect();

  document.getElementById("activeFormFactor").textContent = device.formFactor;
  document.getElementById("activeTouch").textContent = device.touchable ? 'Supported' : 'None';
  document.getElementById("activeOS").textContent = device.OS;
  document.getElementById("activeBrowser").textContent = device.browser;
}

window.addEventListener('load', async function() {
  ta_inputJSON = document.getElementById('inputRecord');
  in_output = document.getElementById('receiver');

  await keyman.config.deferForInitialization.corePromise;

  // Set up Scribe methods
  recorderScribe = new KMWRecorder.Scribe();
  recorderScribe.initHooks(in_output);
  recorderScribe.on('record-changed', onUpdateInputRecord);
  recorderScribe.on('record-reset', onResetInputRecord);
  recorderScribe.on('test-changed', onTestDefinitionChanged);

  keyman.contextManager.page.attachToControl(in_output);
  keyman.setKeyboardForControl(in_output, '', '');
  keyman.addEventListener('keyboardchange', onKeyboardChanged);

  // Used to initialize the responsive JSON-display elements.
  onResetInputRecord();
  onTestDefinitionChanged('');

  // Other setup.
  initDevice();
  setupKeyboardPicker();
});

//var p={'internalName':_internalName,'language':_language,'keyboardName':_keyboardName,'languageCode':_languageCode};
window.keyboardAdded = (properties) => {
  var kbdControl = document.getElementById('KMW_Keyboard');

  var opt = document.createElement('OPTION');
  opt.value = properties.internalName + "$$" + properties.languageCode;
  opt.innerHTML = properties.keyboardName + " (" + properties.language + ")";
  kbdControl.appendChild(opt);
}

window.setupKeyboardPicker = () => {
  /* Make sure that Keyman is initialized (we can't guarantee initialization order) */
  keyman.init();

  var kbdControl = document.getElementById('KMW_Keyboard');
  /* Retrieve the list of keyboards available from KeymanWeb and populate the selector using the DOM */
  var kbds = keyman.getKeyboards();
  for(var kbd in kbds) {
    var opt = document.createElement('OPTION');
    opt.value = kbds[kbd].InternalName + "$$" + kbds[kbd].LanguageCode;
    opt.innerHTML = kbds[kbd].Name;
    kbdControl.appendChild(opt);
  }

  // Ensures the default keyboard is active, to match our listbox's initial (default) option.
  keyman.setActiveKeyboard('', '');
  keyman.addEventListener('keyboardregistered', keyboardAdded);
}

window.doKeyboardChange = (name, languageCode) => {
  var activeElement = document.activeElement;
  if(!activeElement) {
    activeElement = in_output;
  }
  recorderScribe.keyboardJustActivated = true;
  focusReceiver();
  let promise = keyman.setActiveKeyboard(name, languageCode);
  return promise.then(function() {
    activeElement.focus();
  });
}

/* Called when user selects an item in the KMW_Keyboard SELECT */
window.KMW_KeyboardChange = () => {
  var kbdControl = document.getElementById('KMW_Keyboard');
  /* Select the keyboard in KeymanWeb */
  var name = kbdControl.value.substr(0, kbdControl.value.indexOf("$$"));
  var languageCode = kbdControl.value.substr(kbdControl.value.indexOf("$$")+2);

  doKeyboardChange(name, languageCode);
}

window.loadExistingTest = (files) => {
  if(files.length > 0) {
    var reader = new FileReader();
    reader.onload = function() {
      var kbdTest;
      try {
        kbdTest = new KMWRecorder.KeyboardTest(reader.result);
        recorderScribe.testDefinition = kbdTest;

        // Make sure we've loaded the keyboard!  Problem - we're not running from the unit_tests folder!
        var kbdStub = new KMWRecorder.KeyboardStub(kbdTest.keyboard);
        kbdStub.filename = RESOURCE_PATH_PREFIX + "/" + kbdStub.filename;

        keyman.addKeyboards(kbdStub);
      } catch (e) {
        alert("File does not contain a valid KeyboardTest definition.")
        console.error(e);
      }

      let promise = doKeyboardChange("Keyboard_" + kbdTest.keyboard.id, kbdTest.keyboard.getFirstLanguage());
      promise.then(function() {
        let convertBtn = document.getElementById('btnConvert');
        convertBtn.style.display = kbdTest.isLegacy ? 'inline-block' : 'none';

        if(kbdTest.isLegacy && confirm("This test definition uses an older version of the KMW test spec.  Update?  ('OK' to update, 'Cancel' to load in read-only mode.)")) {
          convertTestDefinition(kbdTest);
        }
      });
    }
    reader.readAsText(files[0]);
  }
}

// --------- TEST DEFINITIONS: SPEC VERSION MIGRATION ----------

window.convertTestDefinition = (sourceSet) => {
  if(!sourceSet) {
    // Called via HTML button, so we use the currently-loaded test definition.
    sourceSet = recorderScribe.testDefinition;
  }

  setTestDefinition(new KMWRecorder.KeyboardTest(sourceSet.keyboard));

  let currentPromise = Promise.resolve();

  // Iterate over the contained TestSets and simulate them, allowing re-recording of the events
  // with the up-to-date Recorder version.
  for(let i=0; i < sourceSet.inputTestSets.length; i++) {
    let testSet = sourceSet.inputTestSets[i];
    currentPromise = currentPromise.then(function() { return convertSet(testSet); });
  }

  return currentPromise.then(function() {
    // success!
    let convertBtn = document.getElementById('btnConvert');
    convertBtn.style.display = 'none';
    return true;
  }, function(error) {
    // failure.
    // Restore the original test def, as if we hadn't tried to convert it.
    console.error(error);
    setTestDefinition(sourceSet);
    return false;
  }).then(function(success) {
    // Reset KMW's detected-device set to normal.
    // keyman.util.initDevices();

    if(success) {
      alert("Test Definition conversion complete.");
    } else {
      alert("Issue(s) detected during attempted conversion; process aborted.");
    }
  });
}

window.convertSet = (testSet) => {
  // Ensure KMW's master Device property reflects what is needed for the test!
  let simDevice = deviceFromConstraint(testSet.constraint);
  let simPhysDevice = new com.keyman.Device();
  simPhysDevice.touchable = false;
  simPhysDevice.browser = simDevice.browser;
  simPhysDevice.formFactor = 'desktop';
  simPhysDevice.OS = simDevice.OS;

  keyman.util.device = simDevice;
  keyman.util.physicalDevice = simPhysDevice;

  // Now to run the tests.
  let proctor = new KMWRecorder.BrowserProctor(in_output, simDevice.coreSpec, testSet.constraint.target != 'hardware');
  proctor.beforeAll();

  let currentPromise = Promise.resolve();
  for(let i = 0; i < testSet.testSet.length; i++) { // Cannot use for(let sequence of ...) b/c not Promise-compatible.
    let sequence = testSet.testSet[i];
    currentPromise = currentPromise.then(function() {
      keyman.setActiveElement(in_output); // Ensure it's the active element; some of the recorder/conversion process will change this!
      proctor.simulateSequence(sequence);

      // Copy over any custom error messages that would be displayed on unit test failure.
      if(sequence.msg) {
        recorderScribe.errorUpdate(sequence.msg);
      }

      return new Promise(function(resolve) {
        let saveFunc = function() {
          // May need conversion in the future, but is fine for now - the 'Constraint' part of the spec didn't change.
          saveInputRecord(testSet.constraint);  // Directly copy the original constraint.
          resolve();
        };

        let saveCheck = function() {
          if(recorderScribe.currentSequence.inputs.length == sequence.inputs.length) {
            saveFunc();
          } else {
            // Delay until all inputs have processed properly.
            window.setTimeout(function() {
              saveCheck(); // Wait an extra block of time if the results aren't ready yet.
            }, sequence.inputs.length); // Each input has a 1 ms wait before addInputRecord is called.
          }
        }

        saveCheck();
      });
    });
  }

  return currentPromise;
}

window.deviceFromConstraint = (constraint) => {
  let trueDevice = new com.keyman.Device();
  trueDevice.detect();

  let simDevice = new com.keyman.Device();
  simDevice.detect();

  if(constraint.validOSList && constraint.validOSList.indexOf(trueDevice.OS) < 0) {
    simDevice.OS = constraint.validOSList[0];
  }

  if(constraint.validBrowsers && constraint.validBrowsers.indexOf(trueDevice.browser) < 0) {
    simDevice.browser = constraint.validBrowsers[0];
  }

  // Now, the more difficult one:  form-factor matching.
  switch(constraint.target) {
    case 'hardware':
      simDevice.formFactor = 'desktop';
      break;
    case 'desktop':
    case 'phone':
    case 'tablet':
      simDevice.formFactor = constraint.target;
      simDevice.touchable = 'true';
  }

  return simDevice;
}

// ----------- END SPEC VERSION MIGRATION -------------

window.loadExistingStubs = (files) => {

  var processStub = function(json, file) {
    try {
      // Load the stub
      var kbdStub = new KMWRecorder.KeyboardStub(JSON.parse(json));
      kbdStub.filename = RESOURCE_PATH_PREFIX + "/" + kbdStub.filename;

      keyman.addKeyboards(kbdStub);

      if(files.length == 1) {
        doKeyboardChange("Keyboard_" + kbdStub.id, kbdStub.getFirstLanguage());
      }
    } catch (e) {
      if(file instanceof File) {
        alert("File " + file.name + " does not contain a valid KeyboardStub definition.")
      } else {
        alert("File " + file + " does not contain a valid KeyboardStub definition.");
      }
      console.error(e);
      let stubHelpDiv = document.getElementById("stubHelp");
      stubHelpDiv.style.display="inline-block";
    }
  }

  // We need this as a separate function for its closure behavior.
  var readFile = function(index) {
    var reader = new FileReader();
    var file = files[index];
    if(file instanceof File) {
      reader.onload = function() {
        processStub(reader.result, file);
      }
      reader.readAsText(file);
    } else if(typeof file == "string") {
      var reader = new XMLHttpRequest();
      reader.open('GET', file);
      reader.responseType = 'text';

      reader.onload = function() {
        processStub(reader.response, file);
      };

      reader.send();
    }
  }

  for(var i=0; i < files.length; i++) {
    readFile(i);
  }
}

var PLATFORMS = ['windows', 'macosx', 'linux', 'android', 'ios'];
var BROWSERS = ['ie', 'chrome', 'firefox', 'safari', 'opera'];

function _clearCategory(arr, prefix) {
  for(var i=0; i < arr.length; i++) {
    var cb = document.getElementById(prefix + arr[i]);
    cb.checked = false;
  }

  _setCategoryAny(arr, prefix);
}

function _setCategoryAny(arr, prefix) {
  var any = true;
  for(var i=0; i < arr.length; i++) {
    var cb = document.getElementById(prefix + arr[i]);
    if(cb.checked) {
      any = false;
    }
  }
  document.getElementById(prefix + 'any').checked = any;
}

function _getCategory(arr, prefix) {
  var res = [];
  for(var i=0; i < arr.length; i++) {
    var cb = document.getElementById(prefix + arr[i]);
    if(cb.checked) {
      res.push(arr[i]);
    }
  }

  return res.length > 0 ? res : null;
}

window.clearPlatforms = () => { _clearCategory(     PLATFORMS,  "platform_"); }
window.setPlatformAny = () => { _setCategoryAny(    PLATFORMS,  "platform_"); }
window.getPlatforms = ()   => { return _getCategory(PLATFORMS,  "platform_"); }
window.clearBrowsers = ()  => { _clearCategory(     BROWSERS,   "browser_"); }
window.setBrowserAny = ()  => { _setCategoryAny(    BROWSERS,   "browser_"); }
window.getBrowsers = ()    => { return _getCategory(BROWSERS,   "browser_"); }