var UNIT_TEST_FOLDER_RELATIVE_PATH = "../../unit_tests";

var ta_inputJSON;
var in_output;
var recorderScribe;

function focusReceiver() {
  var receiver = document.getElementById('receiver');
  if(receiver['kmw_ip']) {
    receiver = receiver['kmw_ip'];
  }
  receiver.focus();
  
  if(keyman.util.device.touchable) {
    // At present, touch doesn't 'focus' properly.
    keyman.setActiveElement(receiver);
    keyman.osk.show(true);
  }
}

setElementText = function(ele, text) {
  ele.value = text;
  if(ele['kmw_ip']) {
    ele['kmw_ip'].setTextBeforeCaret(ele.value);
  }
}

onUpdateInputRecord = function(json) {
  setElementText(ta_inputJSON, json);
}

onResetInputRecord = function() {
  setElementText(ta_inputJSON, "");
  setElementText(in_output, "");
  setElementText(document.getElementById('errorText'), "");
}

resetInputRecord = function() {
  recorderScribe.resetInputRecord();
}

copyInputRecord = function() {
  try {
    if(!ta_inputJSON['kmw_ip']) {
      ta_inputJSON.select();
    } else {
      var range = document.createRange();
      range.selectNode(ta_inputJSON['kmw_ip']);
      window.getSelection().removeAllRanges();
      window.getSelection().addRange(range);
    }
    
    var res = document.execCommand('copy');
    if(res) {
      in_output.focus();
      return;
    }
  } catch (err) { console.log(err) }
  alert("Unable to copy successfully.");
}

function saveInputRecord() {
  var target;
  if(recorderScribe.inputJSON.hasOSKInteraction()) {
    var device = new com.keyman.Device();
    device.detect();
    target = device.formFactor;
  } else {
    target = 'hardware';
  }
  var os_list = getPlatforms();
  var browsers = getBrowsers();
  var config = new KMWRecorder.Constraint(target, os_list, browsers);

  recorderScribe.saveInputRecord(config);
}

reviseInputRecord = function() {
  let inputJSON = new KMWRecorder.InputTestSequence(JSON.parse(ta_inputJSON.value));
  recorderScribe.setInputRecord(inputJSON);
}

onTestDefinitionChanged = function(testDefJSON) {
  var masterJSON = document.getElementById('masterJSON');
  masterJSON.value = testDefJSON;
}

setTestDefinition = function(testDef) {
  recorderScribe.setTestDefinition(testDef);
}

copyTestDefinition = function() {
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

onKeyboardChanged = function(kbdProperties) {
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

function errorUpdate() {
  var errorInput = document.getElementById('errorText');
  recorderScribe.errorUpdate(errorInput.value);
}

var initDevice = function() {
  // From KMW.
  var device = new com.keyman.Device();
  device.detect();

  document.getElementById("activeFormFactor").textContent = device.formFactor;
  document.getElementById("activeTouch").textContent = device.touchable ? 'Supported' : 'None';
  document.getElementById("activeOS").textContent = device.OS;
  document.getElementById("activeBrowser").textContent = device.browser;
}

window.addEventListener('load', function() {
  ta_inputJSON = document.getElementById('inputRecord');
  in_output = document.getElementById('receiver');

  // Set up Scribe methods
  recorderScribe = new KMWRecorder.Scribe();
  recorderScribe.initHooks(in_output);
  recorderScribe.on('record-changed', onUpdateInputRecord);
  recorderScribe.on('record-reset', onResetInputRecord);
  recorderScribe.on('test-changed', onTestDefinitionChanged);

  keyman.attachToControl(in_output);
  keyman.setKeyboardForControl(in_output, '', '');
  keyman.addEventListener('keyboardchange', onKeyboardChanged);
  onResetInputRecord();
  initDevice();
  setupKeyboardPicker();
  recorderScribe.setTestDefinition();

  var errorInput = document.getElementById('errorText');
  if(errorInput['kmw_ip']) {
    // Alias DIVs use subelements b/c caret simulation.
    // Interestingly, 'childList' is the most important for noting textContent changes.
    var config = { childList: true, subtree: true };
    var observer = new MutationObserver(function(mutations) {
      errorUpdate();
    });

    observer.observe(errorInput['kmw_ip'], config);
  }
});

//var p={'internalName':_internalName,'language':_language,'keyboardName':_keyboardName,'languageCode':_languageCode};
function keyboardAdded(properties) {
  var kbdControl = document.getElementById('KMW_Keyboard');

  var opt = document.createElement('OPTION');
  opt.value = properties.internalName + "$$" + properties.languageCode;
  opt.innerHTML = properties.keyboardName + " (" + properties.language + ")";
  kbdControl.appendChild(opt);
}

function setupKeyboardPicker() {
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

function doKeyboardChange(name, languageCode) {
  var activeElement = document.activeElement;
  if(!activeElement) {
    activeElement = in_output;
  }
  recorderScribe.keyboardJustActivated = true;
  focusReceiver();
  keyman.setActiveKeyboard(name, languageCode);
  activeElement.focus();
}

/* Called when user selects an item in the KMW_Keyboard SELECT */
function KMW_KeyboardChange() {
  var kbdControl = document.getElementById('KMW_Keyboard');
  /* Select the keyboard in KeymanWeb */
  var name = kbdControl.value.substr(0, kbdControl.value.indexOf("$$"));
  var languageCode = kbdControl.value.substr(kbdControl.value.indexOf("$$")+2);

  doKeyboardChange(name, languageCode);
}

function loadExistingTest(files) {
  if(files.length > 0) {
    var reader = new FileReader();
    reader.onload = function() {
      try {
        var kbdTest = new KMWRecorder.KeyboardTest(reader.result);
        recorderScribe.setTestDefinition(kbdTest)

        // Make sure we've loaded the keyboard!  Problem - we're not running from the unit_tests folder!
        var kbdStub = new KMWRecorder.KeyboardStub(kbdTest.keyboard);
        kbdStub.filename = UNIT_TEST_FOLDER_RELATIVE_PATH + "/" + kbdStub.filename;

        keyman.addKeyboards(kbdStub);

        doKeyboardChange("Keyboard_" + kbdTest.keyboard.id, kbdTest.keyboard.getFirstLanguage());
      } catch (e) {
        alert("File does not contain a valid KeyboardTest definition.")
        console.error(e);
      }
    }
    reader.readAsText(files[0]);
  }
}

function loadExistingStubs(files) {
  
  var processStub = function(json, file) {
    try {
      // Load the stub
      var kbdStub = new KMWRecorder.KeyboardStub(JSON.parse(json));
      kbdStub.filename = UNIT_TEST_FOLDER_RELATIVE_PATH + "/" + kbdStub.filename;

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

function clearPlatforms() { _clearCategory(     PLATFORMS,  "platform_"); }
function setPlatformAny() { _setCategoryAny(    PLATFORMS,  "platform_"); }
function getPlatforms()   { return _getCategory(PLATFORMS,  "platform_"); }
function clearBrowsers()  { _clearCategory(     BROWSERS,   "browser_"); }
function setBrowserAny()  { _setCategoryAny(    BROWSERS,   "browser_"); }
function getBrowsers()    { return _getCategory(BROWSERS,   "browser_"); }