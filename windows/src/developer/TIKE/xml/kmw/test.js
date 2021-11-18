const keyboardSelect = document.getElementById('keyboard-select');
const ta1 = document.getElementById('ta1');
const charGrid = document.getElementById('character-grid');

keyman.addEventListener('keyboardregistered', function(keyboardProperties) {
  console.log('keyboardregistered:'+JSON.stringify(keyboardProperties)+' [active='+keyman.getActiveKeyboard()+';'+keyman.core.activeKeyboard+']');
  buildKeyboardList();
});

keyman.addEventListener('keyboardloaded', function(keyboardProperties) {
  // Note: see #5730 for why we have a timeout here
  //console.log('keyboardloaded:'+JSON.stringify(keyboardProperties)+' [active='+keyman.getActiveKeyboard()+';'+keyman.core.activeKeyboard+']');
  //buildKeyboardList();
  window.setTimeout(function() {
    //console.log('keyboardloaded.timeout:'+JSON.stringify(keyboardProperties)+' [active='+keyman.getActiveKeyboard()+';'+keyman.core.activeKeyboard+']');
    buildKeyboardList();
  }, 10);
});

keyman.init({
  ui:'button',
  resources:'/resource/',
  keyboards:'/keyboard/',
  fonts:'/font/',
  attachType:'auto'
});

buildKeyboardList();

function buildKeyboardList() {
  let keyboards = keyman.getKeyboards();
  keyboardSelect.innerHTML = ''; // In the future perhaps we can use: keyboardSelect.replaceChildren();

  let opt = document.createElement('option');
  opt.value = '';
  opt.innerText = '(system keyboard)';
  keyboardSelect.appendChild(opt);
  for(let keyboard of keyboards) {
    let opt = document.createElement('option');
    opt.value = keyboard.InternalName;
    opt.innerText = keyboard.Name;
    keyboardSelect.appendChild(opt);
  }
  keyboardSelect.value = keyman.getActiveKeyboard();
}

var lastContent = null;

if(keyman.util.isTouchDevice()) {
  document.body.className += ' touch-device';
  if(keyman.util.device.OS == 'iOS') {
    document.body.className += ' touch-device-ios';
  } else if(keyman.util.device.OS == 'Android') {
    document.body.className += ' touch-device-android';
  }
} else {
  document.body.className += ' desktop-device';
}

function removeChildNodes(node) {
  while (node.lastChild) {
    node.removeChild(node.lastChild);
  }
}

function addCharElements(text, code) {
  var ebox = document.createElement('div'), echar = document.createElement('div'), ecode = document.createElement('div');
  echar.textContent = text;
  echar.className = 'char-char keymanweb-font';
  ecode.textContent = code;
  ecode.className = 'char-code';
  ebox.appendChild(echar);
  ebox.appendChild(ecode);
  charGrid.appendChild(ebox);
}

function logContent() {
  if(lastContent === ta1.value) {
    updateLogCursor();
    return;
  }
  removeChildNodes(charGrid);
  if(ta1.value.length == 0) {
    addCharElements('-','empty');
  } else {
    for(var i = 0; i < ta1.value.length; i++) {
      //
      var code = ta1.value.charCodeAt(i);
      var text = ta1.value.charAt(i);
      var slice = 4;
      // Test for SMP
      if(code >= 0xD800 && code < 0xDC00) {
        if(i < ta1.value.length) {
          var code2 = ta1.value.charCodeAt(i+1);
          if(code2 >= 0xDC00 && code < 0xE000) {
            code = (code - 0xD800) * 0x400 + (code2 - 0xDC00) + 0x10000;
            text += ta1.value.charAt(i+1);
            slice = 6;
            i++;
          }
        }
      }
      addCharElements(text, ('000000'+(code).toString(16)).slice(-slice));
    }
  }
  updateLogCursor();
  lastContent = ta1.value;
}

var lastSelStart = -1;

function calculateLengthByCodepoint(text, base, x)  {
  var stop = base + x;
  while(base < stop - 1) {
    if(text.charCodeAt(base) >= 0xD800 && text.charCodeAt(base) < 0xDC00 &&
       text.charCodeAt(base+1) >= 0xDC00 && text.charCodeAt(base+1) < 0xE000) {
      // Decrement position by one for each surrogate pair
      x--;
    }
    base++;
  }
  return x;
}

function updateLogCursor() {
  var i, selStart, selLength, selDirection;

  if(keyman.isPositionSynthesized()) { // this is an internal function
    // For touch devices, we need to ask KMW
    selStart = ta1.kmw_ip ? ta1.kmw_ip.getTextBeforeCaret().length : 0;
    selLength = 0;
    selDirection = 'forward';
  } else {
    // For desktop devices, we use the position reported by the textarea control
    selStart = ta1.selectionStart;
    selLength = ta1.selectionEnd - ta1.selectionStart;
    selDirection = ta1.selectionDirection;
  }

  selLength = calculateLengthByCodepoint(ta1.value, selStart, selLength);
  selStart = calculateLengthByCodepoint(ta1.value, 0, selStart);

  //console.log('selStart='+selStart+', selLength='+selLength);
  if(lastSelStart != selStart || lastSelLength != selLength) {
    for(i = 0; i < charGrid.childNodes.length; i++) {
      charGrid.childNodes[i].className = '';
    }

    var x = selDirection == 'backward' ? selStart-1 : selStart+selLength - 1;

    if(x < 0) {
      charGrid.className = 'cursor';
    } else {
      charGrid.className = '';
      if(x >= 0 && x < charGrid.childNodes.length) {
        charGrid.childNodes[x].className = 'cursor';
      }
      charGrid.childNodes[x].scrollIntoView();
    }

    for(i = selStart; i < selStart+selLength; i++) {
      charGrid.childNodes[i].className += ' cursor-selected';
    }
    lastSelStart = selStart;
    lastSelLength = selLength;
  }
}

logContent();
window.setInterval(logContent, 100);

/* TODO: once KeymanWeb supports oninput signalling
document.getElementById('ta1').addEventListener('input', logContent, false);
*/

window.onload = function() {
  window.setTimeout(
    function () {
      //document.getElementById('ta1').focus();
      keyman.moveToElement('ta1');
    }, 10
  );

  let newOSK = null;
  let deviceSelect = document.getElementById('device-select');
  if(deviceSelect.value == '') deviceSelect.value = 'desktop';

  deviceSelect.addEventListener('change', function() {
    setOSK();
    ta1.focus();
  });


  function setOSK() {
    const devices = {
      Windows:         { browser: 'chrome', formFactor: 'desktop', OS: 'windows', touchable: false, dimensions: [640, 300] },
      macOS:           { browser: 'chrome', formFactor: 'desktop', OS: 'macosx',  touchable: false, dimensions: [640, 300] },
      Linux:           { browser: 'chrome', formFactor: 'desktop', OS: 'linux',   touchable: false, dimensions: [640, 300] },
      iPhone:          { browser: 'chrome', formFactor: 'phone',   OS: 'ios',     touchable: true,  dimensions: [527, 280] },
      iPadMini:        { browser: 'chrome', formFactor: 'tablet',  OS: 'ios',     touchable: true,  dimensions: [829, 300] },
      SamsungGalaxyS5: { browser: 'chrome', formFactor: 'phone',   OS: 'android', touchable: true,  dimensions: [520, 270] },
      SamsungTablet:   { browser: 'chrome', formFactor: 'tablet',  OS: 'android', touchable: true,  dimensions: [640, 300] },
    };

    const targetDevice = devices[deviceSelect.value] || devices.Windows;

    document.getElementById('osk-host-frame').className = deviceSelect.value;

    if(newOSK) {
      document.getElementById('osk-host').removeChild(newOSK.element);
      keyman.osk = null;
    }

    newOSK = new com.keyman.osk.InlinedOSKView(targetDevice, keyman.util.device.coreSpec);

    if(document.body.offsetWidth < targetDevice.dimensions[0]) {
      newOSK.setSize('320px', '200px');
    } else {
      newOSK.setSize(targetDevice.dimensions[0]+'px', targetDevice.dimensions[1]+'px');
    }
    document.getElementById('osk-host').appendChild(newOSK.element);

    keyman.osk = newOSK;
    newOSK.activeKeyboard = keyman.core.activeKeyboard;
    keyman.alignInputs();
  }

  setOSK();

  keyman.addEventListener('keyboardchange', function(keyboardProperties) {
    keyman.osk = newOSK;
    newOSK.activeKeyboard = keyman.core.activeKeyboard;
    keyboardSelect.value = keyboardProperties.internalName;
    keyman.alignInputs();
    //console.log('keyboardchange:'+JSON.stringify(keyboardProperties)+' [active='+keyman.getActiveKeyboard()+';'+keyman.core.activeKeyboard+']');
  });
}

/* Lexical models */

var firstModel = true;
var modelList = document.getElementById('model-list');

/**
 * Register a model for debugging. Called by keyboards.js. The
 * first model registered will be activated automatically.
 */
function registerModel(model, src) {
  var modelList = document.getElementById('model-list');
  var opt = document.createElement('option');
  opt.innerText = model;
  opt.value = model;
  opt['data-src'] = src;
  modelList.appendChild(opt);
}

keyboardSelect.addEventListener('change', function() {
  keyman.setActiveKeyboard(keyboardSelect.value);
  ta1.focus();
});

modelList.addEventListener('change', function() {
  let model = modelList.value;
  if(modelList.selectedOptions.length == 0) return;
  let opt = modelList.selectedOptions[0];


  let lastModel = keyman.core.activeModel;
  if(lastModel) {
    keyman.modelManager.deregister(lastModel.id);
  }
  if(model != '') {
    keyman.modelManager.register({
      id: model,
      languages: ['en'],
      path: 'http://'+location.host+'/model/'+opt['data-src']
    });
  }
});
