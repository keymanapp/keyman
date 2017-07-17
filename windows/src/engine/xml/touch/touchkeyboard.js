var device = 'desktop';  // may need to change?
var oskHeight = window.innerHeight - 64;
window.addEventListener('load', init, false);

function init() {
  var kmw=window.tavultesoft.keymanweb;
  kmw.init({'app':device,'fonts':'./'});
  kmw.util.setOption('attachType','manual');
  kmw.ontextchange = textChanged;
  kmw.showKeyboardList = showMenu;
  kmw.hideKeyboard = hideKeyboard;
  kmw.getOskHeight = getOskHeight;
  kmw.setActiveElement('ta');

  setKeymanLanguage('English','us','./languages/us.js');
}

function setDeviceType(deviceType) {
  // Set device type: iPhone|iPad
  device = deviceType;
  init();
}

function setOskHeight(height) {
  oskHeight = height;
}

function getOskHeight() {
  return oskHeight;
}


var keyboardOffset = 0;

function setKeymanLanguage(keyboardName,internalName,path) {
    var languageName=keyboardName,langId='eng', font1='', font2='';

    var kmw=window.tavultesoft.keymanweb;
    
    kmw.KRS({KN:keyboardName,KI:'Keyboard_'+internalName,KLC:langId,KL:languageName,KF:path,KFont:font1,KOskFont:font2});
    kmw.setActiveKeyboard('Keyboard_'+internalName,langId);
    kmw.osk.show(true);
    document.getElementById('debug').innerText = keyboardName+','+internalName+','+path;
}


var fragmentToggle = 0;
function textChanged() {
  var ta = document.getElementById('ta');
  //tavultesoft.keymanweb.debug(ta.value);
  fragmentToggle = (fragmentToggle + 1) % 100;
  window.location.hash = 'updateText-' + fragmentToggle + 'cursorPos' + ta.selectionEnd;
}

function setContext(context) {
  document.getElementById('context').innerText = context;
  document.getElementById('ta').value = context;
}

oskCreatePopup = function(obj,x,y) {};

function showMenu() {
                fragmentToggle = (fragmentToggle + 1) % 100;
                window.location.hash = 'showLanguageList-' + fragmentToggle;
            }
            
            function hideKeyboard() {
                fragmentToggle = (fragmentToggle + 1) % 100;
                window.location.hash = 'hideKeyboard-' + fragmentToggle;
            }
            
            function langMenuPos() {
                var kmw = window['tavultesoft']['keymanweb'];
                var pos = kmw['touchMenuPos']();
                return pos;
            }
                       
            function cursorPos() {
                var ta = document.getElementById('ta');
                return ta.selectionEnd;
            }
            
            function setCursorRange(pos, length) {
                var ta = document.getElementById('ta');
                var kmw = window['tavultesoft']['keymanweb'];
                ta.selectionStart = ta._KeymanWebSelectionStart = pos;
                ta.selectionEnd = ta._KeymanWebSelectionEnd = pos + length;
                kmw['setActiveElement'](ta);
                return ta.selectionEnd;
            }
            
            function setKeymanVal(text) {
                if(undefined == text) text = '';
                var ta = document.getElementById('ta');
                var kmw = window['tavultesoft']['keymanweb'];
                ta.value = text;
                kmw['setActiveElement'](ta);
                return ta.value;
            }
            
            function curKeymanVal() {
                var ta = document.getElementById('ta');
                return ta.value;
            }
            
            function executePopupKey(keyID, keyText) {
                var kmw=window['tavultesoft']['keymanweb'];
                kmw['executePopupKey'](keyID, keyText);
            }
            
            function toHex(theString) {
                var hexString = '';
                for (var i=0; i < theString.length; i++) {
                    var theHex = theString.charCodeAt(i).toString(16).toUpperCase();
                    while (theHex.length < 4) {
                        theHex = '0' + theHex;
                    }
                    theHex = '0x' + theHex;
                    hexString += theHex + ',';
                }
                return hexString.substr(0, hexString.length-1);
            }
//KeymanWeb.KRS({KN:'US',KI:'Keyboard_us',KL:'English',KLC:'eng',KR:'Europe',KRC:'eu',KF:'us.js'});
