var _debug = 0;

// Android harness attachment
if(window.parent && window.parent.jsInterface && !window.jsInterface) {
  window.jsInterface = window.parent.jsInterface;
}

var device = window.jsInterface.getDeviceType();
var oskHeight = Math.ceil(window.jsInterface.getKeyboardHeight() / window.devicePixelRatio);
var oskWidth = 0;
var fragmentToggle = 0;

var sentryManager = new com.keyman.KeymanSentryManager({
  hostPlatform: "android"
});
sentryManager.init();

window.addEventListener('load', init, false);

function loadDefaultKeyboard() {
  notifyHost('reloadAfterError');
}

function init() {
  //document.body.style.backgroundColor="transparent";
  //window.console.log('Device type = '+device);
  //window.console.log('Keyboard height = '+oskHeight);
  keyman.init({'app':device,'fonts':'packages/',root:'./'});
  keyman.util.setOption('attachType','manual');
  keyman.oninserttext = insertText;
  window.showKeyboardList = showMenu;
  window.hideKeyboard = hideKeyboard;
  window.menuKeyUp = menuKeyUp;
  keyman.getOskHeight = getOskHeight;
  keyman.getOskWidth = getOskWidth;
  keyman.beepKeyboard = beepKeyboard;
  var ta = document.getElementById('ta');
  keyman.setActiveElement(ta);

  ta.readOnly = false;
  checkTextArea();

  // Tell KMW the default banner height to use
  com.keyman.osk.Banner.DEFAULT_HEIGHT =
    Math.ceil(window.jsInterface.getDefaultBannerHeight() / window.devicePixelRatio);

  keyman.addEventListener('keyboardloaded', setIsChiral);
  keyman.addEventListener('keyboardchange', setIsChiral);
  keyman.core.languageProcessor.on('statechange', onStateChange);

  document.body.addEventListener('touchend', loadDefaultKeyboard);

  notifyHost('pageLoaded');
}

function notifyHost(event, params) {
  console_debug('notifyHost(event='+event+',params='+params+')');
  // TODO: Update all other host notifications to use notifyHost instead of directly setting window.location.hash
  window.setTimeout(function() {
    // We use a timeout so that the navigation doesn't cause the calling function to abort after the call
    fragmentToggle = (fragmentToggle + 1) % 100;
    params = params ? '+'+params : '';
    window.location.hash = event+'-'+fragmentToggle+params;
  }, 10);
}

// Update the KMW banner height
function setBannerHeight(h) {
  if (h > 0) {
    var osk = keyman.osk;
    osk.banner.height = Math.ceil(h / window.devicePixelRatio);
  }
  // Refresh KMW OSK
  keyman.correctOSKTextSize();
}

function setOskHeight(h) {
  if(h > 0) {
    oskHeight = Math.ceil(h / window.devicePixelRatio);
  }
  if(keyman && keyman.core && keyman.core.activeKeyboard) {
    keyman.core.activeKeyboard.refreshLayouts();
  }
  keyman.correctOSKTextSize();
}

function setOskWidth(w) {
  if(w > 0) {
    oskWidth = w;
  }
}

function getOskHeight() {
  // banner height is calculated completely separately, unlike iOS
  return oskHeight;
}

function getOskWidth() {
  if(oskWidth <= 0) {
    oskWidth = window.jsInterface.getKeyboardWidth();
  }
  //window.console.log('Keyboard width = '+oskWidth);
  return oskWidth;
}

function beepKeyboard() {
  window.jsInterface.beepKeyboard();
}

function onStateChange(change) {
  //window.console.log('onStateChange change: ' + change);

  // Refresh KMW OSK
  keyman.correctOSKTextSize();

  fragmentToggle = (fragmentToggle + 1) % 100;
  if(change != 'configured') { // doesn't change the display; only initiates suggestions.
    window.location.hash = 'refreshBannerHeight-'+fragmentToggle+'+change='+change;
  }
}

// Query KMW if a given keyboard uses chiral modifiers.
function setIsChiral(keyboardProperties) {
  var name = typeof(keyboardProperties.internalName) == 'undefined' ? keyboardProperties.keyboardName : keyboardProperties.internalName;
  var isChiral = keyman.isChiral(name);
  window.jsInterface.setIsChiral(isChiral);
  return true;
}

function setKeymanLanguage(k) {
  KeymanWeb.registerStub(k);
  keyman.setActiveKeyboard(k.KP + '::'+k.KI, k.KLC);
  keyman.osk.show(true);
}

function setSpacebarText(mode) {
  keyman.options['spacebarText'] = mode;
  keyman.osk.show(true);

  // Refresh KMW OSK
  keyman.correctOSKTextSize();
}

// #6665: we need to know when the user has pressed a hardware key so we don't
// generate haptic feedback when text changes are made via insertText. We know
// that the executeHardware->insertText process is a non-reentrant
// single-threaded sequence so this global flag tracking whether we are
// currently executing a hardware keystroke event is safe here. We can't do this
// on the Java side, because the Java<-->JS interface is asynchronous, and we
// cannot reliably know when each hardware keystroke has completed processing.
var executingHardwareKeystroke = false;

/**
 * Inserts the selected string <i>s</i>
 * @param dn  Number of pre-caret code points (UTF+8 characters) to delete
 * @param s   Text to insert
 * @param dr  Number of post-caret code points to delete.  (optional)
 */
function insertText(dn, s, dr) {
  console_debug('insertText(dn='+dn+',s='+s+',dr='+dr+')');
  dr = dr || 0; // Sets a default value of zero when dr is undefined
  //window.console.log('insertText('+ dn +', ' + s +', ' + dr + ');');
  window.jsInterface.insertText(dn, s, dr, executingHardwareKeystroke);
}

function deregisterModel(modelID) {
  keyman.modelManager.deregister(modelID);
}

function enableSuggestions(model, mayPredict, mayCorrect) {
  // Set the options first so that KMW's ModelManager can properly handle model enablement states
  // the moment we actually register the new model.
  keyman.core.languageProcessor.mayPredict = mayPredict;
  keyman.core.languageProcessor.mayCorrect = mayCorrect;

  registerModel(model);
}

function setBannerOptions(mayPredict) {
  keyman.osk.banner.setOptions({
    'mayPredict': mayPredict
  });
}

function registerModel(model) {
  //window.console.log('registerModel: ' + model);
  keyman.addModel(model);
}

function resetContext() {
  keyman.resetContext();
}

// Tell KMW to switch to "numeric" layer
function setNumericLayer() {
  if (keyman && keyman.core && keyman.core.activeKeyboard) {
    keyman.setNumericLayer();
  }
}

function updateKMText(text) {
  var ta = document.getElementById('ta');
  console_debug('updateKMText(text='+text+') ta.value='+ta.value);

  if(text == undefined) {
      text = '';
  }

  if(ta.value != text) {
    ta.value = text;
    window.resetContext();
  }
}

function console_debug(s) {
  if(_debug) {
    console.debug(s);
  }
}

function updateKMSelectionRange(start, end) {
  var ta = document.getElementById('ta');
  console_debug('updateKMSelectionRange('+start+','+end+'): ta.selectionStart='+ta.selectionStart+' '+
    '['+ta._KeymanWebSelectionStart+'] ta.selectionEnd='+ta.selectionEnd+' '+ta._KeymanWebSelectionEnd);

  var selDirection = 'forward';
  if(start > end) {
    var e0 = end;
    end = start;
    start = e0;
    selDirection = 'backward';
  }

  if(ta.selectionStart != start || ta.selectionEnd != end || ta.selectionDirection != selDirection) {
    ta.selectionStart = ta._KeymanWebSelectionStart = start;
    ta.selectionEnd = ta._KeymanWebSelectionEnd = end;
    ta.selectionDirection = selDirection;
    keyman.resetContext();
  }
}

var lastKeyTip = null;
function oskCreateKeyPreview(x,y,w,h,t) {
  if(lastKeyTip &&
      lastKeyTip.t == t &&
      lastKeyTip.x == x &&
      lastKeyTip.y == y &&
      lastKeyTip.w == w &&
      lastKeyTip.h == h) {
    return;
    }
  lastKeyTip = {x:x,y:y,w:w,h:h,t:t};

  fragmentToggle = (fragmentToggle + 1) % 100;
  var div = document.createElement('div');
  div.innerHTML = t;
  var dt = div.firstChild.nodeValue;
  window.location.hash = 'showKeyPreview-'+fragmentToggle+'+x='+x+'+y='+y+'+w='+w+'+h='+h+'+t='+toHex(dt);
}

function oskClearKeyPreview() {
  lastKeyTip = null;
  fragmentToggle = (fragmentToggle + 1) % 100;
  window.location.hash = 'dismissKeyPreview-'+fragmentToggle;
}

function signalHelpBubbleDismissal() {
  fragmentToggle = (fragmentToggle + 1) % 100;
  window.location.hash = 'helpBubbleDismissed-'+fragmentToggle;
}

function oskCreatePopup(obj,x,y) {
  if(obj != null) {
    var i;
    var s = '';
    var shift = false;
    var keyPos = x.toString() + ',' + y.toString();
    for(i=0; i<obj.length; i++)
    {
      // elementID contains the layer and coreID
      s=s+obj[i].elementID;
      if(obj[i].sp == 1 || obj[i].sp == 2) shift = true;
      if(typeof(obj[i].text) != 'undefined' && obj[i].text != null && obj[i].text != '') s=s+':'+toHex(obj[i].text);
      if(i < (obj.length -1)) s=s+';'
    }
    fragmentToggle=(fragmentToggle+1) % 100;
    var hash = 'showMore-' + fragmentToggle + '+keyPos=' + keyPos + '+keys=' + s;
    if(shift) {
      hash = hash + '+font=' + 'SpecialOSK';
    }
    window.location.hash = hash;
  }
}

function suggestionPopup(obj,custom,x,y,w,h) {
  if(obj != null) {
    var s = JSON.stringify(obj);

    fragmentToggle=(fragmentToggle+1) % 100;
    var hash = 'suggestPopup-' + fragmentToggle;

    hash = hash + '&x=' + encodeURIComponent(x) + '&y=' + encodeURIComponent(y);
    hash = hash + '&w=' + encodeURIComponent(w) + '&h=' + encodeURIComponent(h);
    hash = hash + '&suggestion=' + encodeURIComponent(s) + '&custom=' + encodeURIComponent(custom);

    window.location.hash = hash;
  }
}

// legacy function name for globe key down
function showMenu() {
  fragmentToggle = (fragmentToggle + 1) % 100;
  var hash = 'globeKeyAction&fragmentToggle=' + fragmentToggle + '&keydown=true';
  window.location.hash = hash;
}

// legacy function name for globe key up
function menuKeyUp() {
  fragmentToggle = (fragmentToggle + 1) % 100;
  var hash = 'globeKeyAction&fragmentToggle=' + fragmentToggle + '&keydown=false';
  window.location.hash = hash;
}

function hideKeyboard() {
  fragmentToggle = (fragmentToggle + 1) % 100;
  window.location.hash = 'hideKeyboard' + fragmentToggle;
}

function showKeyboard() {
  // Refresh KMW OSK
  keyman.correctOSKTextSize();
}

function executePopupKey(keyID, keyText) {
  // KMW only needs keyID to process the popup key. keyText merely logged to console
  //window.console.log('executePopupKey('+keyID+'); keyText: ' + keyText);
  keyman.executePopupKey(keyID);
}

function executeHardwareKeystroke(code, shift, lstates, eventModifiers) {
  console_debug('executeHardwareKeystroke(code='+code+',shift='+shift+',lstates='+lstates+',eventModifiers='+eventModifiers+')');
  try {
    executingHardwareKeystroke = true;
    if (keyman.executeHardwareKeystroke(code, shift, lstates)) { // false if matched, true if not
      // KMW didn't process the key, so have the Android app dispatch the key with the original event modifiers
      window.jsInterface.dispatchKey(code, eventModifiers);
    }
    executingHardwareKeystroke = false;
  } catch(e) {
    window.console.log('executeHardwareKeystroke exception: '+e);
    executingHardwareKeystroke = false;
  }
}

function popupVisible(value) {
  keyman.popupVisible(value);
}

function toHex(theString) {
  if (typeof theString != 'string') {
    return '';
  }
  var hexString = '';
  for (var i=0; i < theString.length; i++) {
    var theHex = theString.charCodeAt(i).toString(16).toUpperCase();
    while (theHex.length < 4) {
      theHex = '0' + theHex;
    }
    theHex = '\\u' + theHex;
    hexString += theHex;
  }
  return hexString;
}

/**
 * Check the WebView version and determine if the textarea that KeymanWeb uses needs to be "visible".
 * Normally, this textarea is not displayed to avoid redundant layout calculations.
 * In older WebViews on Android 5.0 though, selectionStart and selectionEnd positions fail to
 * update unless the textarea is visible.
 * Reference: Issue #5376
 */
function checkTextArea() {
  var uaRe = /Chrome\/([0-9]*)\./g;
  var chromeMajorVersion = uaRe.exec(navigator.userAgent);
  if (chromeMajorVersion && parseInt(chromeMajorVersion[1]) <= 37) {
    var ta = document.getElementById('ta');
    if (ta != null) {
      ta.style.display = '';
      ta.style.position = 'absolute';
      ta.style.left = '-500px';
      ta.style.top = '0px';
    }
  }
}
