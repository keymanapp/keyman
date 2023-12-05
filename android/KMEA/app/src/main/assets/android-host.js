var _debug = 0;

// Android harness attachment
if(window.parent && window.parent.jsInterface && !window.jsInterface) {
  window.jsInterface = window.parent.jsInterface;
}

var device = window.jsInterface.getDeviceType();
var oskHeight = Math.ceil(window.jsInterface.getKeyboardHeight() / window.devicePixelRatio);
var oskWidth = 0;
var bannerHeight = 0;
var bannerImagePath = '';
var bannerHTMLContents = '';
var fragmentToggle = 0;

var sentryManager = new KeymanSentryManager({
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
  keyman.showKeyboardList = showMenu;
  keyman.hideKeyboard = hideKeyboard;
  keyman.menuKeyUp = menuKeyUp;
  keyman.getOskHeight = getOskHeight;
  keyman.getOskWidth = getOskWidth;
  keyman.beepKeyboard = beepKeyboard;

  // Readies the keyboard stub for instant loading during the init process.
  KeymanWeb.registerStub(JSON.parse(jsInterface.initialKeyboard()));

  keyman.init({
    'embeddingApp':device,
    'fonts':'packages/',
    oninserttext: insertText,
    root:'./'
  }).then(function () {  // Note:  For non-upgraded API 21, arrow functions will break the keyboard!
    bannerHeight = Math.ceil(window.jsInterface.getDefaultBannerHeight() / window.devicePixelRatio);
    if (bannerHeight > 0) {

      // The OSK is not available until initialization is complete.
      keyman.osk.bannerView.activeBannerHeight = bannerHeight;
      keyman.refreshOskLayout();
    }
  });

  keyman.addEventListener('keyboardloaded', setIsChiral);
  keyman.addEventListener('keyboardchange', setIsChiral);
  keyman.core.languageProcessor.on('statechange', onStateChange);

  document.body.addEventListener('touchend', loadDefaultKeyboard);

  notifyHost('pageLoaded');
}

function showBanner(flag) {
  console_debug("Setting banner display for dictionaryless keyboards to " + flag);
  console_debug("bannerHTMLContents: " + bannerHTMLContents);
  var bc = keyman.osk.bannerController;
  if (bc) {
    if (bannerHTMLContents != '') {
      bc.inactiveBanner = flag ? new bc.HTMLBanner(bannerHTMLContents) : null;
    } else {
      bc.inactiveBanner = flag ? new bc.ImageBanner(bannerImagePath) : null;
    }
  }
}

function setBannerImage(path) {
  bannerImagePath = path;
}

// Set the HTML banner to use when predictive-text is not available
// contents - HTML content to use for the banner
function setBannerHTML(contents) {
  bannerHTMLContents = contents;
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
// h is in dpi (different from iOS)
function setBannerHeight(h) {
  if (h > 0) {
    // The banner itself may not be loaded yet.  This will preemptively help set
    // its eventual display height.
    bannerHeight = Math.ceil(h / window.devicePixelRatio);

    if (keyman.osk) {
      keyman.osk.bannerView.activeBannerHeight = bannerHeight;
    }
  }

  // Refresh KMW's OSK
  keyman.refreshOskLayout();
}

function setOskHeight(h) {
  if(h > 0) {
    oskHeight = Math.ceil(h / window.devicePixelRatio);
  }
  if(keyman && keyman.core && keyman.core.activeKeyboard) {
    keyman.core.activeKeyboard.refreshLayouts();
  }
  keyman.refreshOskLayout();
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
  keyman.refreshOskLayout();

  fragmentToggle = (fragmentToggle + 1) % 100;
  if(change != 'configured') {
    window.location.hash = 'refreshBannerHeight-'+fragmentToggle;
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
  keyman.setActiveKeyboard(k.KI, k.KLC);
}

function setSpacebarText(mode) {
  keyman.config.spacebarText = mode;
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
  keyman.removeModel(modelID);
}

function enableSuggestions(model, mayPredict, mayCorrect) {
  // Set the options first so that KMW's ModelManager can properly handle model enablement states
  // the moment we actually register the new model.
  keyman.core.languageProcessor.mayPredict = mayPredict;
  keyman.core.languageProcessor.mayCorrect = mayCorrect;

  registerModel(model);
}

function setBannerOptions(mayPredict) {
  keyman.core.languageProcessor.mayPredict = mayPredict;
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
  if(text == undefined) {
      text = '';
  }

  console_debug('updateKMText(text='+text+') context.value='+keyman.context.getText());

  if(text != keyman.context.getText()) {
    keyman.context.setText(text);
    keyman.resetContext();
  }
}

function console_debug(s) {
  if(_debug) {
    console.debug(s);
  }
}

function updateKMSelectionRange(start, end) {
  var context = keyman.context;

  // console_debug('updateKMSelectionRange('+start+','+end+'): context.selStart='+ta.selectionStart+' '+
  //   '['+ta._KeymanWebSelectionStart+'] context.selEnd='+ta.selectionEnd+' '+ta._KeymanWebSelectionEnd);

  if(start > end) {
    var e0 = end;
    end = start;
    start = e0;
  }

  if(context.selStart != start || context.selEnd != end) {
    keyman.context.setSelection(start, end);
    keyman.resetContext();
  }
}

var lastKeyTip = null;

function signalHelpBubbleDismissal() {
  fragmentToggle = (fragmentToggle + 1) % 100;
  window.location.hash = 'helpBubbleDismissed-'+fragmentToggle;
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
  keyman.refreshOskLayout();
}

// Cannot make it explicitly async / await on API 21.
function executeHardwareKeystroke(code, shift, lstates, eventModifiers) {
  console_debug('executeHardwareKeystroke(code='+code+',shift='+shift+',lstates='+lstates+',eventModifiers='+eventModifiers+')');

  executingHardwareKeystroke = true;

  // Would be cleaner if we could async / await here, which would give us a simple try-catch implementation.
  var promise = keyman.hardKeyboard.raiseKeyEvent(code, shift, lstates);
  promise.then(function (result) {
    if(result) { // false if matched, true if not
      // KMW didn't process the key, so have the Android app dispatch the key with the original event modifiers
      window.jsInterface.dispatchKey(code, eventModifiers);
      executingHardwareKeystroke = false;
    }
  }).catch(function (e) {
    window.console.log('executeHardwareKeystroke exception: '+e);
    executingHardwareKeystroke = false;
  });
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
