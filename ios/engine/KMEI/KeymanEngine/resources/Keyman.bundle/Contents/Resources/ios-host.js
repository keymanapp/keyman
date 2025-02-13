let _debug = false;

if(_debug) {
    console = new Object();
    console.log = function(log) {
        var iframe = document.createElement("IFRAME");
        iframe.setAttribute("src", "ios-log:#iOS#" + log);
        document.documentElement.appendChild(iframe);
        iframe.parentNode.removeChild(iframe);
        iframe = null;
        if (typeof(window.webkit) != 'undefined') {
            window.webkit.messageHandlers.keyman.postMessage("ios-log:#iOS#" + log);
        }
    };
    console.debug = console.log;
    console.info = console.log;
    console.warn = console.log;
    console.error = console.log;
    window.onerror = function(error, url, line) {
        console.log('ERROR: '+error+' URL:'+url+' L:'+line);
    };
}

var oskHeight = 0;
var oskWidth = 0;
var bannerHeight = 0;
var bannerImgPath = '';

var sentryManager = new KeymanSentryManager({
    hostPlatform: "ios"
});

sentryManager.init();

window.addEventListener('load', init, false);

function init() {
    const device = navigator.userAgent.match(/iPad|Macintosh/) ? 'AppleTablet' : 'AppleMobile';

    // As of iOS 15, Safari WebViews will try to avoid letting us use the "safe area"
    // at the bottom of iPhone X style devices.  While `-webkit-fill-available` will partly
    // counteract this... it's only a "partly".  Fortunately, we can manually force the
    // page to our desired size.
    document.body.style.height = window.outerHeight;

    var kmw=window['keyman'];
    kmw['showKeyboardList'] = menuKeyDown;
    kmw['menuKeyUp'] = menuKeyUp;
    kmw['hideKeyboard'] = hideKeyboard;
    kmw['getOskHeight'] = getOskHeight;
    kmw['getOskWidth'] = getOskWidth;
    kmw['beepKeyboard'] = beepKeyboard;
    kmw.init({
        'embeddingApp':device,
        'fonts':'fonts/',
        oninserttext: insertText
    }).then(function() {
      if(bannerHeight > 0) {
        // The OSK is not available until initialization is complete.
        kmw.osk.bannerView.activeBannerHeight = bannerHeight;
        keyman.refreshOskLayout();
      }

      var bc = keyman.osk.bannerController;
      if(bannerImgPath) {
        bc.inactiveBanner = new bc.ImageBanner(bannerImgPath);
      }
    });
}

function verifyLoaded() {
    // During proper loads of KMW, a keyboard will be set very early on.
    // There's a chance that the keyboard is still loading, so we double-check
    // against the stub count.
    if(!keyman.core.activeKeyboard && !keyman.keyboardRequisitioner.cache.defaultStub) {
    location.reload();
    }
}

function setBannerImage(path) {
    bannerImgPath = path;

    var bc = keyman && keyman.osk && keyman.osk.bannerController;
    if(!bc) {
      return;
    }

    // If an inactive banner is set, update its image.
    bc.inactiveBanner = new bc.ImageBanner(bannerImgPath);
}

function setBannerHeight(h) {
  if(h >= 0) {
    // The banner itself may not be loaded yet.  This will preemptively help set
    // its eventual display height.
    bannerHeight = h;

    if(keyman.osk) {
        keyman.osk.bannerView.activeBannerHeight = h;
    }
  }

  // Refresh KMW's OSK
  keyman.refreshOskLayout();
  doResetContext();
}

function setOskHeight(height) {
    var kmw=window['keyman'];
    oskHeight = height;
    if(kmw && kmw.core && kmw.core.activeKeyboard) {
        kmw.core.activeKeyboard.refreshLayouts();
    }
    kmw.refreshOskLayout();
    doResetContext();
}

function setOskWidth(width) {
    oskWidth = width;
}

function getOskWidth() {
    if (oskWidth == 0)
        oskWidth = Math.abs(window.orientation) == 90 ? screen.height : screen.width;

    if (Math.abs(window.orientation) == 90) document.body.className="kmw-embedded keyman-app kmw-landscape";
    else document.body.className="kmw-embedded keyman-app kmw-portrait";

    return oskWidth;
}

function getOskHeight() {
    var height = oskHeight;
    if(keyman.osk && keyman.osk.banner && keyman.osk.banner._activeType != 'blank') {
        height = height - keyman.osk.banner.height;
    }
    return height;
}

var keyboardOffset = 0;
function setKeymanLanguage(stub) {
    var kmw = window.keyman;

    KeymanWeb.registerStub(stub);

    kmw.setActiveKeyboard(stub.KI, stub.KLC).then(function() {
        keyman.refreshOskLayout();
        doResetContext();
    });
}

function setHostAppName(name) {
    // Sentry will capture this in a breadcrumb for Web errors.
    keyman.config.embeddingApp = name;
}

var fragmentToggle = 0;
/**
 * Inserts the selected string <i>s</i>
 * @param dn  Number of pre-caret code points (UTF+8 characters) to delete
 * @param s   Text to insert
 * @param dr  Number of post-caret code points to delete.  (optional)
 */
function insertText(dn, s, dr) {
    dr = dr || 0;  // Sets a default value of zero when dr is undefined
    fragmentToggle = (fragmentToggle + 1) % 100;
    var insertHash = 'insertText-'+ fragmentToggle + '+dn=' + dn + '+s=' + toHex(s) + '+dr=' + dr;
    if (typeof(window.webkit) != 'undefined')
        window.webkit.messageHandlers.keyman.postMessage('#' + insertHash);
}

function beepKeyboard() {
    fragmentToggle = (fragmentToggle + 1) % 100;
    var beepHash = 'beep-'+ fragmentToggle;
    if(typeof(window.webkit) != 'undefined') {
        window.webkit.messageHandlers.keyman.postMessage('#' + beepHash);
    }
}

function oskCreateKeyPreview(x,y,w,h,t)
{
    fragmentToggle = (fragmentToggle + 1) % 100;
    var div = document.createElement('div');
    div.innerHTML = t;
    var dt = div.firstChild.nodeValue;
    var previewHash = 'showKeyPreview-'+fragmentToggle+'+x='+x+'+y='+y+'+w='+w+'+h='+h+'+t='+toHex(dt);
    //window.location.hash = 'showKeyPreview-'+fragmentToggle+'+x='+x+'+y='+y+'+w='+w+'+h='+h+'+t='+toHex(dt);
    if (typeof(window.webkit) != 'undefined')
        window.webkit.messageHandlers.keyman.postMessage('#' + previewHash);
}

function oskClearKeyPreview()
{
    fragmentToggle = (fragmentToggle + 1) % 100;
    var dismissHash = 'dismissKeyPreview-'+fragmentToggle;
    //window.location.hash = 'dismissKeyPreview-'+fragmentToggle;
    if (typeof(window.webkit) != 'undefined')
        window.webkit.messageHandlers.keyman.postMessage('#' + dismissHash);
}

oskCreatePopup = function(obj,x,y,w,h)
{
    if(obj != null) {
        var i;
        var s = '';
        var shift = false;
        var frame = x.toString() + ',' + y.toString() + ',' + w.toString() + ',' + h.toString();
        for(i=0; i<obj.length; i++)
        {
            s=s+obj[i].layer+'-'+obj[i].id;
            if(obj[i].sp == 1 || obj[i].sp == 2) shift = true;
            if(typeof(obj[i].text) != 'undefined' && obj[i].text != '') s=s+':'+toHex(obj[i].text);
            if(i < (obj.length -1)) s=s+';'
        }
        fragmentToggle=(fragmentToggle+1) % 100;
        var hash = 'showMore-' + fragmentToggle + '+baseFrame=' + frame + '+keys=' + s;
        if(shift) {
            hash = hash + '+font=' + 'SpecialOSK';
        }
        //window.location.hash = hash;
        if (typeof(window.webkit) != 'undefined')
            window.webkit.messageHandlers.keyman.postMessage('#' + hash);
    }
}

function suggestionPopup(obj,custom,x,y,w,h) {
    if(obj != null) {
    fragmentToggle=(fragmentToggle+1) % 100;

    var cmd = {
        'suggestion': obj,
        'isCustom': custom,
        'x': x,
        'y': y,
        'width': w,
        'height': h
    };

    var hash = 'suggestPopup-' + fragmentToggle + '+cmd=' + JSON.stringify(cmd);

    if (typeof(window.webkit) != 'undefined') {
        window.webkit.messageHandlers.keyman.postMessage('#' + hash);
    }
    }
}

function menuKeyDown() {
    fragmentToggle = (fragmentToggle + 1) % 100;
    var keyDownHash = 'menuKeyDown-' + fragmentToggle;
    //window.location.hash = 'menuKeyDown-' + fragmentToggle;
    if (typeof(window.webkit) != 'undefined')
        window.webkit.messageHandlers.keyman.postMessage('#' + keyDownHash);
}

function menuKeyUp()
{
    fragmentToggle = (fragmentToggle + 1) % 100;
    var keyUpHash = 'menuKeyUp-'+fragmentToggle;
    //window.location.hash = 'menuKeyUp-'+fragmentToggle;
    if (typeof(window.webkit) != 'undefined')
        window.webkit.messageHandlers.keyman.postMessage('#' + keyUpHash);
}

function hideKeyboard() {
    fragmentToggle = (fragmentToggle + 1) % 100;
    var hideHash = 'hideKeyboard-' + fragmentToggle;
    //window.location.hash = 'hideKeyboard-' + fragmentToggle;
    if (typeof(window.webkit) != 'undefined')
        window.webkit.messageHandlers.keyman.postMessage('#' + hideHash);
}

function langMenuPos() {
    var kmw = window['keyman'];
    var pos = kmw['touchMenuPos']();
    return pos;
}

function doResetContext() {
    keyman.resetContext();
}

function setKeymanContext(text, doSync, selStart, selLength) {
    // console.log(`setKeymanContext(${JSON.stringify(text)}, ${doSync}, ${pos}, ${length})`);
    if(text == undefined) {
        text = '';
    }

    // Both pos + length are optional parameters.
    // undefined + <number> => NaN; undefined + undefined => NaN.
    let selEnd = selStart + selLength;
    selEnd = isNaN(selEnd) ? undefined : selEnd;

    if(doSync) {
      const shouldReset = keyman.context.updateContext(text, selStart, selEnd);
      if(shouldReset) {
        keyman.resetContext();
      }
    } else {
      // if not in "sync" mode, we have a hard context reset; just full-reset it.
      keyman.context.setText(text);
      keyman.context.setSelection(selStart, selStart+selLength);
      keyman.resetContext();
    }
}

function executePopupKey(keyID, keyText) {
    var kmw=window['keyman'];
    kmw['executePopupKey'](keyID, keyText);
}

function popupVisible(value) {
    var kmw=window['keyman'];
    kmw['popupVisible'](value);
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

function enableSuggestions(model, mayPredict, mayCorrect) {
    // Set the options first so that KMW's ModelManager can properly handle model enablement states
    // the moment we actually register the new model.
    keyman.core.languageProcessor.mayPredict = mayPredict;
    keyman.core.languageProcessor.mayCorrect = mayCorrect;

    keyman.addModel(model);
}

function setSpacebarText(mode) {
    keyman.config.spacebarText = mode;
}

