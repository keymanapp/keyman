// KeymanWeb 11.0
// Copyright 2019 SIL International

/*****************************************/
/*                                       */
/*   Embedded application-specific code  */
/*                                       */
/*****************************************/

(function() {
  // Declare KeymanWeb and related objects
  var keymanweb=window['keyman'], util=keymanweb['util'];
  var dom = com.keyman.dom;

  // Flag to control refreshing of a keyboard that is already loaded
  keymanweb.mustReloadKeyboard = true;

  // Skip full page initialization - skips native-mode only code
  keymanweb.isEmbedded = true;

  util.wait = function() {
    // Empty stub - this function should not be implemented or used within embedded code routes.
    console.warn("util.wait() call attempted in embedded mode!");  // Sends log message to embedding app.
  };

  util.alert = function() {
    // Empty stub - this function should not be implemented or used within embedded code routes.
    console.warn("util.alert() call attempted in embedded mode!");  // Sends log message to embedding app.
  };

  /**
   * Set target element text direction (LTR or RTL): not functional for KMEI, KMEA
   *
   * @param       {Object}      Ptarg      Target element
   */
  keymanweb.domManager._SetTargDir = function(Ptarg){};

  /**
   * Use rotation events to adjust OSK element positions and scaling if necessary
   */
  keymanweb.handleRotationEvents = function() {};

  /**
   * Caret position always determined from the active (but hidden) element
   *
   * @return  {boolean}
   **/
  keymanweb.isPositionSynthesized = function()
  {
    return false;
  };

  /**
   * correctOSKTextSize handles rotation event -- currently rebuilds keyboard and adjusts font sizes
   */
  keymanweb['correctOSKTextSize']=function() {
    keymanweb.osk?.refreshLayout();
  };

  /**
   * Register a lexical model
   *
   * @param {com.keyman.text.prediction.ModelSpec} model  Spec of the lexical model
   */
  keymanweb['registerModel']=function(model: com.keyman.text.prediction.ModelSpec) {
    keymanweb.modelManager.register(model);
  };

  /**
   * Function called by Android and iOS when a device-implemented keyboard popup
   * is displayed or hidden.  As this is controlled by the app, we use it as a
   * trigger for 'embedded'-mode gesture state management.
   *
   *  @param  {boolean}  isVisible
   *
   **/
  keymanweb['popupVisible'] = function(isVisible) {
    let osk = keymanweb.osk;
    let gesture = osk.vkbd.subkeyGesture as com.keyman.osk.embedded.SubkeyDelegator;
    let pendingLongpress = osk.vkbd.pendingSubkey;

    /*
     * If a longpress popup was visible, but is no longer, this means that the
     * associated longpress gesture was cancelled.  It is possible for the base
     * key to emit if selected at this time; detection of this is managed by
     * the `SubkeyDelegator` class.
     */
    if(!isVisible) {
      if(gesture) {
        gesture.resolve(null);
        osk.vkbd.subkeyGesture = null;
      } else if(pendingLongpress) {
        pendingLongpress.cancel();
        osk.vkbd.pendingSubkey = null;
      }
    }

    /*
     * If the popup was not visible, but now is, that means our previously-pending
     * longpress is now 'realized' (complete).  The OSK relies upon this state
     * information, which will be properly updated by `resolve`.
     *
     * Prominent uses of such state info helps prevent change of base key, key
     * previews, and key output from occurring while a subkey popup remains active.
     */
    if(isVisible && pendingLongpress) {
      // Fulfills the first-stage promise.
      pendingLongpress.resolve();
    }
  };

  /**
   *  Return position of language menu key to KeymanTouch
   *
   *  @return  {string}      comma-separated x,y,w,h of language menu key
   *
   **/
  keymanweb['touchMenuPos'] = function() {
    let osk = keymanweb.osk;
    if(osk == null || osk.vkbd == null || osk.vkbd.lgKey == null) {
      return '';
    }

    var key: HTMLElement = osk.vkbd.lgKey;
    // A CSS change of kmd-key-square from position:fixed to position:static was needed
    // for Android 4.3 to display the OSK correctly, but resulted in the position of
    // the menu key not being returned correctly.  The following line gets the
    // key element, instead of the key-square element, fixes this.  It should be
    // removed again when the key-square elements are all removed as planned.
    if(typeof key.firstChild != 'undefined' && key.firstChild != null && util.hasClass(key.firstChild,'kmw-key')) {
      key = <HTMLElement> key.firstChild;
    }

    var w=key.offsetWidth,
        h=key.offsetHeight,
        // Since the full OSKManager '_Box' is displayed within the keyboards' WebViews,
        // these calculations should be performed with respect to that, rather than osk.vkbd.kbdDiv.
        x=dom.Utils.getAbsoluteX(key) - dom.Utils.getAbsoluteX(osk._Box) + w/2,
        y=dom.Utils.getAbsoluteY(key) - dom.Utils.getAbsoluteY(osk._Box);

    return x+','+y+','+w+','+h;
  };

  keymanweb['showGlobeHint'] = function(text: string, onAutodismissal?: () => void) {
    const keyman = keymanweb as KeymanBase;
    const globeHint = keyman.osk?.vkbd?.globeHint;

    if(globeHint) {
      // Ensure localized text is properly in-place.
      globeHint.text = text;
      globeHint.show(keyman.osk.vkbd.currentLayer.globeKey.btn, onAutodismissal);
    }
  }

  keymanweb['hideGlobeHint'] = function() {
    const keyman = keymanweb as KeymanBase;
    keyman.osk?.vkbd?.globeHint?.hide(keyman.osk.vkbd.currentLayer.globeKey.btn);
  }

 /**
   *  Accept an external key ID (from KeymanTouch) and pass to the keyboard mapping
   *
   *  @param  {string}  keyName   key identifier which could contain a display layer and a "functional" layer
   *                              e.g: 'shift-K_E+rightalt-shift'
   **/
  keymanweb['executePopupKey'] = function(keyName: string) {
      let osk = keymanweb.osk;
      var origArg = keyName;
      if(!keymanweb.core.activeKeyboard || !osk.vkbd) {
        return false;
      }

      /* Clear any pending (non-popup) key */
      osk.vkbd.keyPending = null;

      // Changes for Build 353 to resolve KMEI popup key issues
      keyName=keyName.replace('popup-',''); //remove popup prefix if present (unlikely)

      // Regex for 'display layer'-'virtual key name'+'optional functional layer'
      // Can't just split on '-' because some layers like ctrl-shift contain it.
      // Virtual key name starts with T_, K_, or U_
      // matches[1]: displayLayer (not used)
      // matches[2]: keyId
      // matches[3]: optional functionalLayer
      let matches = keyName.match(/^(.+)-([TKU]_[^+]+)\+?(.+)?$/);
      if (matches == null) {
        return false;
      }
      keyName = matches[2] + (matches[3] ? '+' + matches[3] : '');

      // Note:  this assumes Lelem is properly attached and has an element interface.
      // Currently true in the Android and iOS apps.
      var Lelem=keymanweb.domManager.lastActiveElement;
      keymanweb.domManager.initActiveElement(Lelem);

      // This should be set if we're within this method... but it's best to guard against nulls here, just in case.
      if(osk.vkbd.subkeyGesture) {
        let gesture = osk.vkbd.subkeyGesture as com.keyman.osk.embedded.SubkeyDelegator;
        gesture.resolve(keyName);
        osk.vkbd.subkeyGesture = null;
      } else {
        console.warn("No base key exists for the subkey being executed: '" + origArg + "'");
      }
  };
})();
