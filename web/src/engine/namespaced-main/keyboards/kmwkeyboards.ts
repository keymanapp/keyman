/// <reference path="../kmwbase.ts" />

namespace com.keyman.keyboards {
  export class KeyboardTag {
    stores: {[text: string]: text.ComplexKeyboardStore} = {};
  }

  export interface KeyboardChangeData {
    ['internalName']: string;
    ['languageCode']: string;
    ['indirect']: boolean;
  }

  export class KeyboardManager {
    keymanweb: KeymanBase;

    activeStub: KeyboardStub = null;
    keyboardStubs: KeyboardStub[] = [];

    // For deferment of adding keyboards until keymanweb initializes
    deferment: Promise<void> = null;
    endDeferment:() => void;

    // The following was not actually utilized within KeymanWeb; I think it's handled via different logic.
    // See setDefaultKeyboard() below.
    dfltStub = null;           // First keyboard stub loaded - default for touch-screen devices, ignored on desktops

    keyboards: any[] = [];

    linkedScripts: HTMLScriptElement[] = [];

    constructor(kmw: KeymanBase) {
      this.keymanweb = kmw;

      let _this = this;
      this.deferment = new Promise(function(resolve) {
        _this.endDeferment = resolve;
      });
    }

    getActiveKeyboardName(): string {
      let core = com.keyman.singleton.core;
      return core.activeKeyboard ? core.activeKeyboard.id : '';
    }

    getActiveLanguage(fullName?: boolean): string {
      if(this.activeStub == null) {
        return '';
      } else if(fullName) {
        return this.activeStub['KL'];
      } else {
        return this.activeStub['KLC'];
      }
    }

    /**
     * Get an associative array of keyboard identification strings
     *   This was defined as an array, so is kept that way, but
     *   Javascript treats it as an object anyway
     *
     * @param       {Object}    Lstub      Keyboard stub object
     * @param       {Object}    Lkbd       Keyboard script object
     * @return      {Object}               Copy of keyboard identification strings
     *
     */
    private _GetKeyboardDetail = function(Lstub: KeyboardStub, Lkbd: any /* KeyboardScriptObject */) { // I2078 - Full keyboard detail
      var Lr={};
      Lr['Name'] = Lstub['KN'];
      Lr['InternalName'] =  Lstub['KI'];
      Lr['LanguageName'] = Lstub['KL'];  // I1300 - Add support for language names
      Lr['LanguageCode'] = Lstub['KLC']; // I1702 - Add support for language codes, region names, region codes, country names and country codes
      Lr['RegionName'] = Lstub['KR'];
      Lr['RegionCode'] = Lstub['KRC'];
      Lr['CountryName'] = Lstub['KC'];
      Lr['CountryCode'] = Lstub['KCC'];
      Lr['KeyboardID'] = Lstub['KD'];
      Lr['Font'] = Lstub['KFont'];
      Lr['OskFont'] = Lstub['KOskFont'];
      Lr['HasLoaded'] = !!Lkbd;

      Lr['IsRTL'] = Lkbd ? !!Lkbd['KRTL'] : null;
      return Lr;
    }

    /**
     * Get array of available keyboard stubs
     *
     * @return   {Array}     Array of available keyboards
     *
     */
    getDetailedKeyboards() {
      var Lr = [], Ln, Lstub, Lrn;

      for(Ln=0; Ln < this.keyboardStubs.length; Ln++) { // I1511 - array prototype extended
        Lstub = this.keyboardStubs[Ln];

        // In Chrome, (including on Android), Array.prototype.find() requires Chrome 45.
        // This is a later version than the default on our oldest-supported Android devices.
        const Lkbd = this.keyboards.find(k => k['KI'] == Lstub['KI']);
        Lrn = this._GetKeyboardDetail(Lstub, Lkbd);  // I2078 - Full keyboard detail
        Lr=this.keymanweb._push(Lr,Lrn); // TODO:  Resolve without need for the cast.
      }
      return Lr;
    }

    /**
     *  Create or update a keyboard meta-data 'stub' during keyboard registration
     *
     *  Cross-reference with https://help.keyman.com/developer/engine/web/11.0/reference/core/addKeyboards.
     *
     *  @param  {Object}  kp  (partial) keyboard meta-data object (`spec` object)
     *  @param  {Object}  lp  language object (`spec.languages` object)
     *  @param  {Object}  options   KeymanCloud callback options
     **/
    mergeStub(kp: any, lp: any, options) {
      var sp: KeyboardStub = this.findStub(kp['id'], lp['id']);
      var isNew: boolean = false;

      // <now handled in modular code>

      // BUT:  the rest of this, with events?  Yeah, we need to be a little more on top of that.

      // Update the UI
      this.doKeyboardRegistered(sp['KI'],sp['KL'],sp['KN'],sp['KLC'],sp['KP']);

      // If we have no activeStub because there were no stubs, set the new keyboard as active.
      // Do not trigger on merges.
      if(!this.activeStub && isNew && this.keyboardStubs.length == 1 && this.keymanweb.options['setActiveOnRegister']=='true') {
        // #676: We call _SetActiveKeyboard so we can avoid overwriting
        // cookies that determine our active keyboard at page load time
        this.doBeforeKeyboardChange(sp['KI'], sp['KLC']);
        this._SetActiveKeyboard(sp['KI'], sp['KLC'], false);
        this.doKeyboardChange(sp['KI'], sp['KLC']);
      }
    }

    // Called on the embedded path at the end of its initialization.
    setDefaultKeyboard() {
      if(this.keyboardStubs.length > 0) {
        // Select the first stub as our active keyboard.
        this._SetActiveKeyboard(this.keyboardStubs[0]['KI'], this.keyboardStubs[0]['KLC']);
        return true;
      } else {
        return false;
      }
    }

    /**
     * Allow to change active keyboard by (internal) keyboard name
     *
     * @param       {string}    PInternalName   Internal name
     * @param       {string}    PLgCode         Language code
     */
    setActiveKeyboard(PInternalName: string, PLgCode: string): Promise<void> {
      //TODO: This does not make sense: the callbacks should be in _SetActiveKeyboard, not here,
      //      since this is always called FROM the UI, which should not need notification.
      //      If UI callbacks are needed at all, they should be within _SetActiveKeyboard

      // Skip on embedded which namespaces packageID::Keyboard_keyboardID
      if(!this.keymanweb.isEmbedded && PInternalName && PInternalName.indexOf("Keyboard_") != 0) {
        PInternalName = "Keyboard_" + PInternalName;
      }

      this.doBeforeKeyboardChange(PInternalName,PLgCode);
      let p: Promise<void> = this._SetActiveKeyboard(PInternalName,PLgCode,true);
      if(this.keymanweb.domManager.lastActiveElement != null) {
        this.keymanweb.domManager.focusLastActiveElement(); // TODO:  Resolve without need for the cast.
      }
      // If we ever allow PLgCode to be set by default, we can auto-detect the language code
      // after the _SetActiveKeyboard call.
      // if(!PLgCode && (<KeymanBase>keymanweb).keyboardManager.activeStub) {
      //   PLgCode = (<KeymanBase>keymanweb).keyboardManager.activeStub['KLC'];
      // }
      const _this = this;
      p.then(function() {
        // Only mark the keyboard as having changed once the setActiveKeyboard op
        // is successful.
        _this.doKeyboardChange(PInternalName, PLgCode);
      });

      p.catch(error => {
        // Rejection indicates a failure of the keyboard to load.
        //
        // In case p's rejection is never caught, throwing this error will generate logs that shows up
        // in Sentry or in the console, with useful information for debugging either way.
        throw new Error("Unable to load keyboard with internal name \"" + PInternalName + "\", language code \"" + PLgCode + "\": " + error);
      });

      return p;
    }

    /* TODO: why not use util.loadCookie and saveCookie?? */

    /**
     * Function     saveCurrentKeyboard
     * Scope        Private
     * @param       {string}    PInternalName       name of keyboard
     * @param       {string}    PLgCode             language code
     * Description Saves current keyboard as a cookie
     */
    private saveCurrentKeyboard(PInternalName: string, PLgCode: string) {
      var s = "current="+PInternalName+":"+PLgCode;
      this.keymanweb.util.saveCookie('KeymanWeb_Keyboard',{'current':PInternalName+':'+PLgCode});

      // Additionally, make sure we save the (upcoming) per-control keyboard settings.
      // This allows us to ensure the keyboard is set correctly without waiting for focus event
      // triggers - very helpful for automated testing.
      if(!this.keymanweb.isEmbedded) {
        this.keymanweb.touchAliasing._BlurKeyboardSettings(this.keymanweb.domManager.lastActiveElement, PInternalName, PLgCode);
      }
    }

    /**
     * Restore the most recently used keyboard, if still available
     */
    restoreCurrentKeyboard() {
      var stubs = this.keyboardStubs, i, n=stubs.length;
      let core = com.keyman.singleton.core;

      // Do nothing if no stubs loaded
      if(stubs.length < 1) return;

      // If no saved keyboard, default to US English, else first loaded stub
      var d=this.getSavedKeyboard();
      var t=d.split(':');

      // Identify the stub with the saved keyboard
      t=d.split(':');
      if(t.length < 2) t[1]='';

      // This loop is needed to select the correct stub when several apply to a given keyboard
      // TODO: There should be a better way!
      for(i=0; i<n; i++)
      {
        if(stubs[i]['KI'] == t[0] && (stubs[i]['KLC'] == t[1] || t[1] == '')) break;
      }

      // Sets the default stub (as specified with the `getSavedKeyboard` call) as active.
      // if((i < n) || (device.touchable && (this.activeKeyboard == null)))
      if((i < n) || (core.activeKeyboard == null))
      {
        this._SetActiveKeyboard(t[0],t[1],false);
        this.keymanweb.globalKeyboard = t[0];
        this.keymanweb.globalLanguageCode = t[1];

        this.doKeyboardChange(t[0],t[1]);        // And update the UI if necessary
      }
    }

    /**
     * Gets the cookie for the name and language code of the most recently active keyboard
     *
     *  Defaults to US English, but this needs to be user-set in later revision (TODO)
     *
     * @return      {string}          InternalName:LanguageCode
     **/
    getSavedKeyboard(): string {
      var v = this.keymanweb.util.loadCookie('KeymanWeb_Keyboard');

      if(typeof(v['current']) != 'string') {
        return 'Keyboard_us:eng';
      }

      // Check that the requested keyboard is included in the available keyboard stubs
      var n, stubs = this.keyboardStubs,kd;

      for(n=0; n<stubs.length; n++) {
        kd=stubs[n]['KI']+':'+stubs[n]['KLC'];
        if(kd == v['current']) return kd;
      }

      // Default to US English if available (but don't assume it is first)
      for(n=0; n<stubs.length; n++) {
        kd=stubs[n]['KI']+':'+stubs[n]['KLC'];
        if(kd == 'Keyboard_us:eng') return kd;
      }

      // Otherwise use the first keyboard stub
      if(stubs.length > 0) {
        return stubs[0]['KI']+':'+stubs[0]['KLC'];
      }

      // Or US English if no stubs loaded (should never happen)
      return 'Keyboard_us:eng';
    }

    /**
     * Function    isCJK
     * Scope       Public
     * @param      {Object=}  k0
     * @return     {boolean}
     * Description Tests if the keyboard stub uses a pick list (Chinese, Japanese, Korean, etc.)
     *             (This function accepts either keyboard structure.)
     */
    isCJK(k: KeyboardStub) { // I3363 (Build 301)
      var lg: string;
      if(typeof(k['KLC']) != 'undefined') {
        lg = k['KLC'];
      } else if(typeof(k['LanguageCode']) != 'undefined') {
        lg = k['LanguageCode'];
      }

      return ((lg == 'cmn') || (lg == 'jpn') || (lg == 'kor'));
    }

    /**
     * Function     _getKeyboardByID
     * Scope        Private
     * @param       {string}  keyboardID
     * @return      {Object|null}
     * Description  Returns the internal, registered keyboard object - not the stub, but the keyboard itself.
     */
    private getKeyboardByID(keyboardID: string):any {
      var Li;
      for(Li=0; Li<this.keyboards.length; Li++) {
        if(keyboardID == this.keyboards[Li]['KI']) {
          return this.keyboards[Li];
        }
      }

      return null;
    }

    /* ------------------------------------------------------------
    *  Definitions for adding, removing, and requesting keyboards.
    *  ------------------------------------------------------------
    */

    /**
     * Build 362: removeKeyboards() remove keyboard from list of available keyboards
     *
     * @param {string}  x      keyboard name string
     * @param {boolean} force  When true, also drops the cached keyboard object
     *
     */
    removeKeyboards(x: string, force?: boolean) {
      if(arguments.length == 0) {
        return false;
      }

      var i, j;
      var success = true, activeRemoved = false, anyRemoved = false;;

      for(i=0; i<arguments.length; i++) {
        for(j=this.keyboardStubs.length-1; j>=0; j--) {
          if('Keyboard_'+arguments[i] == this.keyboardStubs[j]['KI']) {
            if('Keyboard_'+arguments[i] == this.getActiveKeyboardName()) {
              activeRemoved = true;
            }

            anyRemoved = true;
            this.keyboardStubs.splice(j,1);
            break;
          }
        }

        if(j < 0) {
          success = false;
        }
      }

      for(i=0; i<arguments.length; i++) {
        for(j=this.keyboards.length-1; j>=0; j--) {
          if('Keyboard_'+arguments[i] == this.keyboards[j]['KI']) {
            this.keyboards.splice(j, 1);
            break;
          }
        }
      }

      if(activeRemoved) {
        if(this.keyboardStubs.length > 0) {
        // Always reset to the first remaining keyboard
          this._SetActiveKeyboard(this.keyboardStubs[0]['KI'],this.keyboardStubs[0]['KLC'],true);
        } else {
          this._SetActiveKeyboard('', '', false);
        }
        // This is likely to be triggered by a UI call of some sort, and we need to treat
        // this call as such to properly maintain the globalKeyboard setting.
        this.keymanweb.uiManager.justActivated = true;
      }

      if(anyRemoved) {
        // Update the UI keyboard menu
        this.doKeyboardUnregistered();
      }

      return success;
    }

    /**
     * Function     _registerKeyboard  KR
     * Scope        Public
     * @param       {Object}      Pk      Keyboard  object
     * Description  Register and load the keyboard
     */
    async _registerKeyboard(Pk) {
      // Ensure keymanweb is initialized before continuing to register keyboards
      if(!this.keymanweb.initialized) {
        await this.deferment;
      }

      if(Pk['_kmw']) {
        console.error("The keyboard _kmw property is a reserved field for engine use only; this keyboard is invalid.");
        return;
      } else {
        Pk['_kmw'] = new KeyboardTag();
      }

      var Li,Lstub;

      // For package namespacing with KMEA/KMEI.
      if(this.keymanweb.isEmbedded) {
        this.keymanweb.preserveID(Pk);
      }

      // Check if the active stub refers to this keyboard, else find applicable stub

      var Ps=this.activeStub;
      var savedActiveStub = this.activeStub;
      if(!Ps || !('KI' in Ps) || (Ps['KI'] != Pk['KI'])) {
        // Find the first stub for this keyboard
        for(Lstub=0; Lstub < this.keyboardStubs.length; Lstub++) { // I1511 - array prototype extended
          Ps=this.keyboardStubs[Lstub];
          if(Pk['KI'] == Ps['KI']) {
            break;
          }

          Ps=null;
        }
      }

      // Build 369: ensure active stub defined when loading local keyboards
      if(this.activeStub == null && Ps != null) {
        this.activeStub = Ps;
      }

      // Register the stub for this language (unless it is already registered)
      // keymanweb.KRS(Ps?Ps:Pk);

      // Test if keyboard already loaded
      for(Li=0; Li<this.keyboards.length; Li++) {
        if(Pk['KI'] == this.keyboards[Li]['KI']) {
          return;
        }
      }

      // Append to keyboards array
      this.keyboards=this.keymanweb._push(this.keyboards, Pk); // TODO:  Resolve without need for the cast.

      // Execute any external (UI) code needed after loading keyboard
      this.doKeyboardLoaded(Pk['KI']);

      // Restore the originally-active stub to its prior state.  No need to change it permanently.
      this.activeStub = savedActiveStub;
    }

    /**
     * Add the basic keyboard parameters (keyboard stub) to the array of keyboard stubs
     * If no language code is specified in a keyboard it cannot be registered,
     * and a keyboard stub must be registered before the keyboard is loaded
     * for the keyboard to be usable.
     *
     * @param       {Object}      Pstub     Keyboard stub object
     * @return      {Promise<?boolean>}      1 if already registered, else null
     */
    async _registerStub(Pstub): Promise<boolean> {
      Pstub = { ... Pstub}; // shallow clone the stub object
      // Ensure keymanweb is initialized before continuing to register stub
      if(!this.keymanweb.initialized) {
        await this.deferment;
      }

      // The default stub is always the first keyboard stub loaded [and will be ignored by desktop browsers - not for beta, anyway]
      if(this.dfltStub == null) {
        this.dfltStub=Pstub;
        //if(device.formFactor == 'desktop') return 1;    //Needs further thought before release
      }

      // If no language code has been defined, and no stub has been registered for this keyboard, register with empty string as the language code
      if(this.keymanweb.isEmbedded) {
        this.keymanweb.namespaceID(Pstub);
      } // else leave undefined.  It's nice to condition upon.
      if(typeof(Pstub['KLC']) == 'undefined') {
        Pstub['KLC'] = '';
      }
      if(typeof(Pstub['KL']) == 'undefined') {
        Pstub['KL'] = 'undefined';
      }

      // Register stub (add to KeyboardStubs array)
      this.keyboardStubs=this.keymanweb._push(this.keyboardStubs, Pstub); // TODO:  Resolve without need for the cast.

      // TODO: Need to distinguish between initial loading of a large number of stubs and any subsequent loading.
      //   UI initialization should not be needed for each registration, only at end.
      // Reload this keyboard if it was the last active keyboard and
      // make any changes needed by UI for new keyboard stub
      // (Uncommented for Build 360)
      this.doKeyboardRegistered(Pstub['KI'],Pstub['KL'],Pstub['KN'],Pstub['KLC'],Pstub['KP']);

      // If we have no activeStub because there were no stubs, set the new keyboard as active.
      // Do not trigger on merges.
      if(!this.activeStub && this.dfltStub == Pstub && this.keyboardStubs.length == 1 && this.keymanweb.options['setActiveOnRegister']=='true') {
        this.setActiveKeyboard(Pstub['KI'], Pstub['KLC']);
      }

      return Promise.resolve(false);
    }

    /*
    * Last part - the events.
    */

    /**
     * Execute external (UI) code needed on registering keyboard, used
     * to update each UIs language menu
     *
     * Note that the argument object is not at present used by any UI,
     * since the menu is always fully recreated when needed, but the arguments
     * remain defined to allow for possible use in future (Aug 2014)
     *
     * @param       {string}            _internalName
     * @param       {string}            _language
     * @param       {string}            _keyboardName
     * @param       {string}            _languageCode
     * @param       {string=}           _packageID        Used by KMEA/KMEI to track .kmp related info.
     * @return      {boolean}
     */
    doKeyboardRegistered(_internalName: string, _language: string, _keyboardName: string,
        _languageCode: string, _packageID?: string): boolean {
      var p={'internalName':_internalName,'language':_language,'keyboardName':_keyboardName,'languageCode':_languageCode};

      // Utilized only by our embedded codepaths.
      if(_packageID) {
        p['package'] = _packageID;
      }
      return this.keymanweb.util.callEvent('kmw.keyboardregistered',p);
    }

    /**
     * Execute external (UI) code to rebuild menu when deregistering keyboard
     *
     * @return      {boolean}
     */

    doKeyboardUnregistered(): boolean {
      var p={};
      return this.keymanweb.util.callEvent('kmw.keyboardregistered',p);
    }

    /**
     * Execute external (UI) code needed on loading keyboard
     *
     * @param       {string}            _internalName
     * @return      {boolean}
     */
    doKeyboardLoaded(_internalName: string): boolean {
      var p={};
      p['keyboardName']=_internalName;
      return this.keymanweb.util.callEvent('kmw.keyboardloaded', p);
    }

    /**
     * Function     doBeforeKeyboardChange
     * Scope        Private
     * @param       {string}            _internalName
     * @param       {string}            _languageCode
     * @return      {boolean}
     * Description  Execute external (UI) code needed before changing keyboard
     */
    doBeforeKeyboardChange(_internalName: string, _languageCode: string): boolean {
      var p={};
      p['internalName']=_internalName;
      p['languageCode']=_languageCode;
      return this.keymanweb.util.callEvent('kmw.beforekeyboardchange',p);
    }

    /**
     * Execute external (UI) code needed *after* changing keyboard
     *
     * @param       {string}            _internalName
     * @param       {string}            _languageCode
     * @param       {boolean=}           _indirect
     * @return      {boolean}
     */
    doKeyboardChange(_internalName: string, _languageCode: string, _indirect?:boolean): boolean {
      var p: KeyboardChangeData = {
        'internalName': _internalName,
        'languageCode': _languageCode,
        'indirect': (arguments.length > 2 ? _indirect : false)
      }

      return this.keymanweb.util.callEvent('kmw.keyboardchange', p);
    }

    shutdown() {
      for(let script of this.linkedScripts) {
        if(script.remove) {
          script.remove();
        } else if(script.parentNode) {
          script.parentNode.removeChild(script);
        }
      }
    }
  }
}
