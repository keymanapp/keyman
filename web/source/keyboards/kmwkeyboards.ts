/// <reference path="../kmwbase.ts" />
// Includes a promise polyfill (needed for IE)
/// <reference path="../../node_modules/es6-shim/es6-shim.min.js" />

namespace com.keyman.keyboards {
  class CloudRequestEntry {
    id: string;
    language?: string;
    version?: string;

    constructor(id: string, language?: string) {
      this.id = id;
      this.language = language;
    }

    toString(): string {
      var kbid=this.id;
      var lgid='';
      var kvid='';

      if(this.language) {
        kbid=kbid+'@'+this.language;
        if(this.version) {
          kbid=kbid+'@'+this.version;
        }
      } else {
        if(this.version) {
          kbid=kbid+'@@'+this.version;
        }
      }

      //TODO: add specifier validation...

      return kbid;
    }
  }

  class KeyboardFont {
    'family': string;
    'files': string;
    'path': string;

    constructor(fontObj: any, fontPath: string) {
      this['family'] = fontObj['family'];
      this['files'] = fontObj['source'];
      this['path'] = fontPath;
    }
  }

  export class KeyboardStub {
    'KI': string;
    'KLC': string;

    'KN': string;
    'KL': string;
    'KR': string;
    'KRC': string;

    'KF': string;

    'KFont': KeyboardFont;
    'KOskFont': KeyboardFont;

    'displayName': string; // Display name as shown on spacebar

    // Used when loading a stub's keyboard.
    asyncLoader?: any;

    constructor(id: string, langCode: string) {
      this['KI'] = 'Keyboard_' + id;
      this['KLC'] = langCode;
    }

    /**
     * Utility to convert stubs to KeyboardStub[]
     * @param arg 
     * @returns (KeyboardStub|ErrorStub)[]
     */
    public static toStubs(arg: any): (KeyboardStub|ErrorStub)[] {
      let errorMsg: string = '';
      if (!arg) {
        errorMsg = "Stub undefined";
      } else if (!arg.id) {
        errorMsg = "KeyboardStub has undefined id";
      } else if (!arg.languages) {
        errorMsg = "KeyboardStub has undefined languages"
      }
      if (errorMsg != '') {
        return [{error: new Error(errorMsg)}];
      }

      // Extract all the languages
      let languages: any[] = [];
      if (typeof arg.languages === 'object') {
        languages.push(arg.languages);
      } else {
        arg.languages.foreach(language => {
          languages.push(language);
        });
      }

      let stubs: KeyboardStub[] = [];
      languages.forEach(language => {
        let stub: KeyboardStub = new KeyboardStub(arg.id, language.id);

        if (arg.name) {
          stub['KN'] = arg.name
        }
        if (arg.filename) {
          stub['KF'] = arg.filename;
        }
        if (arg.displayName) {
          stub['displayName'] = arg.displayName;
        }

        if (language.name) {
          stub['KL'] = language.name;
        }
        if (language.region) {
          stub['KR'] = language.region;
        }

        // Can ignore ['KRC'] RegionCodes used by the Toolbar UI

        if (language.font) {
          stub['KFont'] = language.font;
        }
        if (language.oskFont) {
          stub['KOskFont'] = language.oskFont;
        }

        stubs.push(stub);
      })

      return stubs;
    }
  }

  // Information about a keyboard that fails to get added
  export interface ErrorStub {
    keyboard?: {
      id: string;
      name: string;
    },
    language?: {
      id?: string;
      name?: string;
    }

    error: Error;
  }
  export class KeyboardTag {
    stores: {[text: string]: text.ComplexKeyboardStore} = {};
  }

  export interface KeyboardChangeData {
    ['internalName']: string;
    ['languageCode']: string;
    ['indirect']: boolean;
  }

  // For when the API call straight-up times out.
  const CLOUD_TIMEOUT_ERR = "The Cloud API request timed out.";
  // Currently cannot distinguish between "no matching keyboard" and other script-load errors.
  const CLOUD_MALFORMED_OBJECT_ERR = "Could not find a keyboard with that ID.";
  // Represents unspecified errors that occur when registering the results of a successful API call.
  const CLOUD_STUB_REGISTRATION_ERR = "The Cloud API failed to find an appropriate keyboard.";
  // Represents custom, specified KMW errors that occur when registering the results of a successful API call.
  const CLOUD_REGISTRATION_ERR = "Error occurred while registering keyboards: ";

  const MISSING_KEYBOARD = function(kbdid: string) {
    return kbdid + ' keyboard not found.';
  }

  interface RegistrationPromiseTuple {
    resolve: (stubs: KeyboardStub[]) => void;
    reject: (err: Error) => void;
  }

  export class KeyboardManager {
    // Language regions as defined by cloud server
    static readonly regions = ['World','Africa','Asia','Europe','South America','North America','Oceania','Central America','Middle East'];
    static readonly regionCodes = ['un','af','as','eu','sa','na','oc','ca','me'];

    keymanweb: KeymanBase;

    activeStub: KeyboardStub = null;
    keyboardStubs: KeyboardStub[] = [];

    firstCall: boolean = true; // First time to call keymanCloudRequest()

    // For deferment of adding keyboards until keymanweb initializes
    deferment: Promise<void> = null;
    endDeferment:() => void;

    // The following was not actually utilized within KeymanWeb; I think it's handled via different logic.
    // See setDefaultKeyboard() below.
    dfltStub = null;           // First keyboard stub loaded - default for touch-screen devices, ignored on desktops

    keyboards: any[] = [];

    /**
     * Holds the 'resolve' function for Promises built by keymanCloudRequest.
     * These should be cleared when the Promise is fulfilled.
     */
    registrationResolvers: {[timeoutID: number] : RegistrationPromiseTuple} = {};

    languageListPromise: Promise<(KeyboardStub|ErrorStub)[]> = null;

    languageList: any[] = null; // List of keyboard languages available for KeymanCloud

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
     * @param       {Object}    Lkbd       Keyboard object
     * @return      {Object}               Copy of keyboard identification strings
     *
     */
    private _GetKeyboardDetail = function(Lkbd) { // I2078 - Full keyboard detail
      var Lr={};
      Lr['Name'] = Lkbd['KN'];
      Lr['InternalName'] =  Lkbd['KI'];
      Lr['LanguageName'] = Lkbd['KL'];  // I1300 - Add support for language names
      Lr['LanguageCode'] = Lkbd['KLC']; // I1702 - Add support for language codes, region names, region codes, country names and country codes
      Lr['RegionName'] = Lkbd['KR'];
      Lr['RegionCode'] = Lkbd['KRC'];
      Lr['CountryName'] = Lkbd['KC'];
      Lr['CountryCode'] = Lkbd['KCC'];
      Lr['KeyboardID'] = Lkbd['KD'];
      Lr['Font'] = Lkbd['KFont'];
      Lr['OskFont'] = Lkbd['KOskFont'];
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

      for(Ln=0; Ln < this.keyboardStubs.length; Ln++)  // I1511 - array prototype extended
      {
        Lstub = this.keyboardStubs[Ln];
        Lrn = this._GetKeyboardDetail(Lstub);  // I2078 - Full keyboard detail
        Lr=this.keymanweb._push(Lr,Lrn); // TODO:  Resolve without need for the cast.
      }
      return Lr;
    }

    /**
     * Register a fully specified keyboard (add meta-data for each language) immediately
     *
     * @param  {Object}  arg
     * @returns {boolean}
     **/
    addStub(arg: any): boolean {
      if(typeof(arg['id']) != 'string') {
        return false;
      }

      if(typeof(arg['language']) != "undefined") {
        console.warn("The 'language' property for keyboard stubs has been deprecated.  Please use the 'languages' property instead.");
        arg['languages'] = arg['language'];
      }

      if(typeof(arg['languages']) == 'undefined') {
        return false;
      }

      // Default the keyboard name to its id, capitalized
      if(typeof(arg['name']) != 'string') {
        arg['name'] = arg['id'].replace('_',' ');
        arg['name'] = arg['name'].substr(0,1).toUpperCase()+arg['name'].substr(1);
      }

      var lgArg=arg['languages'];
      var lgList=[], i, lg;
      if(typeof(lgArg.length) == 'undefined') {
        lgList[0] = lgArg;
      } else {
        lgList = lgArg;
      }

      var localOptions={
        'keyboardBaseUri': this.keymanweb.options['keyboards'],
        'fontBaseUri': this.keymanweb.options['fonts']
      };

      // Add a stub for each correctly specified language
      for(i=0; i<lgList.length; i++) {
        this.mergeStub(arg, lgList[i], localOptions);
      }

      return true;
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

      if(sp == null) {
        sp= new KeyboardStub(kp['id'], lp['id']);
        this.keyboardStubs.push(sp);
        isNew = true;
      }

      // Accept region as number (from Cloud server), code, or name
      var region=lp['region'], rIndex=0;
      if(typeof(region) == 'number') {
        if(region < 1 || region > 9) {
          rIndex = 0;
        } else {
          rIndex = region-1;
        }
      } else if(typeof(region) == 'string') {
        var list = (region.length == 2 ? KeyboardManager.regionCodes : KeyboardManager.regions);
        for(var i=0; i<list.length; i++) {
          if(region.toLowerCase() == list[i].toLowerCase()) {
            rIndex=i;
            break;
          }
        }
      }

      var rx: RegExp;

      sp['KL'] = (typeof sp['KL'] === 'undefined') ? lp['name'] : sp['KL'];
      sp['KR'] = (typeof sp['KR'] === 'undefined') ? KeyboardManager.regions[rIndex] : sp['KR'];
      sp['KRC'] = (typeof sp['KRC'] === 'undefined') ? KeyboardManager.regionCodes[rIndex] : sp['KRC'];
      sp['KN'] = (typeof sp['KN'] === 'undefined') ? kp['name'] : sp['KN'];
      sp['displayName'] = (typeof sp['displayName'] === 'undefined') ? kp['displayName'] : sp['displayName'];

      if(typeof(sp['KF']) == 'undefined') {
        rx=RegExp('^(([\\.]/)|([\\.][\\.]/)|(/))|(:)');
        sp['KF'] = kp['filename'];

        if(!rx.test(sp['KF'])) {
          sp['KF'] = options['keyboardBaseUri']+sp['KF'];
        }
      }

      // Font path defined by cloud entry
      var fontPath=options['fontBaseUri'];

      // or overridden locally, in page source
      if(this.keymanweb.options['fonts'] != '') {
        fontPath=this.keymanweb.options['fonts'];
        rx=new RegExp('^https?\\:');
        if(!rx.test(fontPath)) {
          if(fontPath.substr(0,2) == '//') {
            fontPath = this.keymanweb.protocol + fontPath;
          } else if(fontPath.substr(0,1) == '/') {
            fontPath = this.keymanweb.rootPath + fontPath.substr(1);
          } else {
            fontPath = this.keymanweb.rootPath + fontPath;
          }
        }
      }
      else {
        this.keymanweb.options.fonts=fontPath;
      }

      // Add font specifiers where necessary and not overridden by user
      if(typeof(lp['font']) != 'undefined') {
        sp['KFont'] = (typeof sp['KFont'] === 'undefined') ? new KeyboardFont(lp['font'], fontPath) : sp['KFont'];
      }

      // Fixed OSK font issue Github #7 (9/1/2015)
      if(typeof(lp['oskFont']) != 'undefined') {
        sp['KOskFont'] = (typeof sp['KOskFont'] === 'undefined') ? new KeyboardFont(lp['oskFont'], fontPath) : sp['KOskFont'];
      }

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

    /**
     *  Find a keyboard stub by id in the registered keyboards list
     *
     *  @param  {string}  kid   internal keyboard id (without 'Keyboard_' prefix)
     *  @param  {string}  lgid  language code
     *
     **/
    findStub(kid: string, lgid: string): KeyboardStub {
      var i;
      for(i=0; i<this.keyboardStubs.length; i++) {
        if((this.keyboardStubs[i]['KI'] == 'Keyboard_'+kid) && (this.keyboardStubs[i]['KLC'] == lgid)) {
          return this.keyboardStubs[i];
        }
      }

      return null;
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
      this.doKeyboardChange(PInternalName, PLgCode);

      p.catch(error => {
        // Rejection indicates a failure of the keyboard to load.
        //
        // In case p's rejection is never caught, throwing this error will generate logs that shows up
        // in Sentry or in the console, with useful information for debugging either way.
        throw new Error("Unable to load keyboard with internal name \"" + PInternalName + "\", language code \"" + PLgCode + "\".");
      });

      return p;
    }

    /**
     * Change active keyboard to keyboard selected by (internal) name and language code
     *
     *  Test if selected keyboard already loaded, and simply update active stub if so.
     *  Otherwise, insert a script to download and insert the keyboard from the repository
     *  or user-indicated file location.
     *
     * Note that the test-case oriented 'recorder' stubs this method to provide active
     * keyboard stub information.  If changing this function, please ensure the recorder is
     * not affected.
     *
     * @param       {string}    PInternalName
     * @param       {string=}    PLgCode
     * @param       {boolean=}   saveCookie
     */
    _SetActiveKeyboard(PInternalName: string, PLgCode?: string, saveCookie?: boolean): Promise<void> {
      var n, Ln;

      let keyman = com.keyman.singleton;

      var util = keyman.util;
      var osk = keyman.osk;

      let activeKeyboard = keyman.core.activeKeyboard;

      // Set default language code
      if(arguments.length < 2 || (!PLgCode)) {
        PLgCode='---';
      }

      // Check that the saved keyboard is currently registered
      for(n=0; n < this.keyboardStubs.length; n++) {
        if(PInternalName == this.keyboardStubs[n]['KI']) {
          if(PLgCode == this.keyboardStubs[n]['KLC'] || PLgCode == '---') break;
        }
      }

      // Mobile device addition: force selection of the first keyboard if none set
      if(util.device.touchable && (PInternalName == '' || PInternalName == null || n >= this.keyboardStubs.length)) {
        if(this.keyboardStubs.length != 0) {
          PInternalName=this.keyboardStubs[0]['KI'];
          PLgCode=this.keyboardStubs[0]['KLC'];
        }
      }

      // Save name of keyboard (with language code) as a cookie
      if(arguments.length > 2 && saveCookie) {
        this.saveCurrentKeyboard(PInternalName,PLgCode);
      }

      // Check if requested keyboard and stub are currently active
      if(this.activeStub && activeKeyboard && activeKeyboard.id == PInternalName
        && this.activeStub['KI'] == PInternalName     //this part of test should not be necessary, but keep anyway
        && this.activeStub['KLC'] == PLgCode && !this.keymanweb.mustReloadKeyboard
        ) return Promise.resolve();

      // Check if current keyboard matches requested keyboard, but not stub
      if(activeKeyboard && (activeKeyboard.id == PInternalName)) {
        // If so, simply update the active stub
        for(Ln=0; Ln<this.keyboardStubs.length; Ln++) {
          if((this.keyboardStubs[Ln]['KI'] == PInternalName)
            && (this.keyboardStubs[Ln]['KLC'] == PLgCode)) {
            this.activeStub = this.keyboardStubs[Ln];

            // Append a stylesheet for this keyboard for keyboard specific styles
            // or if needed to specify an embedded font
            osk.vkbd?.appendStyleSheet();

            // Re-initializate OSK before returning if required
            if(this.keymanweb.mustReloadKeyboard) {
              activeKeyboard.refreshLayouts();
              if(osk) {
                osk._Load();
              }
            }
            return Promise.resolve();
          }
        }
      }

      keyman.core.activeKeyboard = null;
      this.activeStub = null;

      // Hide OSK and do not update keyboard list if using internal keyboard (desktops)
      if(PInternalName == '') {
        osk.startHide(false);

        if(!this.keymanweb.isEmbedded) {
          util.wait(false);
        }

        return Promise.resolve();
      }

      // Determine if the keyboard was previously loaded but is not active and use the prior load if so.
      for(Ln=0; Ln<this.keyboards.length; Ln++) { // I1511 - array prototype extended
        if(this.keyboards[Ln]['KI'] == PInternalName) {
          keyman.core.activeKeyboard = new Keyboard(this.keyboards[Ln]);
          // As a rotation may have occurred since the keyboard was swapped out,
          // we should refresh its layouts.
          keyman.core.activeKeyboard.refreshLayouts();
          this.keymanweb.domManager._SetTargDir(this.keymanweb.domManager.lastActiveElement);  // I2077 - LTR/RTL timing

          // and update the active stub
          for(var Ls=0; Ls<this.keyboardStubs.length; Ls++) {
            if((this.keyboardStubs[Ls]['KI'] == PInternalName) &&
              (this.keyboardStubs[Ls]['KLC'] == PLgCode || PLgCode == '---')) {
              this.activeStub = this.keyboardStubs[Ls];
              break;
            }
          }
          break;
        }
      }

      // If we've reached this point, this is the first load request for the requested keyboard.
      if(keyman.core.activeKeyboard == null) {
        for(Ln=0; Ln<this.keyboardStubs.length; Ln++) { // I1511 - array prototype extended
          if((this.keyboardStubs[Ln]['KI'] == PInternalName)
            && ((this.keyboardStubs[Ln]['KLC'] == PLgCode) || (PLgCode == '---'))) {
            // Force OSK display for CJK keyboards (keyboards using a pick list)
            if(this.isCJK(this.keyboardStubs[Ln]) || util.device.touchable) {
              osk.displayIfActive = true;
            }

            // Create a script to load from the server - when it finishes loading, it will register itself,
            //  detect that it is active, and focus as appropriate. The second test is needed to allow recovery from a failed script load

            // Ensure we're not already loading the keyboard.
            if(!this.keyboardStubs[Ln].asyncLoader) {
              // Always (temporarily) hide the OSK when loading a new keyboard, to ensure that a failure to load doesn't leave the current OSK displayed
              if(osk) {
                osk.startHide(false);
              }

              var loadingStub = this.keyboardStubs[Ln];
              // Tag the stub so that we don't double-load the keyboard!
              loadingStub.asyncLoader = {};

              var kbdName = loadingStub['KN'];
              var lngName = loadingStub['KL'];
              kbdName = kbdName.replace(/\s*keyboard\s*/i, '');

              // Setup our default error-messaging callback if it should be implemented.
              loadingStub.asyncLoader.callback = function(altString, msgType) {
                var msg = altString || 'Sorry, the '+kbdName+' keyboard for '+lngName+' is not currently available.';

                // Thanks, Closure errors.
                if(!this.keymanweb.isEmbedded) {
                  util.wait(false);
                  util.internalAlert(altString || msg, function() {
                      this.keymanweb['setActiveKeyboard'](''); // The API call!
                  }.bind(this));
                }

                switch(msgType) { // in case we extend this later.
                  case 'err':
                    console.error(msg);
                    break;
                  case 'warn':
                  default:
                    console.warn(msg);
                    break;
                }

                if(Ln > 0) {
                  var Ps = this.keyboardStubs[0];
                  this._SetActiveKeyboard(Ps['KI'], Ps['KLC'], true);
                }
              }.bind(this);

              loadingStub.asyncLoader.timer = window.setTimeout(loadingStub.asyncLoader.callback, 10000);

              //Display the loading delay bar (Note: only append 'keyboard' if not included in name.)
              if(!this.keymanweb.isEmbedded) {
                util.wait('Installing keyboard<br/>' + kbdName);
              }

              // Installing the script immediately does not work reliably if two keyboards are
              // loaded in succession if there is any delay in downloading the script.
              // It works much more reliably if deferred (KMEW-101, build 356)
              // The effect of a delay can also be tested, for example, by setting the timeout to 5000
              var manager = this;
              loadingStub.asyncLoader.promise = new Promise<void>(function(resolve, reject) {
                window.setTimeout(function(){
                  manager.installKeyboard(resolve, reject, loadingStub);
                },0);
              });
            }
            this.activeStub=this.keyboardStubs[Ln];
            return this.keyboardStubs[Ln].asyncLoader.promise;
          }
        }
        this.keymanweb.domManager._SetTargDir(this.keymanweb.domManager.lastActiveElement);  // I2077 - LTR/RTL timing
      }

      // Initialize the OSK (provided that the base code has been loaded)
      if(osk) {
        osk._Load();
      }
      return Promise.resolve();
    }

    /**
     * Install a keyboard script that has been downloaded from a keyboard server
     * Operates as the core of a Promise, hence the 'resolve' and 'reject' parameters.
     *
     *  @param  {Object}  kbdStub   keyboard stub to be loaded.
     *
     **/
    installKeyboard(resolve: () => void, reject: () => void, kbdStub: KeyboardStub) {
      var util = this.keymanweb.util;
      var osk = this.keymanweb.osk;

      var Lscript = util._CreateElement('script');
      Lscript.charset="UTF-8";        // KMEW-89
      Lscript.type = 'text/javascript';

      // Preserve any namespaced IDs by use of the script's id tag attribute!
      if(this.keymanweb.isEmbedded) {
        Lscript.id = kbdStub['KI'];
      }

      var kbdFile = kbdStub['KF'];
      var kbdLang = kbdStub['KL'];
      var kbdName = kbdStub['KN'];

      var manager = this;
      let core = com.keyman.singleton.core;

      // Add a handler for cases where the new <script> block fails to load.
      Lscript.addEventListener('error', function() {
        if(kbdStub.asyncLoader.timer !== null) {
          // Clear the timeout timer.
          window.clearTimeout(kbdStub.asyncLoader.timer);
          kbdStub.asyncLoader.timer = null;
        }

        // We already know the load has failed... why wait?
        kbdStub.asyncLoader.callback('Cannot find the ' + kbdName + ' keyboard for ' + kbdLang + '.', 'warn');
        kbdStub.asyncLoader = null;

        reject();
      }, false);


      // The load event will activate a newly-loaded keyboard if successful and report an error if it is not.
      Lscript.addEventListener('load', function() {
        if(kbdStub.asyncLoader.timer !== null) {
          // Clear the timeout timer.
          window.clearTimeout(kbdStub.asyncLoader.timer);
          kbdStub.asyncLoader.timer = null;
        }

        // To determine if the load was successful, we'll need to check the keyboard array for our desired keyboard.
        // Test if keyboard already loaded
        var kbd = manager.getKeyboardByID(kbdStub['KI']), Li;
        if(kbd) {  // Is cleared upon a successful load.

          //Activate keyboard, if it's still the active stub.
          if(kbdStub == manager.activeStub) {
            manager.doBeforeKeyboardChange(kbd['KI'],kbdStub['KLC']);
            core.activeKeyboard=new Keyboard(kbd);

            if(manager.keymanweb.domManager.lastActiveElement != null) { // TODO:  Resolve without need for the cast.
              manager.keymanweb.uiManager.justActivated = true; // TODO:  Resolve without need for the cast.
              manager.keymanweb.domManager._SetTargDir(manager.keymanweb.domManager.lastActiveElement);
            }

            manager.saveCurrentKeyboard(kbd['KI'], kbdStub['KLC']);

            // Prepare and show the OSK for this keyboard
            if(osk) {
              osk._Load();
            }
          }

          // Remove the wait message, if defined
          if(!manager.keymanweb.isEmbedded) {
            util.wait(false);
          }

          kbdStub.asyncLoader = null;
          resolve();
          // A handler portion for cases where the new <script> block loads, but fails to process.
        } else {  // Output error messages even when embedded - they're useful when debugging the apps and KMEA/KMEI engines.
          kbdStub.asyncLoader.callback('Error registering the ' + kbdName + ' keyboard for ' + kbdLang + '.', 'error');
          kbdStub.asyncLoader = null;
          reject();
        }
      }, false);

      // IE likes to instantly start loading the file when assigned to an element, so we do this after the rest
      // of our setup.  This method is not relocated here (yet) b/c it varies based upon 'native' vs 'embedded'.
      Lscript.src = this.keymanweb.getKeyboardPath(kbdFile);

      try {
        document.body.appendChild(Lscript);
        this.linkedScripts.push(Lscript);
      }
      catch(ex) {
        try {
          document.getElementsByTagName('head')[0].appendChild(Lscript);
        } catch(ex2) {
          reject();
        }
      }
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
     * Function       isUniqueRequest
     * Scope          Private
     * @param         {Object}    tEntry
     * Description    Checks to ensure that the stub isn't already loaded within KMW or subject
     *                to an already-pending request.
     */
    isUniqueRequest(cloudList: {id: string, language?: string}[], tEntry: CloudRequestEntry) {
      var k;

      if(this.findStub(tEntry.id, tEntry.language) == null) {
        for(k=0; k < cloudList.length; k++) {
          if(cloudList[k].id == tEntry['id'] && cloudList[k].language == tEntry.language) {
            return false;
          }
        }
        return true;
      } else {
        return false;
      }
    };

    /**
     * Returns a Promise of the merged keyboard stubs and error stubs.
     * 
     * If the keyboard stub array is empty, will return a rejected Promise, 
     * otherwise returns a resolved Promise.
     *
     * @param keyboardStubs  array of keyboard stubs to merge.
     * @param errorStubs     array of error stubs to merge.
     * @returns  resolved or rejected promise with merged array of stubs.
     */
    private mergeAndResolveStubPromises(keyboardStubs: (KeyboardStub|ErrorStub)[], errorStubs: ErrorStub[]) :
        Promise<(KeyboardStub|ErrorStub)[]> {
      if (errorStubs.length == 0) {
        return Promise.resolve(keyboardStubs);
      } if (keyboardStubs.length == 0) {
        return Promise.reject(errorStubs);
      } else {
        // Merge this with errorStubs
        let result: (KeyboardStub|ErrorStub)[] = keyboardStubs;
        return Promise.resolve(result.concat(errorStubs));
      }
    }

    /**
     * Build 362: addKeyboardArray() link to Cloud. One or more arguments may be used
     *
     * @param x  keyboard name string or keyboard metadata JSON object
     * @returns resolved or rejected promise with merged array of stubs.
     */
    async addKeyboardArray(x: (string|KeyboardStub)[]): Promise<(KeyboardStub|ErrorStub)[]> {
      let errorStubs: ErrorStub[] = [];

      // Ensure keymanweb is initialized before continuing to add keyboards
      if(!this.keymanweb.initialized) {
        await this.deferment;
      }

      // Ignore empty array passed as argument
      if(x.length == 0) {
        let stub: ErrorStub = {error: new Error("No keyboards to add")}
        errorStubs.push(stub);
        // Normally reject error, but this can be a warning
        return Promise.resolve(errorStubs);
      }

      // Create a temporary array of metadata objects from the arguments used
      var i,j,cmd='',comma='';
      var cloudList: CloudRequestEntry[] = [];
      let keyboardStubs: KeyboardStub[] = [];
      var tEntry: CloudRequestEntry;

      for(i=0; i<x.length; i++) {
        if(typeof(x[i]) == 'string' && (<string>x[i]).length > 0) {
          var pList=(<string>x[i]).split('@'),lList=[''];
          if(pList[0].toLowerCase() == 'english') {
            pList[0] = 'us';
          }

          if(pList.length > 1) {
            lList=pList[1].split(',');
          }

          for(j=0; j<lList.length; j++) {
            tEntry = new CloudRequestEntry(pList[0]);

            if(lList[j] != '') {
              tEntry.language=lList[j];
            }

            if(pList.length > 2) {
              tEntry.version=pList[2];
            }

            // If we've already registered or requested a stub for this keyboard-language pairing,
            // don't bother with a cloud request.
            if(this.isUniqueRequest(cloudList, tEntry)) {
              cloudList.push(tEntry);
            }
          }
        }
        if(typeof(x[i]) == 'object' && x[i] != null) {
          // Register any local keyboards immediately:
          // - must specify filename, keyboard name, language codes, region codes
          // - no request will be sent to cloud
          if(typeof(x[i]['filename']) == 'string') {
            if(!this.addStub(x[i])) {
              this.keymanweb.util.internalAlert('To use a custom keyboard, you must specify file name, keyboard name, language, language code and region code.');
            }
          } else {
            if(x[i]['language']) {
              console.warn("The 'language' property for keyboard stubs has been deprecated.  Please use the 'languages' property instead.");
              x[i]['languages'] = x[i]['language'];
            }

            lList=x[i]['languages'];
            if (!lList) {
              let msg = 'To use keyboard \'' + x[i]['id'] + '\', you must specify languages.';
              let e: ErrorStub = {
                keyboard: {
                  id : x[i]['id'],
                  name: x[i]['name']
                },
                error: new Error(msg)
              };
              errorStubs.push(e);
              continue;
            }

            //Array or single entry?
            if(typeof(lList.length) == 'number') {
              for(j=0; j<lList.length; j++) {
                tEntry = new CloudRequestEntry(x[i]['id'], x[i]['languages'][j]['id']);
                if(this.isUniqueRequest(cloudList, tEntry)) {
                  cloudList.push(tEntry);
                }
              }
            } else { // Single language element
              tEntry = new CloudRequestEntry(x[i]['id'], x[i]['languages']['id']);
              if(this.isUniqueRequest(cloudList, tEntry)) {
                cloudList.push(tEntry);
              }
            }
          }

          // Convert stub from one-to-many KeyboardStub[]
          let convertedStubs = KeyboardStub.toStubs(x[i]);
          convertedStubs.forEach(s => {
            if (s instanceof KeyboardStub) {
              keyboardStubs.push(s);
            } else {
              errorStubs.push(s);
            }
          })
        }
      }

      // Return if all keyboards being registered are local and fully specified
      try {
        if(cloudList.length == 0) {
          return this.mergeAndResolveStubPromises(keyboardStubs, errorStubs);
        }
      } catch (error) {
        console.error(error);
        return Promise.reject(error);
      }

      // Update the keyboard metadata list from keyman.com - build the command
      cmd='&keyboardid=';
      for(i=0; i<cloudList.length; i++) {
        cmd=cmd+comma+cloudList[i].toString();
        comma=',';
      }

      // Request keyboard metadata from the Keyman Cloud keyboard metadata server
      try {
        let result: (KeyboardStub|ErrorStub)[]|Error = await this.keymanCloudRequest(cmd,false);
        return this.mergeAndResolveStubPromises(result, errorStubs);
      } catch(err) {
        // We don't have keyboard info for this ErrorStub
        console.error(err);
        let stub: ErrorStub = {error: err};
        errorStubs.push(stub);
        return Promise.reject(errorStubs);
      }

      // no keyboards added so return empty stub
      return Promise.resolve(errorStubs);
    }

    /**
     *  Register a keyboard for each associated language
     *
     *  @param  {Object}  kp  Keyboard Object or Object array
     *  @param  {Object}  options   keymanCloud callback options
     *  @param  {number}  nArg  keyboard index in argument array
     *
     **/
    registerLanguagesForKeyboard(kp, options, nArg:number) {
      var i,j,id,nDflt=0,kbId='';

      // Do not attempt to process badly formatted requests
      if(typeof(kp) == 'undefined') {
        return;
      }

      if(typeof(options['keyboardid']) == 'string') {
        kbId = options['keyboardid'].split(',')[nArg];
      }

      // When keyboards requested by language code, several keyboards may be returned as an array
      if(typeof(kp.length) == 'number') {
        // If language code is suffixed by $, register all keyboards for this language
        if(kp.length == 1 || kbId.substr(-1,1) == '$' || kbId == '') {
          for(i=0; i<kp.length; i++) {
            this.registerLanguagesForKeyboard(kp[i],options,nArg);
          }
        }
        // Register the default keyboard for the language code
        // Until a default is defined, the default will be the Windows keyboard,
        // that is, the keyboard named for the language (exception: English:US), or the
        // first keyboard found.
        else {
          for(i=0; i<kp.length; i++) {
            id=kp[i].id.toLowerCase();
            if(id == 'us') {
              id='english';
            }

            for(j=0; j<kp[i]['languages'].length; j++) {
              if(id == kp[i]['languages'][j]['name'].toLowerCase()) {
                nDflt = i;
                break;
              }
            }
          }

          this.registerLanguagesForKeyboard(kp[nDflt],options,nArg);
        }
      } else { // Otherwise, process a single keyboard for the specified languages
        // May need to filter returned stubs by language
        var lgCode=kbId.split('@')[1];
        if(typeof(lgCode) == 'string') {
          lgCode=lgCode.replace(/\$$/,'');
        }

        // Can only add keyboard stubs for defined languages
        var ll=kp['languages'];
        if(typeof(ll) != 'undefined') {
          if(typeof(ll.length) == 'number') {
            for(i=0; i<ll.length; i++) {
              if(typeof(lgCode) == 'undefined' || ll[i]['id'] == lgCode) {
                this.mergeStub(kp,ll[i],options);
              }
            }
          } else {
            this.mergeStub(kp,ll,options);
          }
        }
      }
    }

    /**
     * Call back from cloud for adding keyboard metadata
     *
     * @param {Object}    x   metadata object
     **/
    register(x) {
      const promiseid = x['timerid'];

      let result: KeyboardStub[] | Error;
      try {
        result = this._registerCore(x);
      } catch(err) {
        result = new Error(CLOUD_REGISTRATION_ERR + err);
      }

      if(promiseid) {
        const promiseFuncs = this.registrationResolvers[promiseid];
        window.clearTimeout(promiseid);

        try {
          if(result instanceof Error) {
            promiseFuncs.reject(result as Error);
          } else {
            promiseFuncs.resolve(result as KeyboardStub[]);
          }
        } finally {
          delete this.registrationResolvers[promiseid];
        }
      }
    }

    /**
     * Call back from cloud for adding keyboard metadata
     *
     * @param {Object}    x   metadata object
     **/
    private _registerCore(x): KeyboardStub[] | Error { // TODO (#5044): should return heterogenous type; allow array of stubs.
      var options=x['options'];
      let currentKeyboardStubsCount = this.keyboardStubs.length;

      // Indicate if unable to register keyboard
      if(typeof(x['error']) == 'string') {
        // Currently unreachable (24 May 2021 - API returns a 404; returned 'script' does not call register)
        var badName='';
        if(typeof(x['keyboardid']) == 'string') {
          badName = x['keyboardid'].substr(0,1).toUpperCase()+x['keyboardid'].substr(1);
        }

        return new Error(MISSING_KEYBOARD(badName));
      }

      // Ignore callback unless the context is defined
      if(typeof(options) == 'undefined' || typeof(options['context']) == 'undefined') {
        return new Error(CLOUD_MALFORMED_OBJECT_ERR);
      }

      // Register each keyboard for the specified language codes
      if(options['context'] == 'keyboard') {
        var i,kp=x['keyboard'];
        // Process array of keyboard definitions
        if(typeof(kp.length) == 'number') {
          for(i=0; i<kp.length; i++) {
            // Note:  if an invalid language code is specified, the elements here may be
            //        empty arrays.  Will not report an error if so.
            this.registerLanguagesForKeyboard(kp[i],options,i);
          }
        } else { // Process a single keyboard definition
          this.registerLanguagesForKeyboard(kp,options,0);
        }
      } else if(options['context'] == 'language') { // Download the full list of supported keyboard languages
        this.languageList = x['languages'];
      }

      return this.keyboardStubs.slice(currentKeyboardStubsCount);
    }

    /**
     *  Internal handler for processing keyboard registration, used only by `register`
     *
     *  @param  {string[]}   languages    Array of language names
     *  @returns {Promise<(KeyboardStub|ErrorStub)[]>} Promise of added keyboard stubs
     **/
    async addLanguageKeyboards(languages: string[]): Promise<(KeyboardStub|ErrorStub)[]> {
      var i, j, lgName, cmd, addAll, retPromise;

      let errorStubs: ErrorStub[] = [];

      // Defer registering keyboards by language until the language list has been loaded
      if (this.languageList == null) {
        if (this.firstCall) {
          this.firstCall = false;
          let promise = this.keymanCloudRequest('',true);
          // If promise is not error, then... (needs an error guard)
          promise.catch(function(error) {
            let msg = "Unable to retrieve the master language list.";
            console.error(msg, error)
            let stub: ErrorStub = {error: new Error(msg)};
            errorStubs.push(stub);
            return Promise.reject(errorStubs);
          });
          this.languageListPromise = promise;
        }

        let _this = this;
        retPromise = new Promise(function(resolve, reject) {
          if (_this.languageListPromise) {
            // 1: wait for the language list to be loaded properly
            _this.languageListPromise.then(function() {
              // 2: perform the actual query, now that we can find the language code
              resolve(_this.addLanguageKeyboards(languages));
            }).catch(function(error) {
              return Promise.reject(error);
            });
          } else {
            return _this.addLanguageKeyboards(languages);
          }
        }).then(function(result) {
          if (result instanceof Error) {
            return Promise.reject(result);
          }
          return Promise.resolve(result);
        }).catch(function(error) {
          return Promise.reject(error);
        });
      
      } else { // Identify and register each keyboard by language name
        cmd = '';
        for(i=0; i<languages.length; i++) {
          lgName = languages[i].toLowerCase();
          addAll = (lgName.substr(-1,1) == '$');
          if(addAll) {
            lgName = lgName.substr(0,lgName.length-1);
          }

          let languageFound: boolean = false;
          for(j=0; j<this.languageList.length; j++) {
            if(lgName == this.languageList[j]['name'].toLowerCase()) {
              if(cmd != '') {
                cmd = cmd + ',';
              }

              cmd = cmd+'@'+this.languageList[j]['id'];
              if(addAll) {
                cmd = cmd + '$';
              }

              languageFound = true;
              break;
            }
          }
          if (!languageFound) {
            // Construct response array of errors (failed-query keyboards)
            // that will be merged with stubs (successfully-queried keyboards)
            let stub: ErrorStub = {language: {name: lgName}, error: new Error(this.alertLanguageUnavailable(lgName))};
            errorStubs.push(stub);
          }
        }

        if(cmd == '') {
          // No command so return errors
          return Promise.reject(errorStubs);
        } 

        try {
          // Merge this with errorStub
          let result:(KeyboardStub|ErrorStub)[]|Error = await this.keymanCloudRequest('&keyboardid='+cmd, false);
          return this.mergeAndResolveStubPromises(result, errorStubs);
        } catch(err) {
            // We don't have language info for this ErrorStub
            console.error(err);
            let stub: ErrorStub = {error: err};
            errorStubs.push(stub);
            return Promise.reject(errorStubs);
        }
      }

      if (retPromise) {
        return retPromise;
      } else {
        // No keyboards added so return empty stub
        return Promise.resolve(errorStubs);
      }
    }

    /**
     *  Request keyboard metadata from the Keyman Cloud keyboard metadata server
     *
     *  @param  {string}   cmd        command string
     *  @param  {boolean?} byLanguage if true, context=languages, else context=keyboards
     *  @returns {Promise<(KeyboardStub[]>} Promise of added keyboard stubs
     **/
    keymanCloudRequest(cmd: string, byLanguage?: boolean): Promise<KeyboardStub[]> {
      var kbdManager = this;
      var keymanweb = this.keymanweb;

      // Some basic support toward #5044, but definitely not a full solution toward it.
      // Wraps the cloud API keyboard-stub request in a Promise, allowing response on network
      // and/or parser errors.  Also detects when `register` returns due to an error case that
      // does not throw errors.  (There are a few such "empty" `return` statements there.)
      const URL='https://api.keyman.com/cloud/4.0/'
                + ((arguments.length > 1) && byLanguage ? 'languages' : 'keyboards');

      let promise = new Promise(function(resolve: (result: KeyboardStub[]) => void, reject: (error: Error) => void) {
        const Lscript: HTMLScriptElement = keymanweb.util._CreateElement('script');

        const queryConfig = '?jsonp=keyman.register&languageidtype=bcp47&version='+keymanweb['version'];

        // Set callback timer
        const timeoutID = window.setTimeout(function() {
          delete kbdManager.registrationResolvers[timeoutID];
          reject(new Error(CLOUD_TIMEOUT_ERR));
        } ,10000);

        // Save the resolve / reject functions.
        kbdManager.registrationResolvers[timeoutID] = {
          resolve: resolve,
          reject: reject
        };

        const tFlag='&timerid='+ timeoutID;

        Lscript.onload = function(event: Event) {
          window.clearTimeout(timeoutID);
        // This case should only happen if a returned, otherwise-valid keyboard 
          // script does not ever call `register`.  Also provides default handling
          // should `register` fail to report results/failure correctly.
          if(kbdManager.registrationResolvers[timeoutID]) {
            try {
              reject(new Error(CLOUD_STUB_REGISTRATION_ERR));
            } finally {
              delete kbdManager.registrationResolvers[timeoutID];
            }
          }
        };

      // Note:  at this time (24 May 2021), this is also happens for "successful" 
        //        API calls where there is no matching keyboard ID.
      //        
        //        The returned 'error' JSON object is sent with an HTML error code (404)
        //        and does not call `keyman.register`.  Even if it did the latter, the
        //        404 code would likely prevent the returned script's call.
      Lscript.onerror = function(event: string | Event, source?: string, 
                                  lineno?: number, colno?: number, error?: Error) {
          window.clearTimeout(timeoutID);
          try {
            let msg = CLOUD_MALFORMED_OBJECT_ERR;
            if(error) {
              msg = msg + ": " + error.message;
            }
            reject(new Error(msg));
          } finally {
            delete kbdManager.registrationResolvers[timeoutID];
          }
        }

        Lscript.src = URL + queryConfig + cmd + tFlag;

        try {
          document.body.appendChild(Lscript);
        } catch(ex) {
          document.getElementsByTagName('head')[0].appendChild(Lscript);
        }
      });
      return promise;
    }

    /**
     * Display warning if language name unavailable to add keyboard
     * @param {string} languageName
     * @returns string of Error message
     */
    private alertLanguageUnavailable(languageName: string): string {
      let msg = 'No keyboards are available for '+ languageName + '. '
        +'Does it have another language name?';
      this.keymanweb.util.internalAlert(msg);
      return msg;
    }

    /**
     *  Display warning if Keyman Cloud server fails to respond
     *
     *  @param  {string}  cmd command string sent to Cloud
     *
     */
    private serverUnavailable(cmd: string): void {
      this.keymanweb.util.internalAlert(cmd == '' ? 'Unable to connect to Keyman Cloud server!' : cmd);
      this.keymanweb.warned=true;
    }

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
