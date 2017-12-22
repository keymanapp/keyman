/// <reference path="kmwbase.ts" />

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

class KeyboardStub {
  'KI': string;
  'KLC': string;

  'KN': string;
  'KL': string;
  'KR': string;
  'KRC': string;

  'KF': string;

  'KFont': KeyboardFont;
  'KOskFont': KeyboardFont;

  // Used when loading a stub's keyboard.
  asyncLoader: any;

  constructor(id: string, langCode: string) {
    this['KI'] = 'Keyboard_' + id;
    this['KLC'] = langCode;
  }
}

class KeyboardManager {
  // Language regions as defined by cloud server
  static readonly regions = ['World','Africa','Asia','Europe','South America','North America','Oceania','Central America','Middle East'];
  static readonly regionCodes = ['un','af','as','eu','sa','na','oc','ca','me'];

  keymanweb: KeymanBase;

  activeStub: KeyboardStub = null;
  keyboardStubs: KeyboardStub[] = [];
  deferredStubs: any[] = []; // The list of user-provided keyboard stub registration objects.

  activeKeyboard: any; // We might can define a more precise def, though...
  keyboards: any[] = [];

  
  languageList: any[] = null; // List of keyboard languages available for KeymanCloud
  languagesPending: any[] = [];     // Array of languages waiting to be registered

  constructor(kmw: KeymanBase) {
    this.keymanweb = kmw;
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
   *  Cross-reference with https://help.keyman.com/developer/engine/web/10.0/reference/core/addKeyboards.
   *  
   *  @param  {Object}  kp  (partial) keyboard meta-data object (`spec` object)
   *  @param  {Object}  lp  language object (`spec.languages` object)
   *  @param  {Object}  options   KeymanCloud callback options
   **/
  mergeStub(kp: any, lp: any, options) {
    var sp: KeyboardStub = this.findStub(kp['id'], lp['id']);

    if(sp == null) {
      sp= new KeyboardStub(kp['id'], lp['id']);
      this.keyboardStubs.push(sp);
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
    if(typeof(sp['KL']) == 'undefined') {
      sp['KL'] = lp['name'];
    }

    if(typeof(sp['KR']) == 'undefined') {
      sp['KR'] = KeyboardManager.regions[rIndex];
    }

    if(typeof(sp['KRC']) == 'undefined') {
      sp['KRC'] = KeyboardManager.regionCodes[rIndex];
    }

    if(typeof(sp['KN']) == 'undefined') {
      sp['KN'] = kp['name'];
    }

    if(typeof(sp['KF']) == 'undefined') {
      rx=RegExp('^(([\\.]/)|([\\.][\\.]/)|(/))|(:)');
      sp['KF'] = kp['filename'];

      if(!rx.test(sp['KF'])) {
        sp['KF'] = options['keyboardBaseUri']+sp['KF'];
      }
    } 
    
    // Font path defined by cloud entry
    var fp, fontPath=options['fontBaseUri'];
    
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
    if(typeof(sp['KFont']) == 'undefined' && typeof(lp['font']) != 'undefined') {
      fp = sp['KFont'] = new KeyboardFont(lp['font'], fontPath);
    }

    // Fixed OSK font issue Github #7 (9/1/2015)
    if(typeof(sp['KOskFont']) == 'undefined' && typeof(lp['oskFont']) != 'undefined') {
      fp = sp['KOskFont'] = new KeyboardFont(lp['oskFont'], fontPath);
    }           

    // Update the UI 
    (<any>this.keymanweb).doKeyboardRegistered(sp['KI'],sp['KL'],sp['KN'],sp['KLC'],sp['KP']);
  }

  /**
   *  Find a keyboard stub by id in the registered keyboards list
   *  
   *  @param  {string}  kid   internal keyboard id (without 'Keyboard_' prefix)
   *  @param  {string}  lgid  language code
   *  
   **/                 
  findStub(kid: string, lgid: string): KeyboardStub {
    var i, ss=this.keyboardStubs;
    for(i=0; i<ss.length; i++) {
      if((ss[i]['KI'] == 'Keyboard_'+kid) && (ss[i]['KLC'] == lgid)) {
        return ss[i];
      }
    }

    return null;
  }

  /**
   * Change active keyboard to keyboard selected by (internal) name and language code
   * 
   *  Test if selected keyboard already loaded, and simply update active stub if so.
   *  Otherwise, insert a script to download and insert the keyboard from the repository
   *  or user-indicated file location. 
   * 
   * @param       {string}    PInternalName
   * @param       {string=}    PLgCode
   * @param       {boolean=}   saveCookie   
   */    
  _SetActiveKeyboard(PInternalName: string, PLgCode?: string, saveCookie?: boolean) {
    var n, Ln;

    var util = this.keymanweb.util;
    var osk = this.keymanweb.osk;

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
      PInternalName=this.keyboardStubs[0]['KI']; PLgCode=this.keyboardStubs[0]['KLC'];   
    }

    // Save name of keyboard (with language code) as a cookie
    if(arguments.length > 2 && saveCookie) {
      this.saveCurrentKeyboard(PInternalName,PLgCode);
    }

    // Check if requested keyboard and stub are currently active
    if(this.activeStub && this.activeKeyboard && this.activeKeyboard['KI'] == PInternalName 
      && this.activeStub['KI'] == PInternalName     //this part of test should not be necessary, but keep anyway
      && this.activeStub['KLC'] == PLgCode && !this.keymanweb.mustReloadKeyboard                                 
      ) return;   

    // Check if current keyboard matches requested keyboard, but not stub
    if(this.activeKeyboard && (this.activeKeyboard['KI'] == PInternalName)) {
      // If so, simply update the active stub
      for(Ln=0; Ln<this.keyboardStubs.length; Ln++) {
        if((this.keyboardStubs[Ln]['KI'] == PInternalName) 
          && (this.keyboardStubs[Ln]['KLC'] == PLgCode)) {
          this.activeStub = this.keyboardStubs[Ln]; 
          
          // Append a stylesheet for this keyboard for keyboard specific styles 
          // or if needed to specify an embedded font
          osk.appendStyleSheet();
          
          // Re-initializate OSK before returning if required
          if(this.keymanweb.mustReloadKeyboard) {
            osk._Load();
          }
          return;
        }
      }
    }

    this.activeKeyboard = null;
    this.activeStub = null;

    // Hide OSK and do not update keyboard list if using internal keyboard (desktops)
    if(PInternalName == '') {
      osk._Hide(false); 

      if(!this.keymanweb.isEmbedded) {
        util.wait(false);
      }

      return;
    }

    // Determine if the keyboard was previously loaded but is not active and use the prior load if so.
    for(Ln=0; Ln<this.keyboards.length; Ln++) { // I1511 - array prototype extended
      if(this.keyboards[Ln]['KI'] == PInternalName) {
        this.activeKeyboard = this.keyboards[Ln];
        (<any>this.keymanweb)._SetTargDir(this.keymanweb._LastActiveElement);  // I2077 - LTR/RTL timing
      
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

    if(this.activeKeyboard == null) {
      for(Ln=0; Ln<this.keyboardStubs.length; Ln++) { // I1511 - array prototype extended
        if((this.keyboardStubs[Ln]['KI'] == PInternalName) 
          && ((this.keyboardStubs[Ln]['KLC'] == PLgCode) || (PLgCode == '---'))) {
          // Force OSK display for CJK keyboards (keyboards using a pick list)
          if(this.isCJK(this.keyboardStubs[Ln]) || util.device.touchable) {
            osk._Enabled = 1;
          }

          // Create a script to load from the server - when it finishes loading, it will register itself, 
          //  detect that it is active, and focus as appropriate. The second test is needed to allow recovery from a failed script load

          // Ensure we're not already loading the keyboard.
          if(!this.keyboardStubs[Ln].asyncLoader) {   
            // Always (temporarily) hide the OSK when loading a new keyboard, to ensure that a failure to load doesn't leave the current OSK displayed
            if(osk.ready) {
              osk._Hide(false);
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
                util.alert(altString || msg, function() {
                  this.keymanweb['setActiveKeyboard'](''); // The API call!
                });
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
            }

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
            window.setTimeout(function(){
              manager.installKeyboard(loadingStub);
            },0);
          }          
          this.activeStub=this.keyboardStubs[Ln];
          return;
        }
      }
      (<any>this.keymanweb)._SetTargDir(this.keymanweb._LastActiveElement);  // I2077 - LTR/RTL timing
    } 

    var Pk=this.activeKeyboard;  // I3319
    if(Pk !== null)  // I3363 (Build 301)
      String.kmwEnableSupplementaryPlane(Pk && ((Pk['KS'] && (Pk['KS'] == 1)) || (Pk['KN'] == 'Hieroglyphic'))); // I3319
    
    // Initialize the OSK (provided that the base code has been loaded)
    osk._Load();
  }

    /**
   * Install a keyboard script that has been downloaded from a keyboard server
   * 
   *  @param  {Object}  kbdStub   keyboard stub to be loaded.
   *    
   **/      
  installKeyboard(kbdStub: KeyboardStub) {
    var util = this.keymanweb.util;
    var osk = this.keymanweb.osk;

    var Lscript = <HTMLScriptElement>(<any>util)._CreateElement('SCRIPT');
    Lscript.charset="UTF-8";        // KMEW-89
    Lscript.type = 'text/javascript';

    var kbdFile = kbdStub['KF'];
    var kbdLang = kbdStub['KL'];
    var kbdName = kbdStub['KN'];

    var manager = this;

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
          (<any>manager.keymanweb).doBeforeKeyboardChange(kbd['KI'],kbdStub['KLC']);
          manager.activeKeyboard=kbd;

          if((<any>manager.keymanweb)._LastActiveElement != null) {
            (<any>manager.keymanweb)._JustActivatedKeymanWebUI = 1;
            (<any>manager.keymanweb)._SetTargDir(manager.keymanweb._LastActiveElement);            
          }

          String.kmwEnableSupplementaryPlane(kbdStub && ((kbdStub['KS'] && (kbdStub['KS'] == 1)) || (kbd['KN'] == 'Hieroglyphic'))); // I3319 - SMP extension, I3363 (Build 301)
          manager.saveCurrentKeyboard(kbd['KI'], kbdStub['KLC']);
        
          // Prepare and show the OSK for this keyboard
          osk._Load();

          // Remove the wait message, if defined
          if(!manager.keymanweb.isEmbedded) {
            util.wait(false);
          }
        } // A handler portion for cases where the new <script> block loads, but fails to process.
      } else {  // Output error messages even when embedded - they're useful when debugging the apps and KMEA/KMEI engines.
          kbdStub.asyncLoader.callback('Error registering the ' + kbdName + ' keyboard for ' + kbdLang + '.', 'error');
      }
      kbdStub.asyncLoader = null; 
    }, false);

    // IE likes to instantly start loading the file when assigned to an element, so we do this after the rest
    // of our setup.  This method is not relocated here (yet) b/c it varies based upon 'native' vs 'embedded'.
    Lscript.src = this.keymanweb.getKeyboardPath(kbdFile);

    try {                                  
      document.body.appendChild(Lscript);  
    }
    catch(ex) {                                                     
      document.getElementsByTagName('head')[0].appendChild(Lscript);
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
  }

  /**
   * Restore the most recently used keyboard, if still available
   */    
  restoreCurrentKeyboard() {
    var stubs = this.keyboardStubs, i, n=stubs.length;

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
    if((i < n) || (this.activeKeyboard == null))
    {
      this._SetActiveKeyboard(t[0],t[1],false);
      this.keymanweb.globalKeyboard = t[0];
      this.keymanweb.globalLanguageCode = t[1];

      (<any>this.keymanweb).doKeyboardChange(t[0],t[1]);        // And update the UI if necessary
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
   * Description Tests if active keyboard (or optional argument) uses a pick list (Chinese, Japanese, Korean, etc.)
   *             (This function accepts either keyboard structure.)   
   */    
  isCJK(k0?: any|KeyboardStub) { // I3363 (Build 301)
    var k = this.activeKeyboard, lg=''; 

    if(arguments.length > 0) {
      k = k0;
    }
    
    if(k) {
      if(typeof(k['KLC']) != 'undefined') {
        lg = k['KLC'];
      } else if(typeof(k['LanguageCode']) != 'undefined') {
        lg = k['LanguageCode'];
      }
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
  isUniqueRequest(cloudList: {id: string, language?: string}[], tEntry: {id: string, language?: string}) {
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
   * Build 362: addKeyboardArray() link to Cloud. One or more arguments may be used
   * 
   * @param {string|Object} x keyboard name string or keyboard metadata JSON object
   * 
   */  
  addKeyboardArray(x: any[]): void {
    // Store all keyboard meta-data for registering later if called before initialization
    if(!this.keymanweb.initialized) {
      for(var k=0; k<x.length; k++) {
        this.deferredStubs.push(x[k]);
      }
      return;
    }

    // Ignore empty array passed as argument
    if(x.length == 0) {
      return;
    }
  
    // Create a temporary array of metadata objects from the arguments used
    var i,j,kp,kbid,lgid,kvid,cmd='',comma='';
    var cloudList: {
      id: string;
      language?: string;
      version?: string;
    }[] = [];

    var tEntry: {
      id: string;
      language?: string;
      version?: string;
    }

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
          tEntry={
            id: pList[0]
          };
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
        var stub: KeyboardStub = <KeyboardStub> x[i];

        if(typeof(x[i]['filename']) == 'string') {                                                   
          if(!this.addStub(x[i])) {
            alert('To use a custom keyboard, you must specify file name, keyboard name, language, language code and region code.');
          }
        } else {
          if(x[i]['language']) {
            console.warn("The 'language' property for keyboard stubs has been deprecated.  Please use the 'languages' property instead.");
            x[i]['languages'] = x[i]['language'];
          }

          lList=x[i]['languages'];
              
          //Array or single entry?
          if(typeof(lList.length) == 'number') {
            for(j=0; j<lList.length; j++) {
              tEntry = {id:x[i]['id'],language:x[i]['languages'][j]['id']};
              if(this.isUniqueRequest(cloudList, tEntry)) {
                cloudList.push(tEntry);
              }
            }
          } else { // Single language element
            tEntry = {id:x[i]['id'],language:x[i]['languages'][j]['id']};
            if(this.isUniqueRequest(cloudList, tEntry)) {
              cloudList.push(tEntry);
            }
          }
        }
      }
    }
    
    // Return if all keyboards being registered are local and fully specified
    if(cloudList.length == 0) {
      return;
    }

    // Update the keyboard metadata list from keyman.com - build the command
    cmd='&keyboardid=';
    for(i=0; i<cloudList.length; i++) {      
      kp=cloudList[i];
      kbid=kp['id']; lgid=''; kvid='';  
      if(typeof(kp['language']) == 'string' && kp['language'] != '') {
        lgid=kp['language'];
      }
      if(typeof(kp['version']) == 'string' && kp['version'] != '') {
        kvid=kp['version'];
      }
      if(lgid != '') {
        kbid=kbid+'@'+lgid;
        if(kvid != '') {
          kbid=kbid+'@'+kvid;
        }
      } else {
        if(kvid != '') {
          kbid=kbid+'@@'+kvid;
        }
      }

      //TODO: add specifier validation...        
              
      cmd=cmd+comma+kbid;
      comma=',';
    }  
    
    // Request keyboard metadata from the Keyman Cloud keyboard metadata server
    this.keymanCloudRequest(cmd,false);
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
    var options=x['options'];
  
    // Always clear the timer associated with this callback
    if(x['timerid']) {
      window.clearTimeout(x['timerid']);
    }
    
    // Indicate if unable to register keyboard
    if(typeof(x['error']) == 'string') {
      var badName='';
      if(typeof(x['keyboardid']) == 'string') {
        badName = x['keyboardid'].substr(0,1).toUpperCase()+x['keyboardid'].substr(1);
      }

      (<any>this.keymanweb).serverUnavailable(badName+' keyboard not found.');
      return;
    }
    
    // Ignore callback unless the context is defined
    if(typeof(options) == 'undefined' || typeof(options['context']) == 'undefined') {
      return;
    }

    // Register each keyboard for the specified language codes
    if(options['context'] == 'keyboard') {
      var i,kp=x['keyboard'];
      // Process array of keyboard definitions
      if(typeof(kp.length) == 'number') {
        for(i=0; i<kp.length; i++) {
          this.registerLanguagesForKeyboard(kp[i],options,i);
        }
      } else { // Process a single keyboard definition
        this.registerLanguagesForKeyboard(kp,options,0);
      }
    } else if(options['context'] == 'language') { // Download the full list of supported keyboard languages
      this.languageList = x['languages'];
      if(this.languagesPending) {
        this.addLanguageKeyboards(this.languagesPending);
      }
      this.languagesPending = [];      
    }
  }

  /**
   *  Add default or all keyboards for a given language
   *  
   *  @param  {Object}   languages    Array of language names
   **/           
  addLanguageKeyboards(languages) {
    var i, j, lgName, cmd, first, addAll;
    
    // Defer registering keyboards by language until the language list has been loaded
    if(this.languageList == null) {                       
      first = (this.languagesPending.length == 0);

      for(i=0; i<languages.length; i++) {
        this.languagesPending.push(languages[i]);
      }

      if(first) {
        this.keymanCloudRequest('',true);
      }   
    } else { // Identify and register each keyboard by language name
      cmd = '';
      for(i=0; i<languages.length; i++) {
        lgName = languages[i].toLowerCase();
        addAll = (lgName.substr(-1,1) == '$'); 
        if(addAll) {
          lgName = lgName.substr(0,lgName.length-1);
        }

        for(j=0; j<this.languageList.length; j++) {
          if(lgName == this.languageList[j]['name'].toLowerCase()) {
            if(cmd != '') {
              cmd = cmd + ',';
            }

            cmd = cmd+'@'+this.languageList[j]['id'];
            if(addAll) {
              cmd = cmd + '$';
            }

            break;
          }
        }
      }

      if(cmd == '') {
        this.keymanweb.util.alert('No keyboards are available for '+languages[0]+'. '
          +'Does it have another language name?');
      } else {
        this.keymanCloudRequest('&keyboardid='+cmd,false);
      }
    }
  }

  /**
   *  Request keyboard metadata from the Keyman Cloud keyboard metadata server
   *   
   *  @param  {string}   cmd        command string
   *  @param  {boolean?} byLanguage if true, context=languages, else context=keyboards 
   **/
  keymanCloudRequest(cmd: string, byLanguage?: boolean) {         
    var URL='https://r.keymanweb.com/api/4.0/', tFlag, 
      Lscript = <HTMLScriptElement> this.keymanweb.util._CreateElement('SCRIPT');
    
    URL = URL + ((arguments.length > 1) && byLanguage ? 'languages' : 'keyboards')
      +'?jsonp=keyman.register';  
    
    // Set callback timer
    tFlag='&timerid='+window.setTimeout(
      function(){
        this.serverUnavailable(cmd);
      }
      ,10000);    
  
    Lscript.charset="UTF-8";                     
    Lscript.src = URL+cmd+tFlag;       
    Lscript.type = 'text/javascript';       
    try {                                  
      document.body.appendChild(Lscript);  
      }
    catch(ex) {                                                     
      document.getElementsByTagName('head')[0].appendChild(Lscript);
      }                  
  }

  /**
   *  Display warning if Keyman Cloud server fails to respond
   * 
   *  @param  {string}  cmd command string sent to Cloud
   *          
   **/       
  private serverUnavailable(cmd) {
    this.keymanweb.util.alert(cmd == '' ? 'Unable to connect to Keyman Cloud server!' : cmd);
    this.keymanweb.warned=true;
  }  

}