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
  keyboardStubs: KeyboardStub[] = [];

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
}