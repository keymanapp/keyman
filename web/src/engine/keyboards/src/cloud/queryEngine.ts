import { default as KeyboardStub, ErrorStub, KeyboardAPISpec, mergeAndResolveStubPromises } from '../keyboardStub.js';
import { KeyboardFont, LanguageAPIPropertySpec, ManagedPromise, Version } from '@keymanapp/keyboard-processor';
import CloudRequesterInterface from './requesterInterface.js';

// For when the API call straight-up times out.
export const CLOUD_TIMEOUT_ERR = "The Cloud API request timed out.";
// Currently cannot distinguish between "no matching keyboard" and other script-load errors.
export const CLOUD_MALFORMED_OBJECT_ERR = "Could not find a keyboard with that ID.";
// Represents unspecified errors that occur when registering the results of a successful API call.
export const CLOUD_STUB_REGISTRATION_ERR = "The Cloud API failed to find an appropriate keyboard.";
// Represents custom, specified KMW errors that occur when registering the results of a successful API call.
export const CLOUD_REGISTRATION_ERR = "Error occurred while registering keyboards: ";

export const MISSING_KEYBOARD = function(kbdid: string) {
  return kbdid + ' keyboard not found.';
}

type CloudAPIFont = {
  family: string,
  source: string | string[]
}

type CloudQueryOptions = {
  context: 'keyboard' | 'language';
  keyboardid?: string,
  keyboardBaseUri?: string,
  fontBaseUri?: string
}

type CloudKeyboardQueryResult = {
  /**
   * A 1D array is returned for keyboard-id based queries: `addKeyboards('sil_cameroon_qwerty')`
   * returns a single array with one entry.
   *
   * A 2D array is returned for language-code based keyboard queries:  `addKeyboards('@fr,@en')`
   * returns two arrays of keyboards, one per language code.
   * - First index = fr
   * - Second = en
   */
  keyboard: KeyboardAPISpec | KeyboardAPISpec[] | KeyboardAPISpec[][],
  options: { context: 'keyboard' } & CloudQueryOptions,
  error?: string,
  timerid: string
};

type CloudLanguagesQueryResult = {
  languages: LanguageAPIPropertySpec[],
  options: { context: 'language' } & CloudQueryOptions,
  error?: string,
  keyboardid?: string,
  timerid: string
}

export type CloudQueryResult = CloudKeyboardQueryResult | CloudLanguagesQueryResult;

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

export default class CloudQueryEngine {
  // Language regions as defined by cloud server
  static readonly regions = ['World','Africa','Asia','Europe','South America','North America','Oceania','Central America','Middle East'];
  static readonly regionCodes = ['un','af','as','eu','sa','na','oc','ca','me'];

  private cloudResolutionPromises: Record<number, ManagedPromise<KeyboardStub[] | ManagedPromise<LanguageAPIPropertySpec[]>>> = {};

  private languageList: LanguageAPIPropertySpec[];

  private requestEngine: CloudRequesterInterface;

  constructor(requestEngine: CloudRequesterInterface) {
    this.requestEngine = requestEngine;
  }

  /**
   *  Request keyboard metadata from the Keyman Cloud keyboard metadata server
   *
   *  @param  {string}   cmd        command string
   *  @param  {boolean?} byLanguage if true, context=languages, else context=keyboards
   *  @returns {Promise<(KeyboardStub[]>} Promise of added keyboard stubs
   **/
  keymanCloudRequest(cmd: string, byLanguage: true): Promise<LanguageAPIPropertySpec[]>;
  keymanCloudRequest(cmd: string, byLanguage: false): Promise<KeyboardStub[]>;
  keymanCloudRequest(cmd: string, byLanguage?: boolean): Promise<KeyboardStub[]> | Promise<LanguageAPIPropertySpec[]> {
    // Some basic support toward #5044, but definitely not a full solution toward it.
    // Wraps the cloud API keyboard-stub request in a Promise, allowing response on network
    // and/or parser errors.  Also detects when `register` returns due to an error case that
    // does not throw errors.  (There are a few such "empty" `return` statements there.)
    const URL='https://api.keyman.com/cloud/4.0/'
              + ((arguments.length > 1) && byLanguage ? 'languages' : 'keyboards');


    const queryConfig = '?jsonp=keyman.register&languageidtype=bcp47&version='+Version.CURRENT.toString();

    const query = URL + queryConfig + cmd;

    let { promise, queryId } = this.requestEngine.request(query);
    this.cloudResolutionPromises[queryId] = promise as any;

    promise.finally(() => {
      delete this.cloudResolutionPromises[queryId];
    });

    return promise.corePromise as any;
  }

  /**
   * Call back from cloud for adding keyboard metadata
   *
   * This function should be published to scripts returned from the cloud;
   * they will expect to call it as `keyman.register`.
   *
   * @param {Object}    x   metadata object
   **/
  registerFromCloud = (x: CloudQueryResult) => {
    const promiseid = x.timerid;

    let result: KeyboardStub[] | LanguageAPIPropertySpec[] | Error;
    try {
      result = this._registerCore(x);
    } catch(err) {
      result = new Error(CLOUD_REGISTRATION_ERR + err);
    }

    if(promiseid) {
      const promise: ManagedPromise<KeyboardStub[]> | ManagedPromise<LanguageAPIPropertySpec[]> = this.cloudResolutionPromises[promiseid];

      if(promise) {
        try {
          if(result instanceof Error) {
            promise.reject(result as Error);
          } else {
            promise.resolve(result as any);
          }
        } finally {
          delete this.cloudResolutionPromises[promiseid];
        }
      }
    }
  }

  /**
   * Call back from cloud for adding keyboard metadata
   *
   * @param {Object}    queryResult   metadata object
   **/
  private _registerCore(queryResult: CloudQueryResult): KeyboardStub[] | LanguageAPIPropertySpec[] | Error { // TODO (#5044): should return heterogenous type; allow array of stubs.
    const options: CloudQueryOptions = queryResult.options;

    // Indicate if unable to register keyboard
    if(typeof(queryResult.error) == 'string') {
      // Currently unreachable (24 May 2021 - API returns a 404; returned 'script' does not call register)
      var badName='';
      if(typeof(queryResult.options.keyboardid) == 'string') {
        let keyboardId = queryResult.options.keyboardid;
        badName = keyboardId.substring(0,1).toUpperCase() + keyboardId.substring(1);
      }

      return new Error(MISSING_KEYBOARD(badName));
    }

    // Ignore callback unless the context is defined
    if(typeof(options) == 'undefined' || typeof(options['context']) == 'undefined') {
      return new Error(CLOUD_MALFORMED_OBJECT_ERR);
    }

    // Register each keyboard for the specified language codes
    let stubs: KeyboardStub[] = [];

    if(options.context == 'keyboard') {
      let i, kp=(queryResult as CloudKeyboardQueryResult).keyboard;
      // Process array of keyboard definitions
      if(Array.isArray(kp)) {
        for(i=0; i<kp.length; i++) {
          // Note:  if an invalid language code is specified, the elements here may be
          //        empty arrays.  Will not report an error if so.
          stubs = stubs.concat(CloudQueryEngine.registerLanguagesForKeyboard(kp[i],options,i));
        }
      } else { // Process a single keyboard definition
        stubs = stubs.concat(CloudQueryEngine.registerLanguagesForKeyboard(kp,options,0));
      }
    } else if(options.context == 'language') { // Download the full list of supported keyboard languages
      return this.languageList = (queryResult as CloudLanguagesQueryResult).languages;
    }

    // Return any new entries to the `keyboardStubs` array.
    return stubs;
  }


  /**
   *  Register a keyboard for each associated language
   *
   *  @param  {Object}  kp  Keyboard Object or Object array
   *  @param  {Object}  options   keymanCloud callback options
   *  @param  {number}  nArg  keyboard index in argument array
   *
   **/
  private static registerLanguagesForKeyboard(kp: KeyboardAPISpec | KeyboardAPISpec[], options: CloudQueryOptions, nArg: number): KeyboardStub[] {
    let kbId='';

    // Do not attempt to process badly formatted requests
    if(typeof(kp) == 'undefined') {
      return [];
    }

    if(typeof(options.keyboardid) == 'string') {
      kbId = options.keyboardid.split(',')[nArg];
    }

    // When keyboards requested by language code, several keyboards may be returned as an array
    if(Array.isArray(kp)) {
      // If language code is suffixed by $, register all keyboards for this language
      if(kp.length == 1 || kbId.substr(-1,1) == '$' || kbId == '') {
        let stubs: KeyboardStub[] = [];

        for(let i=0; i<kp.length; i++) {
          stubs = stubs.concat(this.registerLanguagesForKeyboard(kp[i],options,nArg));
        }

        return stubs;
      } else {
        let nDflt = 0;

        // Register the default keyboard for the language code
        // Until a default is defined, the default will be the Windows keyboard,
        // that is, the keyboard named for the language (exception: English:US), or the
        // first keyboard found.

        for(let i=0; i<kp.length; i++) {
          let id=kp[i].id.toLowerCase();
          if(id == 'us') {
            id='english';
          }

          if(Array.isArray(kp[i].languages)) {
            let langArray = kp[i].languages as LanguageAPIPropertySpec[];
            for(let j=0; j < langArray.length; j++) {
              if(id == langArray[j].name.toLowerCase()) {
                nDflt = i;
                break;
              }
            }
          } // else nDflt === 0 already.
        }

        return this.registerLanguagesForKeyboard(kp[nDflt],options,nArg);
      }
    } else { // Otherwise, process a single keyboard for the specified languages
      let stubs: KeyboardStub[] = [];

      // May need to filter returned stubs by language
      let lgCode=kbId.split('@')[1];
      if(typeof(lgCode) == 'string') {
        lgCode=lgCode.replace(/\$$/,'');
      }

      // Can only add keyboard stubs for defined languages
      let ll=kp.languages;
      if(typeof(ll) != 'undefined') {
        if(Array.isArray(ll)) {
          for(let i=0; i<ll.length; i++) {
            if(typeof(lgCode) == 'undefined' || ll[i]['id'] == lgCode) {
              stubs.push(CloudQueryEngine.buildCloudStub(kp, ll[i], options));
            }
          }
        } else {
          stubs.push(CloudQueryEngine.buildCloudStub(kp, ll, options));
        }
      }

      return stubs;
    }
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
  private static buildCloudStub(kp: KeyboardAPISpec, lp: LanguageAPIPropertySpec, options: CloudQueryOptions): KeyboardStub {
    let sp: KeyboardStub = new KeyboardStub(kp['id'], lp['id']);

    // Accept region as number (from Cloud server), code, or name
    let region=lp['region'], rIndex=0;
    if(typeof(region) == 'number') {
      if(region < 1 || region > 9) {
        rIndex = 0;
      } else {
        rIndex = region-1;
      }
    } else if(typeof(region) == 'string') {
      let list = (region.length == 2 ? CloudQueryEngine.regionCodes : CloudQueryEngine.regions);
      for(let i=0; i<list.length; i++) {
        if(region.toLowerCase() == list[i].toLowerCase()) {
          rIndex=i;
          break;
        }
      }
    }

    let rx: RegExp;

    sp['KL'] = (typeof sp['KL'] === 'undefined') ? lp['name'] : sp['KL'];
    sp['KR'] = (typeof sp['KR'] === 'undefined') ? CloudQueryEngine.regions[rIndex] : sp['KR'];
    sp['KRC'] = (typeof sp['KRC'] === 'undefined') ? CloudQueryEngine.regionCodes[rIndex] : sp['KRC'];
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
    let fontPath=options['fontBaseUri'];

    // // or overridden locally, in page source  (do "in post", outside of the cloud query class)
    // if(this.keymanweb.options['fonts'] != '') {
    //   fontPath=this.keymanweb.options['fonts'];
    //   rx=new RegExp('^https?\\:');
    //   if(!rx.test(fontPath)) {
    //     if(fontPath.substr(0,2) == '//') {
    //       fontPath = this.keymanweb.protocol + fontPath;
    //     } else if(fontPath.substr(0,1) == '/') {
    //       fontPath = this.keymanweb.rootPath + fontPath.substr(1);
    //     } else {
    //       fontPath = this.keymanweb.rootPath + fontPath;
    //     }
    //   }
    // } else {
    //   this.keymanweb.options.fonts=fontPath;
    // }

    function convertToStubFont(fontObj: CloudAPIFont, path: string): KeyboardFont {
      return {
        family: fontObj.family,
        files: typeof fontObj.source == 'string' ? fontObj.source : fontObj.source[0],
        path: path
      }
    }

    // Add font specifiers where necessary and not overridden by user
    if(typeof(lp['font']) != 'undefined') {
      sp['KFont'] = (typeof sp['KFont'] === 'undefined') ? convertToStubFont(lp['font'] as CloudAPIFont, fontPath) : sp['KFont'];
    }

    // Fixed OSK font issue Github #7 (9/1/2015)
    if(typeof(lp['oskFont']) != 'undefined') {
      sp['KOskFont'] = (typeof sp['KOskFont'] === 'undefined') ? convertToStubFont(lp['oskFont'] as CloudAPIFont, fontPath) : sp['KOskFont'];
    }

    return sp;

    // // Update the UI
    // this.doKeyboardRegistered(sp['KI'],sp['KL'],sp['KN'],sp['KLC'],sp['KP']);
  }

  /**
   * Build 362: addKeyboardArray() link to Cloud. One or more arguments may be used
   *
   * @param x  keyboard name string or keyboard metadata JSON object
   * @returns resolved or rejected promise with merged array of stubs.
   */
  async fetchCloudStubs(x: string[]): Promise<(KeyboardStub|ErrorStub)[]> {
    // // Ensure keymanweb is initialized before continuing to add keyboards
    // if(!this.keymanweb.initialized) {
    //   await this.deferment;
    // }

    // Ignore empty array passed as argument
    if(x.length == 0) {
      let stub: ErrorStub = {error: new Error("No keyboards to add")}
      let errorStubs = [stub];
      // Normally reject error, but this can be a warning
      return Promise.resolve(errorStubs);
    }

    // Create a temporary array of metadata objects from the arguments used
    let cloudList: CloudRequestEntry[] = [];
    let tEntry: CloudRequestEntry;

    for(let i=0; i<x.length; i++) {
      let pList=x[i].split('@'), lList=[''];
      if(pList[0].toLowerCase() == 'english') {
        pList[0] = 'us';
      }

      if(pList.length > 1) {
        lList=pList[1].split(',');
      }

      for(let j=0; j<lList.length; j++) {
        tEntry = new CloudRequestEntry(pList[0]);

        if(lList[j] != '') {
          tEntry.language=lList[j];
        }

        if(pList.length > 2) {
          tEntry.version=pList[2];
        }

        // If we've already registered or requested a stub for this keyboard-language pairing,
        // don't bother with a cloud request.
        // if(this.isUniqueRequest(cloudList, tEntry)) {
        if(!cloudList.find((entry) => entry.id == tEntry.id && entry.language == tEntry.language)) {
          cloudList.push(tEntry);
        }
      }
    }

    // Return if all keyboards being registered are local and fully specified
    try {
      if(cloudList.length == 0) {
        return mergeAndResolveStubPromises([], []);
      }
    } catch (error) {
      console.error(error);
      return Promise.reject(error);
    }

    // Update the keyboard metadata list from keyman.com - build the command
    let cmd='&keyboardid=';
    let comma = '';
    for(let i=0; i<cloudList.length; i++) {
      cmd=cmd+comma+cloudList[i].toString();
      comma=',';
    }

    let errorStubs: ErrorStub[] = [];
    // Request keyboard metadata from the Keyman Cloud keyboard metadata server
    try {
      let result: (KeyboardStub|ErrorStub)[]|Error = await this.keymanCloudRequest(cmd,false);
      return mergeAndResolveStubPromises(result, errorStubs);
    } catch(err) {
      // We don't have keyboard info for this ErrorStub
      console.error(err);
      let stub: ErrorStub = {error: err};
      errorStubs.push(stub);
      return Promise.reject(errorStubs);
    }
  }

  // ------------------------------------------------------------------------------

  // /**
  //  *  Find a keyboard stub by id in the registered keyboards list
  //  *
  //  *  @param  {string}  kid   internal keyboard id (without 'Keyboard_' prefix)
  //  *  @param  {string}  lgid  language code
  //  *
  //  **/
  // findStub(kid: string, lgid: string): KeyboardStub {
  //   var i;
  //   for(i=0; i<this.keyboardStubs.length; i++) {
  //     if((this.keyboardStubs[i]['KI'] == 'Keyboard_'+kid) && (this.keyboardStubs[i]['KLC'] == lgid)) {
  //       return this.keyboardStubs[i];
  //     }
  //   }

  //   return null;
  // }

  // /**
  //  * Build 362: addKeyboardArray() link to Cloud. One or more arguments may be used
  //  *
  //  * @param x  keyboard name string or keyboard metadata JSON object
  //  * @returns resolved or rejected promise with merged array of stubs.
  //  */
  // async addKeyboardArray(x: (string|KeyboardStub)[]): Promise<(KeyboardStub|ErrorStub)[]> {
  //   let errorStubs: ErrorStub[] = [];

  //   // Ensure keymanweb is initialized before continuing to add keyboards
  //   if(!this.keymanweb.initialized) {
  //     await this.deferment;
  //   }

  //   // Ignore empty array passed as argument
  //   if(x.length == 0) {
  //     let stub: ErrorStub = {error: new Error("No keyboards to add")}
  //     errorStubs.push(stub);
  //     // Normally reject error, but this can be a warning
  //     return Promise.resolve(errorStubs);
  //   }

  //   // Create a temporary array of metadata objects from the arguments used
  //   var i,j,cmd='',comma='';
  //   var cloudList: CloudRequestEntry[] = [];
  //   let keyboardStubs: KeyboardStub[] = [];
  //   var tEntry: CloudRequestEntry;

  //   for(i=0; i<x.length; i++) {
  //     if(typeof(x[i]) == 'string' && (<string>x[i]).length > 0) {
  //       var pList=(<string>x[i]).split('@'),lList=[''];
  //       if(pList[0].toLowerCase() == 'english') {
  //         pList[0] = 'us';
  //       }

  //       if(pList.length > 1) {
  //         lList=pList[1].split(',');
  //       }

  //       for(j=0; j<lList.length; j++) {
  //         tEntry = new CloudRequestEntry(pList[0]);

  //         if(lList[j] != '') {
  //           tEntry.language=lList[j];
  //         }

  //         if(pList.length > 2) {
  //           tEntry.version=pList[2];
  //         }

  //         // If we've already registered or requested a stub for this keyboard-language pairing,
  //         // don't bother with a cloud request.
  //         if(this.isUniqueRequest(cloudList, tEntry)) {
  //           cloudList.push(tEntry);
  //         }
  //       }
  //     }
  //     if(typeof(x[i]) == 'object' && x[i] != null) {
  //       // Register any local keyboards immediately:
  //       // - must specify filename, keyboard name, language codes, region codes
  //       // - no request will be sent to cloud
  //       if(typeof(x[i]['filename']) == 'string') {
  //         if(!this.addStub(x[i])) {
  //           this.keymanweb.util.internalAlert('To use a custom keyboard, you must specify file name, keyboard name, language, language code and region code.');
  //         }
  //       } else {
  //         if(x[i]['language']) {
  //           console.warn("The 'language' property for keyboard stubs has been deprecated.  Please use the 'languages' property instead.");
  //           x[i]['languages'] = x[i]['language'];
  //         }

  //         lList=x[i]['languages'];
  //         if (!lList) {
  //           let msg = 'To use keyboard \'' + x[i]['id'] + '\', you must specify languages.';
  //           let e: ErrorStub = {
  //             keyboard: {
  //               id : x[i]['id'],
  //               name: x[i]['name']
  //             },
  //             error: new Error(msg)
  //           };
  //           errorStubs.push(e);
  //           continue;
  //         }

  //         //Array or single entry?
  //         if(typeof(lList.length) == 'number') {
  //           for(j=0; j<lList.length; j++) {
  //             tEntry = new CloudRequestEntry(x[i]['id'], x[i]['languages'][j]['id']);
  //             if(this.isUniqueRequest(cloudList, tEntry)) {
  //               cloudList.push(tEntry);
  //             }
  //           }
  //         } else { // Single language element
  //           tEntry = new CloudRequestEntry(x[i]['id'], x[i]['languages']['id']);
  //           if(this.isUniqueRequest(cloudList, tEntry)) {
  //             cloudList.push(tEntry);
  //           }
  //         }
  //       }

  //       // Convert stub from one-to-many KeyboardStub[]
  //       let convertedStubs = KeyboardStub.toStubs(x[i]);
  //       convertedStubs.forEach(s => {
  //         if (s instanceof KeyboardStub) {
  //           keyboardStubs.push(s);
  //         } else {
  //           errorStubs.push(s);
  //         }
  //       })
  //     }
  //   }

  //   // Return if all keyboards being registered are local and fully specified
  //   try {
  //     if(cloudList.length == 0) {
  //       return this.mergeAndResolveStubPromises(keyboardStubs, errorStubs);
  //     }
  //   } catch (error) {
  //     console.error(error);
  //     return Promise.reject(error);
  //   }

  //   // Update the keyboard metadata list from keyman.com - build the command
  //   cmd='&keyboardid=';
  //   for(i=0; i<cloudList.length; i++) {
  //     cmd=cmd+comma+cloudList[i].toString();
  //     comma=',';
  //   }

  //   // Request keyboard metadata from the Keyman Cloud keyboard metadata server
  //   try {
  //     let result: (KeyboardStub|ErrorStub)[]|Error = await this.keymanCloudRequest(cmd,false);
  //     return this.mergeAndResolveStubPromises(result, errorStubs);
  //   } catch(err) {
  //     // We don't have keyboard info for this ErrorStub
  //     console.error(err);
  //     let stub: ErrorStub = {error: err};
  //     errorStubs.push(stub);
  //     return Promise.reject(errorStubs);
  //   }

  //   // no keyboards added so return empty stub
  //   return Promise.resolve(errorStubs);
  // }

  // /**
  //  * Returns a Promise of the merged keyboard stubs and error stubs.
  //  *
  //  * If the keyboard stub array is empty, will return a rejected Promise,
  //  * otherwise returns a resolved Promise.
  //  *
  //  * @param keyboardStubs  array of keyboard stubs to merge.
  //  * @param errorStubs     array of error stubs to merge.
  //  * @returns  resolved or rejected promise with merged array of stubs.
  //  */
  // private mergeAndResolveStubPromises(keyboardStubs: (KeyboardStub|ErrorStub)[], errorStubs: ErrorStub[]) :
  //     Promise<(KeyboardStub|ErrorStub)[]> {
  //   if (errorStubs.length == 0) {
  //     return Promise.resolve(keyboardStubs);
  //   } if (keyboardStubs.length == 0) {
  //     return Promise.reject(errorStubs);
  //   } else {
  //     // Merge this with errorStubs
  //     let result: (KeyboardStub|ErrorStub)[] = keyboardStubs;
  //     return Promise.resolve(result.concat(errorStubs));
  //   }
  // }

  // /**
  //  * Function       isUniqueRequest
  //  * Scope          Private
  //  * @param         {Object}    tEntry
  //  * Description    Checks to ensure that the stub isn't already loaded within KMW or subject
  //  *                to an already-pending request.
  //  */
  // isUniqueRequest(cloudList: {id: string, language?: string}[], tEntry: CloudRequestEntry) {
  //   var k;

  //   if(this.findStub(tEntry.id, tEntry.language) == null) {
  //     for(k=0; k < cloudList.length; k++) {
  //       if(cloudList[k].id == tEntry['id'] && cloudList[k].language == tEntry.language) {
  //         return false;
  //       }
  //     }
  //     return true;
  //   } else {
  //     return false;
  //   }
  // };

  // /**
  //  * Register a fully specified keyboard (add meta-data for each language) immediately
  //  *
  //  * @param  {Object}  arg
  //  * @returns {boolean}
  //  **/
  // addStub(arg: any): boolean {
  //   if(typeof(arg['id']) != 'string') {
  //     return false;
  //   }

  //   if(typeof(arg['language']) != "undefined") {
  //     console.warn("The 'language' property for keyboard stubs has been deprecated.  Please use the 'languages' property instead.");
  //     arg['languages'] = arg['language'];
  //   }

  //   if(typeof(arg['languages']) == 'undefined') {
  //     return false;
  //   }

  //   // Default the keyboard name to its id, capitalized
  //   if(typeof(arg['name']) != 'string') {
  //     arg['name'] = arg['id'].replace('_',' ');
  //     arg['name'] = arg['name'].substr(0,1).toUpperCase()+arg['name'].substr(1);
  //   }

  //   var lgArg=arg['languages'];
  //   var lgList=[], i, lg;
  //   if(typeof(lgArg.length) == 'undefined') {
  //     lgList[0] = lgArg;
  //   } else {
  //     lgList = lgArg;
  //   }

  //   var localOptions={
  //     'keyboardBaseUri': this.keymanweb.options['keyboards'],
  //     'fontBaseUri': this.keymanweb.options['fonts']
  //   };

  //   // Add a stub for each correctly specified language
  //   for(i=0; i<lgList.length; i++) {
  //     this.mergeStub(arg, lgList[i], localOptions);
  //   }

  //   return true;
  // }
}