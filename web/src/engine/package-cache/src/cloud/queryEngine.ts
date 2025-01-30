import EventEmitter from 'eventemitter3';

import { PathConfiguration } from 'keyman/engine/paths';

import { default as KeyboardStub, ErrorStub, KeyboardAPISpec } from '../keyboardStub.js';
import { LanguageAPIPropertySpec, ManagedPromise, Version } from '@keymanapp/keyboard-processor';
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

interface EventMap {
  'unboundregister': (registration: ReturnType<CloudQueryEngine['_registerCore']>) => void
}

export default class CloudQueryEngine extends EventEmitter<EventMap> {
  private cloudResolutionPromises: Record<number, ManagedPromise<KeyboardStub[] | ManagedPromise<LanguageAPIPropertySpec[]>>> = {};

  private _languageListPromise: ManagedPromise<LanguageAPIPropertySpec[]>;
  private languageFetchStarted: boolean = false;

  private requestEngine: CloudRequesterInterface;
  private pathConfig: PathConfiguration;

  constructor(requestEngine: CloudRequesterInterface, pathConfig: PathConfiguration) {
    super();

    this.requestEngine = requestEngine;
    this.pathConfig = pathConfig;

    this._languageListPromise = new ManagedPromise<LanguageAPIPropertySpec[]>;
  }

  public get languageListPromise(): Promise<LanguageAPIPropertySpec[]> {
    if(!this.languageFetchStarted) {
      this.languageFetchStarted = true;

      this.keymanCloudRequest('', true).catch((error) => {
        // If promise is not error, then...
        this.languageFetchStarted = false;

        // We should allow retries.
        this._languageListPromise.reject(error);
        this._languageListPromise = new ManagedPromise<LanguageAPIPropertySpec[]>;
      });
    }

    return this._languageListPromise.corePromise;
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

    if(!promiseid) {
      this.emit('unboundregister', result);
      return;
    } else {
      const promise: ManagedPromise<KeyboardStub[]> | ManagedPromise<LanguageAPIPropertySpec[]> = this.cloudResolutionPromises[promiseid];

      if(!promise) {
        this.emit('unboundregister', result);
        return;
      } else {
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

    // Font path defined by cloud entry
    let fontPath=options['fontBaseUri'];

    // or overridden locally, in page source
    if(this.pathConfig.fonts != '') {
      fontPath=this.pathConfig.fonts;
    }
    else {
      // If there's no preconfigured option for font paths, uses the cloud's returned `fontPath` in its place.
      this.pathConfig.updateFontPath(fontPath);
    }

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
      const languageList = (queryResult as CloudLanguagesQueryResult).languages;
      this._languageListPromise.resolve(languageList);
      return languageList;
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
      // Font path defined by cloud entry
      let fontPath = options.fontBaseUri || '';

      const allStubs = KeyboardStub.toStubs(kp, options.keyboardBaseUri, fontPath);

      // May need to filter returned stubs by language
      let lgCode=kbId.split('@')[1];
      if(allStubs.length == 1 && typeof (allStubs[0] as ErrorStub).error != 'undefined') {
        throw (allStubs[0] as ErrorStub).error;
      } else if(typeof(lgCode) != 'string') {
        return allStubs as KeyboardStub[];
      } else {
        lgCode=lgCode.replace(/\$$/,'');
        return [(allStubs as KeyboardStub[]).find((stub) => stub.KLC == lgCode)];
      }
    }
  }

  /**
   * Build 362: addKeyboardArray() link to Cloud. One or more arguments may be used
   *
   * @param   x  keyboard name string or keyboard metadata JSON object
   * @returns resolved or rejected promise with merged array of stubs.
   */
  async fetchCloudStubs(cloudList: string[]): Promise<(KeyboardStub)[]> {
    // // Ensure keymanweb is initialized before continuing to add keyboards
    // if(!this.keymanweb.initialized) {
    //   await this.deferment;
    // }

    if(cloudList.length == 0) {
      return Promise.resolve([]);
    }

    // Update the keyboard metadata list from keyman.com - build the command
    let cmd='&keyboardid=';
    let comma = '';
    for(let i=0; i<cloudList.length; i++) {
      cmd=cmd+comma+cloudList[i].toString();
      comma=',';
    }

    // Request keyboard metadata from the Keyman Cloud keyboard metadata server
    try {
      return await this.keymanCloudRequest(cmd,false);
    } catch(err) {
      // We don't have keyboard info for this ErrorStub
      return Promise.reject(err);
    }
  }
}