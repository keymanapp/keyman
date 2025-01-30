import {
  KeyboardAPIPropertySpec,
  KeyboardLoaderBase as KeyboardLoader,
  LanguageAPIPropertySpec,
  RawKeyboardMetadata
} from "@keymanapp/keyboard-processor";
import { PathConfiguration } from "keyman/engine/paths";

// TODO:  is cleanup needed here, to use local paths instead?
import {
  CloudQueryEngine,
  type ErrorStub,
  KeyboardAPISpec,
  KeyboardStub,
  StubAndKeyboardCache,
  RawKeyboardStub,
  mergeAndResolveStubPromises,
  toUnprefixedKeyboardId as unprefixed
 } from "./index.js";
import { default as CloudRequesterInterface } from "./cloud/requesterInterface.js";
import { rejectErrorStubs } from "./keyboardStub.js";

class CloudRequestEntry {
  id: string;
  language?: string;
  version?: string;

  constructor(id: string, language?: string) {
    this.id = id;
    this.language = language;
  }

  toString(): string {
    let str=this.id;

    if(this.language) {
      str=str+'@'+this.language;
      if(this.version) {
        str=str+'@'+this.version;
      }
    } else {
      if(this.version) {
        str=str+'@@'+this.version;
      }
    }

    //TODO: add specifier validation...

    return str;
  }
}

function convertToErrorStub(stub: {id: string, name: string}, err: Error | string): ErrorStub {
  return {
    keyboard: {
      id : stub.id,
      name: stub.name
    },
    error: typeof err == 'string' ? new Error(err) : err
  };
}

function toQuerySpecs(id: string, langId: string, version?: string): CloudRequestEntry {
  let obj = new CloudRequestEntry(id, langId);
  if(version) {
    obj.version = version;
  }
  return obj;
}

/**
 * Function       isUniqueRequest
 * Scope          Private
 * @param         {Object}    tEntry
 * Description    Checks to ensure that the stub isn't already loaded within KMW or subject
 *                to an already-pending request.
 */
function isUniqueRequest(cache: StubAndKeyboardCache, cloudList: {id: string, language?: string}[], tEntry: CloudRequestEntry) {
  if(cache.getStub(tEntry.id, tEntry.language) == null) {
    for(let k=0; k < cloudList.length; k++) {
      if(cloudList[k].id == tEntry['id'] && cloudList[k].language == tEntry.language) {
        return false;
      }
    }
    return true;
  } else {
    return false;
  }
};

// TODO:  Move to the keyboard-cache child project - we can test it headlessly there!
export default class KeyboardRequisitioner {
  readonly cache: StubAndKeyboardCache;
  readonly cloudQueryEngine: CloudQueryEngine;
  readonly pathConfig: PathConfiguration;

  constructor(keyboardLoader: KeyboardLoader, keyboardRequester: CloudRequesterInterface, pathConfig: PathConfiguration) {
    this.pathConfig = pathConfig;
    this.cache = new StubAndKeyboardCache(keyboardLoader);
    this.cloudQueryEngine = new CloudQueryEngine(keyboardRequester, this.pathConfig);

    // Handles keymanweb.com's precached keyboard array.  There is no associated promise,
    // so there's nothing handling the `register` call's results otherwise.
    this.cloudQueryEngine.on('unboundregister', (registration) => {
      // Internal, undocumented use-case of `keyman.register`:  precached keyboard loading
      // Other uses may trigger errors, especially if there's a type-structure mismatch.
      // Those errors should not be handled here; let them surface.
      if(Array.isArray(registration)) {
        registration.forEach((entry) => {
          this.cache.addStub(entry);
        });
      }
    });
  }

  addKeyboardArray(x: (string|RawKeyboardMetadata)[]): Promise<(KeyboardStub | ErrorStub)[]> {
    let completeStubs: KeyboardStub[] = [];
    let incompleteStubs: KeyboardStub[] = [];
    let stubs: KeyboardStub[] = [];
    let identifiers: string[] = [];
    let errorStubs: ErrorStub[] = [];

    // #region Parameter preprocessing: is incoming data already 'complete', or do we need to fetch the 'complete' version?

    for(let entry of x) {
      if(typeof entry == 'string' && entry.length > 0) {
        identifiers.push(entry);
      } else { // is some sort of object.
        if(entry['KI'] || entry['KL'] || entry['KLC'] || entry['KFont'] || entry['KOskFont']) {
          stubs.push(new KeyboardStub(entry as RawKeyboardStub));
        } else {
          let apiSpecEntry = entry as KeyboardAPISpec;
          if(typeof(apiSpecEntry.language) != "undefined") {
            console.warn("The 'language' property for keyboard stubs has been deprecated.  Please use the 'languages' property instead.");
          }
          apiSpecEntry.languages ||= apiSpecEntry.language;

          if(typeof apiSpecEntry.languages === 'undefined') {
            let msg = 'To use keyboard \'' + apiSpecEntry.id + '\', you must specify languages.';
            errorStubs.push(convertToErrorStub(apiSpecEntry, msg));
          } else if(Array.isArray(apiSpecEntry.languages)) {
            let splitStubs = KeyboardStub.toStubs(apiSpecEntry, this.pathConfig.keyboards, this.pathConfig.fonts);
            for(let stub of splitStubs) {
              if(stub instanceof KeyboardStub) {
                stubs.push(stub);
              } else {
                errorStubs.push(stub);
              }
            }
          } else { // is a single-language entry.
            const singleLangEntry = apiSpecEntry as KeyboardAPIPropertySpec & { filename: string };
            stubs.push(new KeyboardStub(singleLangEntry, this.pathConfig.keyboards, this.pathConfig.fonts));
          }
        }
      }
    }

    // Next pass: determine which stubs are fully-formed and which are not.
    for(let stub of stubs) {
      if(stub.KF) {
        let err = stub.validateForCustomKeyboard();
        if(err) {
          errorStubs.push(convertToErrorStub(stub, err));
        } else {
          completeStubs.push(stub); // completes are directly added (if possible without error)
        }
      } else {
        incompleteStubs.push(stub);  // incompletes are only used to build a query & are not merged back later.
      }
    }

    // #endregion

    // After that, request stubs that aren't fully-formed / are just requested by string.
    // Verify that we have at least a keyboard ID or a language code.
    let cloudList: CloudRequestEntry[] = [];
    for(let incomplete of incompleteStubs) {
      if(!incomplete.KI && !incomplete.KLC) {
        errorStubs.push(convertToErrorStub(incomplete, "Cannot fetch keyboard information without a keyboard ID or language code."));
        continue;
      }

      // Requests not of string form never specify a specific version.
      // If an 'incomplete stub', we may have prefixed the keyboard ID - undo that!
      const querySpec = toQuerySpecs(unprefixed(incomplete.id), incomplete.langId);
      if(isUniqueRequest(this.cache, cloudList, querySpec)) {
        cloudList.push(querySpec);
      }
    }

    // Double-check the incoming string-based queries, too!
    for(let identifier of identifiers) {
      const pList=identifier.split('@');
      let lList=[''];
      if(pList[0].toLowerCase() == 'english') {
        pList[0] = 'us';
      }

      if(pList.length > 1) {
        lList=pList[1].split(',');
      }

      for(let j=0; j<lList.length; j++) {
        if(j > 0 && lList[j] == '') { // for j==0 => '', no language was specified.
          continue;
        }

        const querySpec = toQuerySpecs(pList[0], lList[j], pList[2]);
        if(isUniqueRequest(this.cache, cloudList, querySpec)) {
          cloudList.push(querySpec);
        }
      }
    }

    // Go ahead and register 'complete' stubs.
    completeStubs.forEach((stub) => this.cache.addStub(stub));

    // After that, note that we do merge non-fully-formed stubs with returned stubs that match?
    let resultPromise = this.cloudQueryEngine.fetchCloudStubs(cloudList.map((spec) => spec.toString()));
    return resultPromise.then((queryResults) => {
      for(let result of queryResults) {
        if(result instanceof KeyboardStub) {
          // Register the newly-complete stub.
          this.cache.addStub(result);
          completeStubs.push(result);
        } else {
          errorStubs.push(result);
        }
      }

      return [].concat(errorStubs).concat(completeStubs);
    });
  }

  async addLanguageKeyboards(languages: string[]): Promise<(ErrorStub | KeyboardStub)[]> {
    // Covers the 'add a keyboard by language name' angle.
    let errorStubs: ErrorStub[] = [];
    let fetchedLanguageList: LanguageAPIPropertySpec[] = [];

    try {
      fetchedLanguageList = await this.cloudQueryEngine.languageListPromise;
    } catch (error) {
      console.error(error);
      errorStubs.push({error: error});
      return errorStubs;
    }

    // Defer registering keyboards by language until the language list has been loaded
    const languageList = fetchedLanguageList;

    // Identify and register each keyboard by language name
    let cmd = '';
    for(let i=0; i<languages.length; i++) {
      let lgName = languages[i].toLowerCase();
      let addAll = (lgName.substr(-1,1) == '$');
      if(addAll) {
        lgName = lgName.substr(0,lgName.length-1);
      }

      let languageFound: boolean = false;
      for(let j=0; j < languageList.length; j++) {
        if(lgName == languageList[j]['name'].toLowerCase()) {
          if(cmd != '') {
            cmd = cmd + ',';
          }

          cmd = cmd + '@' + languageList[j]['id'];
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
      return rejectErrorStubs(errorStubs);
    }

    return this.cloudQueryEngine.keymanCloudRequest('&keyboardid='+cmd, false).then(async (result) => {
      const results = await mergeAndResolveStubPromises(result, errorStubs);

      for(let result of results) {
        // If not an error stub...
        if(typeof result['error'] == 'undefined') {
          this.cache.addStub(result as KeyboardStub);
        }
      }

      return results;
    }, (err) => {
      console.error(err);
      let stub: ErrorStub = {error: err};
      errorStubs.push(stub);
      return rejectErrorStubs(errorStubs);
    });
  }

  async fetchCloudCatalog() {
    try {
      const stubs = await this.cloudQueryEngine.keymanCloudRequest('', false);
      stubs.forEach((stub) => this.cache.addStub(stub));
      return stubs;
    } catch(error) {
      return Promise.reject([{error: error}]);
    }
  }

  /**
   * Display warning if language name unavailable to add keyboard
   * @param {string} languageName
   * @returns string of Error message
   */
  private alertLanguageUnavailable(languageName: string): string {
    let msg = 'No keyboards are available for '+ languageName + '. '
      +'Does it have another language name?';

    // TODO:  hooks for internal alerts!
    // this.keymanweb.util.internalAlert(msg);
    return msg;
  }
}