import {
  type InternalKeyboardFont,
  type KeyboardAPIPropertySpec as APISimpleKeyboard,
  type KeyboardAPIPropertyMultilangSpec as APICompoundKeyboard,
  KeyboardProperties,
  type LanguageAPIPropertySpec
} from '@keymanapp/keyboard-processor';

import { toPrefixedKeyboardId as prefixed } from './stubAndKeyboardCache.js';


// Language regions as defined by cloud server
export const REGIONS = ['World','Africa','Asia','Europe','South America','North America','Oceania','Central America','Middle East'];
export const REGION_CODES = ['un','af','as','eu','sa','na','oc','ca','me'];

export type KeyboardAPISpec = (APISimpleKeyboard | APICompoundKeyboard) & {
  displayName?: string;
  filename: string
};

export default class KeyboardStub extends KeyboardProperties {
  KR: string;
  KRC: string;
  KF: string;

  public constructor(apiSpec: APISimpleKeyboard & { filename: string }, fontBaseUri?: string);
  public constructor(kbdId: string, lngId: string);
  constructor(arg0: string | (APISimpleKeyboard & { filename: string }), arg1?: string) {
    if(typeof arg0 !== 'string') {
      super(arg0, arg1);
    } else {
      super(prefixed(arg0), arg1);
    }
  }

  get region(): string {
    return this.KR;
  }

  get regionCode(): string {
    return this.KRC;
  }

  get filename(): string {
    return this.KF;
  }

  /**
   * Utility to convert API 'stubs' to internal KeyboardStub[]
   * @param arg
   * @returns (KeyboardStub|ErrorStub)[]
   */
  public static toStubs(arg: KeyboardAPISpec, fontBaseUri: string): (KeyboardStub|ErrorStub)[] {
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

    // We have a valid API object to convert over.

    // Extract all the languages
    let languages: LanguageAPIPropertySpec[] = [];
    if (!(arg.languages instanceof Array)) {
      languages.push(arg.languages);
    } else {
      languages.concat(arg.languages);
    }

    let stubs: KeyboardStub[] = [];
    languages.forEach(language => {
      const intermediate = {...arg, languages: language};
      const stub: KeyboardStub = new KeyboardStub(intermediate, fontBaseUri);

      // Accept region as number (from Cloud server), code, or name
      const region=language.region;
      let rIndex=0;
      if(typeof(region) == 'number') {
        if(region < 1 || region > 9) {
          rIndex = 0;
        } else {
          rIndex = region-1;
        }
      } else if(typeof(region) == 'string') {
        let list = (region.length == 2 ? REGION_CODES : REGIONS);
        for(let i=0; i<list.length; i++) {
          if(region.toLowerCase() == list[i].toLowerCase()) {
            rIndex=i;
            break;
          }
        }
      }

      stub.KR = REGIONS[rIndex];
      stub.KRC = REGION_CODES[rIndex];
      stub.KF = arg.filename;

      stubs.push(stub);
    })

    return stubs;
  }

  public merge(stub: KeyboardStub) {
    this.KL ||= stub.KL;
    this.KR ||= stub.KR;
    this.KRC ||= stub.KRC;
    this.KN ||= stub.KN;
    this.displayName ||= stub.displayName;
    this.KF ||= stub.KF;
    this.KFont ||= stub.KFont;
    this.KOskFont ||= stub.KOskFont;
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

export function mergeAndResolveStubPromises(keyboardStubs: (KeyboardStub|ErrorStub)[], errorStubs: ErrorStub[]) :
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