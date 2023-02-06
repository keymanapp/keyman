import {
  type KeyboardInternalPropertySpec,
  type KeyboardFont,
  type KeyboardAPIPropertySpec,
  type KeyboardAPIPropertyMultilangSpec
} from '@keymanapp/keyboard-processor';

export type KeyboardAPISpec = (KeyboardAPIPropertySpec | KeyboardAPIPropertyMultilangSpec) & {
  filename: string,
  displayName?: string
};

export default class KeyboardStub implements KeyboardInternalPropertySpec {
  KI: string;
  KN: string;
  KL: string;
  KLC: string;
  KR: string;
  KRC: string;
  KF: string;
  KFont: KeyboardFont;
  KOskFont: KeyboardFont;
  displayName: string;

  constructor(kbdId: string, lngId: string) {
    this.KI = kbdId;
    this.KLC = lngId;
  }

  /**
   * Utility to convert stubs to KeyboardStub[]
   * @param arg
   * @returns (KeyboardStub|ErrorStub)[]
   */
  public static toStubs(arg: KeyboardAPISpec): (KeyboardStub|ErrorStub)[] {
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
    if (!(arg.languages instanceof Array)) {
      languages.push(arg.languages);
    } else {
      arg.languages.forEach(language => {
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