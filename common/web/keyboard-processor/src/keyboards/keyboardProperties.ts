import SpacebarText from './spacebarText.js';

export interface KeyboardFont {
  'family': string;
  'files'?: string;
  'filename'?: string[];
  'source'?: string | string[];
  'path': string;
}

// Filename properties are deliberately omitted here; we can add that at higher-levels where it matters
// via 'mix-in'.
//
// For example, the OSK module doesn't care about the filename of a loaded keyboard.  It doesn't do
// keyboard loading on its own whatsoever.

// Corresponds to Keyman Engine for Web's internal "keyboard stub" format.
export interface KeyboardInternalPropertySpec {
  KI: string,
  'KFont': KeyboardFont,
  'KOskFont': KeyboardFont,
  displayName?: string,
  'KN'?: string,
  'KL'?: string,
  'KLC'?: string
};

export type LanguageAPIPropertySpec = {
  id: string,
  name: string,
  font: KeyboardFont,
  oskFont: KeyboardFont,
  region?: number|string
}

/**
 * Corresponds to the documented API for the Web engine's `addKeyboards` function
 * when a single language object is specified - not an array.
 *
 * See https://help.keyman.com/DEVELOPER/ENGINE/WEB/15.0/reference/core/addKeyboards,
 * "Using an `object`".
 */
export type KeyboardAPIPropertySpec = {
  id: string,
  name: string,
  languages: LanguageAPIPropertySpec;
}

/**
 * Corresponds to the documented API for the Web engine's `addKeyboards` function
 * when a language array is specified for the object.
 *
 * See https://help.keyman.com/DEVELOPER/ENGINE/WEB/15.0/reference/core/addKeyboards,
 * "Using an `object`".
 */
export type KeyboardAPIPropertyMultilangSpec = {
  id: string,
  name: string,
  languages: LanguageAPIPropertySpec[];
}

type MetadataObj = KeyboardInternalPropertySpec | KeyboardAPIPropertySpec;

export default class KeyboardProperties {
  /**
   * Designed to match, hold, and correspond to an instance of KeyboardStub, a class external to this module
   * - it's defined within the main Keyman Engine for Web top-level project.
   */
  private readonly wrappedStub: KeyboardInternalPropertySpec;

  public spacebarTextMode: SpacebarText;

  public constructor(metadataObj: MetadataObj, spacebarTextMode?: SpacebarText);
  public constructor(displayName: string, languageCode: string, textFont: KeyboardFont, oskFont?: KeyboardFont);
  public constructor(arg1: (MetadataObj) | string, arg2?: string | SpacebarText, arg3?: KeyboardFont, arg4?: KeyboardFont) {
    if(!(typeof arg1 == 'string')) {
      if(arg1['KLC'] || arg1['KFont'] || arg1['KOskFont']) {
        this.wrappedStub = arg1 as KeyboardInternalPropertySpec;
        this.spacebarTextMode = arg2 as SpacebarText;
      } else {
        let apiStub = arg1 as KeyboardAPIPropertySpec;
        this.wrappedStub = {
          KI: apiStub.id,
          KN: apiStub.name,
          KL: apiStub.languages.name,
          KLC: apiStub.languages.id,
          KFont: apiStub.languages.font,
          KOskFont: apiStub.languages.oskFont
        };
        this.spacebarTextMode = arg2 as SpacebarText;
      }
    } else {
      this.wrappedStub = {
        KI: null,
        displayName: arg1,
        'KLC': arg2,
        'KFont': arg3,
        'KOskFont': arg4 || arg3
      }
    }
  }

  public static fromMultilanguageAPIStub(apiStub: KeyboardAPIPropertyMultilangSpec, spacebarTextMode?: SpacebarText): KeyboardProperties[] {
    let stubs: KeyboardProperties[] = [];

    for(let langSpec of apiStub.languages) {
      let stub: KeyboardAPIPropertySpec = {
        id: apiStub.id,
        name: apiStub.name,
        languages: langSpec
      };

      stubs.push(new KeyboardProperties(stub, spacebarTextMode));
    }

    return stubs;
  }

  public get id(): string {
    return this.wrappedStub.KI;
  }

  public get name(): string {
    return this.wrappedStub.KN;
  }

  public get langId(): string {
    return this.wrappedStub.KLC;
  }

  public get langName(): string {
    return this.wrappedStub.KL;
  }

  public get displayName(): string {
    if(this.wrappedStub.displayName) {
      return this.wrappedStub.displayName;
    }

    // else, construct it.
    const kbdName = this.wrappedStub.KN;
    const lgName = this.wrappedStub.KL;

    switch (this.spacebarTextMode) {
      case SpacebarText.KEYBOARD:
        return kbdName;
      case SpacebarText.LANGUAGE:
        return lgName;
      case SpacebarText.LANGUAGE_KEYBOARD:
        return (kbdName == lgName) ? lgName : lgName + ' - ' + kbdName;
      case SpacebarText.BLANK:
        return '';
      default:
        return kbdName;
    }
  }

  public set displayName(name: string) {
    this.wrappedStub.displayName = name;
  }

  public get textFont() {
    return this.wrappedStub.KFont;
  }

  public get oskFont() {
    return this.wrappedStub.KOskFont;
  }
}