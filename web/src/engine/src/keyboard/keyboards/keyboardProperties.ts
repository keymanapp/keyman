import SpacebarText from './spacebarText.js';

export interface InternalKeyboardFont {
  family: string;
  filename?: never;
  files: string | string[];  // internal
  source?: never;
  path: string;
}

interface CloudKeyboardFont1 {
  family: string;
  filename: string | string[];
  files?: never;
  source?: never;
}

interface CloudKeyboardFont2 {
  family: string;
  filename?: never;
  files?: never;
  source: string | string[];
}

export type CloudKeyboardFont = CloudKeyboardFont1 | CloudKeyboardFont2;

/**
 * Converts one of three public-facing font-specification formats into a consistent structure
 * used generally among the Keyman JS/TS modules.
 * @param fontObj
 * @param fontPath
 * @returns
 */
export function internalizeFont(fontObj: CloudKeyboardFont, fontPath: string): InternalKeyboardFont {
  if(!fontObj) {
    return undefined;
  } else {
    return {
      family: fontObj.family,
      path: fontPath,
      files: fontObj.filename || fontObj.source
    }
  }
}

export type KeyboardFont = CloudKeyboardFont | InternalKeyboardFont;

// Filename properties are deliberately omitted here; we can add that at higher-levels where it matters
// via 'mix-in'.
//
// For example, the OSK module doesn't care about the filename of a loaded keyboard.  It doesn't do
// keyboard loading on its own whatsoever.

// Corresponds to Keyman Engine for Web's internal "keyboard stub" format.
// Also referred to by KMW 2.0-era loaders:  https://help.keyman.com/developer/8.0/docs/reference_kmw20_example
export interface KeyboardInternalPropertySpec {
  KI: string,
  KFont: InternalKeyboardFont,
  KOskFont: InternalKeyboardFont,
  displayName?: string,
  KN?: string,
  KL?: string,
  KLC?: string
};

export type LanguageAPIPropertySpec = {
  id: string,
  name: string,
  font: CloudKeyboardFont,
  oskFont: CloudKeyboardFont,
  region?: number|string
}

/**
 * Corresponds to the documented API for the Web engine's `addKeyboards` function
 * when a single language object is specified - not an array.
 *
 * See https://help.keyman.com/developer/engine/web/15.0/reference/core/addKeyboards,
 * "Using an `object`".
 */
export type KeyboardAPIPropertySpec = {
  id: string,
  name: string,

  /**
   * @deprecated Replaced with `languages`.
   */
  language?: LanguageAPIPropertySpec;
  languages: LanguageAPIPropertySpec;
}

/**
 * Corresponds to the documented API for the Web engine's `addKeyboards` function
 * when a language array is specified for the object.
 *
 * See https://help.keyman.com/developer/engine/web/15.0/reference/core/addKeyboards,
 * "Using an `object`".
 */
export type KeyboardAPIPropertyMultilangSpec = {
  id: string,
  name: string,

  /**
   * @deprecated Replaced with `languages`.
   */
  language?: LanguageAPIPropertySpec[];
  languages: LanguageAPIPropertySpec[];
}

export type MetadataObj = KeyboardInternalPropertySpec | KeyboardAPIPropertySpec | KeyboardAPIPropertyMultilangSpec;

export default class KeyboardProperties implements KeyboardInternalPropertySpec {
  KI: string;
  KN: string;
  KL: string;
  KLC: string;
  KFont: InternalKeyboardFont;
  KOskFont: InternalKeyboardFont;
  _displayName?: string;

  private static spacebarTextModeSrc: SpacebarText | (() => SpacebarText) = SpacebarText.KEYBOARD;

  public static get spacebarTextMode(): SpacebarText {
    if(typeof this.spacebarTextModeSrc == 'string') {
      return this.spacebarTextModeSrc;
    } else {
      return this.spacebarTextModeSrc();
    }
  }

  public static set spacebarTextMode(source: typeof KeyboardProperties.spacebarTextModeSrc) {
    this.spacebarTextModeSrc = source;
  }

  public constructor(metadataObj: MetadataObj, fontPath?: string);
  public constructor(keyboardId: string, languageCode: string);
  public constructor(arg1: MetadataObj | string, arg2?: string) {
    if(!(typeof arg1 == 'string')) {
      // @ts-ignore
      if(arg1['KI'] || arg1['KL'] || arg1['KLC'] || arg1['KFont'] || arg1['KOskFont']) {
        const other = arg1 as KeyboardInternalPropertySpec;
        this.KI = other.KI;
        this.KN = other.KN;
        this.KL = other.KL;
        this.KLC = other.KLC;
        // Do NOT apply fontPath here; the mobile apps will have font issues if you do!
        this.KFont = other.KFont;
        this.KOskFont = other.KOskFont;
        this._displayName = (other instanceof KeyboardProperties) ? other._displayName : other.displayName;
      } else {
        let apiStub = arg1 as KeyboardAPIPropertySpec; // TODO:  could be an array, as currently specified.  :(

        apiStub.languages ||= apiStub.language;

        this.KI = apiStub.id,
        this.KN = apiStub.name,
        this.KL = apiStub.languages.name,
        this.KLC = apiStub.languages.id,
        this.KFont = internalizeFont(apiStub.languages.font, arg2),
        this.KOskFont = internalizeFont(apiStub.languages.oskFont, arg2)
      }
    } else {
      this.KI = arg1;
      this.KLC = arg2;
    }
  }

  public static fromMultilanguageAPIStub(apiStub: KeyboardAPIPropertyMultilangSpec): KeyboardProperties[] {
    let stubs: KeyboardProperties[] = [];

    apiStub.languages ||= apiStub.language;

    for(let langSpec of apiStub.languages) {
      let stub: KeyboardAPIPropertySpec = {
        id: apiStub.id,
        name: apiStub.name,
        languages: langSpec
      };

      stubs.push(new KeyboardProperties(stub));
    }

    return stubs;
  }

  public get id(): string {
    return this.KI;
  }

  public get name(): string {
    return this.KN;
  }

  public get langId(): string {
    return this.KLC;
  }

  public get langName(): string {
    return this.KL;
  }

  public get displayName(): string {
    if(this._displayName) {
      return this._displayName;
    }

    // else, construct it.
    const kbdName = this.KN;
    const lgName = this.KL;

    switch (KeyboardProperties.spacebarTextMode) {
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
    this._displayName = name;
  }

  public get textFont() {
    return this.KFont;
  }

  public get oskFont() {
    return this.KOskFont;
  }

  /**
   * Generates an error for objects with specification levels insufficient for use in the on-screen-keyboard
   * module, complete with a message about one or more details in need of correction.
   * @returns A preconstructed `Error` instance that may be thrown by the caller.
   */
  public validateForOSK(): Error {
    if(!this.KLC) {
      if(this.KI || this.KN) {
        return new Error(`No language code was specified for use with the ${this.KI || this.KN} keyboard`);
      } else {
        return new Error("No language code was specified for use with the corresponding keyboard")
      }
    }

    if(this.displayName === undefined || (KeyboardProperties.spacebarTextMode != SpacebarText.BLANK && !this.displayName)) {
      return new Error("A display name is missing for this keyboard and cannot be generated under current settings.")
    }

    return null;
  }

  public validateForCustomKeyboard(): Error {
    if(!this.KI || !this.KN || !this.KL || !this.KLC) {
      return new Error("To use a custom keyboard, you must specify keyboard id, keyboard name, language and language code.");
    } else {
      return null;
    }
  }
}