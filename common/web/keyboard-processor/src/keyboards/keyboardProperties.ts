import SpacebarText from './spacebarText.js';

export interface KeyboardFont {
  'family': string;
  'files': string;
  'path': string;
}

// Corresponds to Keyman Engine for Web's internal "keyboard stub" format.
export type KeyboardInternalPropertySpec = {
  'KFont': KeyboardFont,
  'KOskFont': KeyboardFont,
  displayName?: string,
  'KN'?: string,
  'KL'?: string,
  'KLC'?: string
};

// Corresponds to the documented API for the Web engine's `addKeyboards` function
// when a single language object is specified - not an array.
export type KeyboardAPIPropertySpec = {
  id: string,
  name: string,
  languages: {
    id: string,
    name: string,
    font: KeyboardFont,
    oskFont: KeyboardFont
  }
}

type MetadataObj = KeyboardInternalPropertySpec | KeyboardAPIPropertySpec;

export default class KeyboardProperties {
  /**
   * Designed to match, hold, and correspond to an instance of KeyboardStub, a class external to this module.
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
        displayName: arg1,
        'KLC': arg2,
        'KFont': arg3,
        'KOskFont': arg4 || arg3
      }
    }
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