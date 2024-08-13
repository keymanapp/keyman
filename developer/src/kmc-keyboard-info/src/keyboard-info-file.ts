export type KeyboardInfoFileEncodings = "ansi" | "unicode";
export type KeyboardInfoFileIncludes = "welcome" | "documentation" | "fonts" | "visualKeyboard";
export type KeyboardInfoFilePlatform = "windows" | "macos" | "desktopWeb" | "ios" | "android" | "mobileWeb" | "linux";
export type KeyboardInfoFilePlatformSupport = "dictionary" | "full" | "basic" | "none";

export interface KeyboardInfoFile {
  id?: string;
  name?: string;
  authorName?: string;
  authorEmail?: string;
  description?: string;
  license?: "freeware" | "shareware" | "commercial" | "mit" | "other";
  languages?: string[] | {[bcp47: string]: KeyboardInfoFileLanguage};
  lastModifiedDate?: string;
  packageFilename?: string;
  packageFileSize?: number;
  jsFilename?: string;
  jsFileSize?: number;
  isRTL?: boolean;
  encodings?: KeyboardInfoFileEncodings[];
  packageIncludes?: KeyboardInfoFileIncludes[];
  version?: string;
  minKeymanVersion?: string;
  helpLink?: string;
  platformSupport?: {[id in KeyboardInfoFilePlatform]?: KeyboardInfoFilePlatformSupport};
  sourcePath?: string;
  related?: {[id: string]: KeyboardInfoFileRelated};
  deprecated?: boolean;
}

export interface KeyboardInfoFileRelated {
  deprecates?: boolean;
  deprecatedBy?: boolean;
}

export interface KeyboardInfoFileLanguage {
  font?: KeyboardInfoFileLanguageFont;
  oskFont?: KeyboardInfoFileLanguageFont;
  examples?: KeyboardInfoFileExample[];
  displayName?: string;
  languageName?: string;
  scriptName?: string;
  regionName?: string;
}

export interface KeyboardInfoFileLanguageFont {
  family?: string;
  source?: string[];
}

export interface KeyboardInfoFileExample {
  /**
   * A space-separated list of keys.
   * - modifiers indicated with "+"
   * - spacebar is "space"
   * - plus key is "shift+=" or "plus" on US English (all other punctuation as per key cap).
   * - Hardware modifiers are: "shift", "ctrl", "alt", "left-ctrl",
   *   "right-ctrl", "left-alt", "right-alt"
   * - Key caps should generally be their character for desktop (Latin script
   *   case insensitive), or the actual key cap for touch
   * - Caps Lock should be indicated with "caps-on", "caps-off"
   *
   * e.g. "shift+a b right-alt+c space plus z z z" represents something like: "Ab{AltGr+C} +zzz"
   */
  keys?: string;
  text?: string;
  note?: string;
}
