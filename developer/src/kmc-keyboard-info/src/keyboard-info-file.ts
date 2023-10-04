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
  keys?: KeyboardInfoFileExampleKey[];
  text?: string;
  note?: string;
}

export interface KeyboardInfoFileExampleKey {
  key: string;
  modifiers?: string[];
}
