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
  encodings: KeyboardInfoFileEncodings[];
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
  example?: KeyboardInfoFileExample;
  displayName?: string;
  languageName?: string;
  scriptName?: string;
  regionName?: string;
}

export interface KeyboardInfoFileLanguageFont {
  family?: string;
  source?: string | string[];
  size?: string;
}

export interface KeyboardInfoFileExample {
  keys?: string | KeyboardInfoFileExampleKey[];
  text?: string;
  note?: string;
}

export type KeyboardInfoFileExampleKeyId =
  "K_SPACE" |
  "K_A" | "K_B" | "K_C" | "K_D" | "K_E" | "K_F" | "K_G" | "K_H" | "K_I" | "K_J" | "K_K" | "K_L" | "K_M" |
  "K_N" | "K_O" | "K_P" | "K_Q" | "K_R" | "K_S" | "K_T" | "K_U" | "K_V" | "K_W" | "K_X" | "K_Y" | "K_Z" |
  "K_1" | "K_2" | "K_3" | "K_4" | "K_5" | "K_6" | "K_7" | "K_8" | "K_9" | "K_0" |
  "K_BKQUOTE" | "K_HYPHEN" | "K_EQUAL" | "K_LBRKT" | "K_RBRKT" | "K_BKSLASH" | "K_COLON" |
  "K_QUOTE" | "K_COMMA" | "K_PERIOD" | "K_SLASH" |
  "K_oE2" | "K_BKSP" | "K_TAB" | "K_ENTER" | "K_ESC" |
  "K_LEFT" | "K_UP" | "K_RIGHT" | "K_DOWN" | "K_PGUP" | "K_PGDN" | "K_HOME" | "K_END" | "K_INS" | "K_DEL" |
  "K_F1" | "K_F2" | "K_F3" | "K_F4" | "K_F5" | "K_F6" | "K_F7" | "K_F8" | "K_F9" | "K_F10" | "K_F11" | "K_F12" |
  "K_KP5" | "K_NP0" | "K_NP1" | "K_NP2" | "K_NP3" | "K_NP4" | "K_NP5" | "K_NP6" | "K_NP7" | "K_NP8" | "K_NP9" |
  "K_NPSTAR" | "K_NPPLUS" | "K_NPMINUS" | "K_NPDOT" | "K_NPSLASH" |
  "K_SEL" | "K_PRINT" | "K_EXEC" | "K_HELP" | "K_SEPARATOR" |
  "K_F13" | "K_F14" | "K_F15" | "K_F16" | "K_F17" | "K_F18" | "K_F19" | "K_F20" | "K_F21" | "K_F22" | "K_F23" | "K_F24" |
  "K_KANJI?15" | "K_KANJI?16" | "K_KANJI?17" | "K_KANJI?18" | "K_KANJI?19" | "K_KANJI?1C" | "K_KANJI?1D" | "K_KANJI?1E" | "K_KANJI?1F" |
  "K_oE0" | "K_oE1" | "K_oE3" | "K_oE4" | "K_oE6" | "K_oE9" | "K_oEA" | "K_oEB" | "K_oEC" | "K_oED" | "K_oEE" | "K_oEF" |
  "K_oF0" | "K_oF1" | "K_oF2" | "K_oF3" | "K_oF4" | "K_oF5" | "K_?00" | "K_?05" | "K_NPENTER" |
  "K_?06" | "K_?07" | "K_?0A" | "K_?0B" | "K_?0E" | "K_?0F" | "K_?1A" | "K_?3A" | "K_?3B" | "K_?3C" | "K_?3D" | "K_?3E" |
  "K_?3F" | "K_?40" | "K_?5B" | "K_?5C" | "K_?5D" | "K_?5E" | "K_?5F" | "K_?88" | "K_?89" | "K_?8A" | "K_?8B" | "K_?8C" |
  "K_?8D" | "K_?8E" | "K_?8F" | "K_?92" | "K_?94" | "K_?95" | "K_?96" | "K_?97" | "K_?98" | "K_?99" | "K_?9A" | "K_?9B" |
  "K_?9C" | "K_?9D" | "K_?9E" | "K_?9F" | "K_?A0" | "K_?A1" | "K_?A2" | "K_?A3" | "K_?A4" | "K_?A5" | "K_?A6" | "K_?A7" |
  "K_?A8" | "K_?A9" | "K_?AA" | "K_?AB" | "K_?AC" | "K_?AD" | "K_?AE" | "K_?AF" | "K_?B0" | "K_?B1" | "K_?B2" | "K_?B3" |
  "K_?B4" | "K_?B5" | "K_?B6" | "K_?B7" | "K_?B8" | "K_?B9" | "K_?C1" | "K_?C2" | "K_?C3" | "K_?C4" | "K_?C5" | "K_?C6" |
  "K_?C7" | "K_?C8" | "K_?C9" | "K_?CA" | "K_?CB" | "K_?CC" | "K_?CD" | "K_?CE" | "K_?CF" | "K_?D0" | "K_?D1" | "K_?D2" |
  "K_?D3" | "K_?D4" | "K_?D5" | "K_?D6" | "K_?D7" | "K_?D8" | "K_?D9" | "K_?DA" | "K_oDF" | "K_?E5" | "K_?E7" | "K_?E8" |
  "K_?F6" | "K_?F7" | "K_?F8" | "K_?F9" | "K_?FA" | "K_?FB" | "K_?FC" | "K_?FD" | "K_?FE" | "K_?FF";

export type KeyboardInfoFileExampleKeyModifier =
  "shift" | "s" | "ctrl" | "c" | "alt" | "a" | "left-ctrl" | "lc" |
  "right-ctrl" | "rc" | "left-alt" | "la" | "right-alt" | "ra";

export interface KeyboardInfoFileExampleKey {
  key?: KeyboardInfoFileExampleKeyId;
  modifiers?: KeyboardInfoFileExampleKeyModifier[];
}

