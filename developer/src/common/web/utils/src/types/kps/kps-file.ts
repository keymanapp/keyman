/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */

//
// The interfaces in this file are designed with reference to the mapped
// structures produced by fast-xml-parser when passed a .kps file.
//
// A few notes:
//
// * Arrays are buried a layer too deep (e.g. <Files><File/><File/></Files>
//   leads to KpsFiles.KpsFile[]. These are boxed automatically by
//   KpsFileReader.
// * Properties such as used in Info Items use `_` and `$` and must be
//   extracted.
// * Strings element is not yet checked to be correct
//

/** minimum supported file version, for keyboard packages */
export const KPS_FILE_MINIMUM_VERSION_KEYBOARD_SUPPORT = '7.0';

/** minimum version for lexical model packages */
export const KPS_FILE_MINIMUM_VERSION_LEXICAL_MODEL_SUPPORT = '12.0';

/** minimum version that supports remote references */
export const KPS_FILE_MINIMUM_VERSION_REMOTE_SUPPORT = '18.0';

export const KPS_FILE_VERSIONS = [
  KPS_FILE_MINIMUM_VERSION_KEYBOARD_SUPPORT,
  KPS_FILE_MINIMUM_VERSION_LEXICAL_MODEL_SUPPORT,
  KPS_FILE_MINIMUM_VERSION_REMOTE_SUPPORT,
];

export interface KpsPackage {
  /**
   * <Package> -- the root element.
   */
  Package: KpsFile;
}

export interface KpsFile {
  System: KpsFileSystem;
  Options: KpsFileOptions;
  Info?: KpsFileInfo;
  Files?: KpsFileContentFiles;
  Keyboards?: KpsFileKeyboards;
  LexicalModels?: KpsFileLexicalModels;
  StartMenu?: KpsFileStartMenu;
  Strings?: KpsFileStrings;
  RelatedPackages?: KpsFileRelatedPackages;
}

export interface KpsFileSystem {
  KeymanDeveloperVersion: string;
  FileVersion: string;
}

export interface KpsFileOptions {
  FollowKeyboardVersion?: string;
  ReadMeFile?: string;
  GraphicFile?: string;
  LicenseFile?: string;
  WelcomeFile?: string;
  ExecuteProgram?: string;
  MSIFileName?: string;
  MSIOptions?: string;
}

export interface KpsFileInfo {
  Name?: KpsFileInfoItem;
  Copyright?: KpsFileInfoItem;
  Author?: KpsFileInfoItem;
  WebSite?: KpsFileInfoItem;
  Version?: KpsFileInfoItem;
  Description?: KpsFileInfoItem;
}

export interface KpsFileInfoItem {
  _: string;
  $: { URL: string };
}

export interface KpsFileContentFiles {
  File: KpsFileContentFile[];
}

export interface KpsFileContentFile {
  Name: string;
  /** @deprecated */
  Description?: string;
  /** @deprecated */
  CopyLocation?: string;
  /** @deprecated */
  FileType?: string;
  /** Source location, e.g. flo:<id>, URL */
  Source?: string;
}

export interface KpsFileLexicalModel {
  Name: string;
  ID: string;
  Languages: KpsFileLanguages;
}

export interface KpsFileLexicalModels {
  LexicalModel: KpsFileLexicalModel[];
}

export interface KpsFileLanguages {
  Language: KpsFileLanguage[];
}

export interface KpsFileLanguage {
  _: string;
  $: { ID: string }
}

export interface KpsFileRelatedPackages {
  RelatedPackage: KpsFileRelatedPackage[];
}

export interface KpsFileRelatedPackage {
  $: {
    ID: string;
    /**
     * relationship between this package and the related package, "related" is default if not specified
     */
    Relationship?: "deprecates";
  }
}

export interface KpsFileKeyboard {
  Name: string;     /// the descriptive name of the keyboard
  ID: string;       /// the keyboard identifier, equal to the basename of the keyboard file sans extension
  Version: string;
  OSKFont?: string;
  DisplayFont?: string;
  RTL?: string;
  Languages?: KpsFileLanguages;
  Examples?: KpsFileLanguageExamples;
  /**
   * array of web font alternatives for OSK. should be same font data as oskFont
   */
  WebOSKFonts?: KpsFileFonts;
  /**
   * array of web font alternatives for display. should be same font data as displayFont
   */
  WebDisplayFonts?: KpsFileFonts;
}

export interface KpsFileFonts {
  Font: KpsFileFont[];
}

export interface KpsFileFont {
  $: {
    Filename: string;
  }
}

export interface KpsFileKeyboards {
  Keyboard: KpsFileKeyboard[];
}

export interface KpsFileStartMenu {
  Folder?: string;
  AddUninstallEntry?: string;
  Items?: KpsFileStartMenuItems;
}

export interface KpsFileStartMenuItem {
  Name: string;
  FileName: string;
  Arguments?: string;
  Icon?: string;
  Location?: string;
}

export interface KpsFileStartMenuItems {
  Item: KpsFileStartMenuItem[];
}

export interface KpsFileStrings {
  String: KpsFileString[];
}

export interface KpsFileString {
  $: {
    Name: string;
    Value: string;
  }
}

export interface KpsFileLanguageExamples {
  Example: KpsFileLanguageExample[];
}

/**
 * An example key sequence intended to demonstrate how the keyboard works
 */
export interface KpsFileLanguageExample {
  $: {
    /**
     * BCP 47 identifier for the example
     */
    ID: string;
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
    Keys: string;
    /**
     * The text that would be generated by typing those keys
     */
    Text?: string;
    /**
     * A short description of what the text means or represents
     */
    Note?: string;
  }
}
