//
// The interfaces in this file are designed with reference to the
// mapped structures produced by xml2js when passed a .kps file.
//
// A few notes:
//
// * Casing is updated to camelCase during load (leaving `iD` as a
//   mixed up beastie).
// * Arrays are buried a layer too deep (e.g. <Files><File/><File/></Files>
//   leads to KpsFiles.KpsFile[]
// * Properties such as used in Info Items use `_` and `$` and must be
//   extracted.
// * Strings element is not yet checked to be correct
//

export interface KpsPackage {
  /**
   * <Package> -- the root element.
   */
  package: KpsFile;
}

export interface KpsFile {
  system: KpsFileSystem;
  options: KpsFileOptions;
  info?: KpsFileInfo;
  files?: KpsFileContentFiles;
  keyboards?: KpsFileKeyboards;
  lexicalModels?: KpsFileLexicalModels;
  startMenu?: KpsFileStartMenu;
  strings?: KpsFileStrings;
}

export interface KpsFileSystem {
  keymanDeveloperVersion: string;
  fileVersion: string;
}

export interface KpsFileOptions {
  followKeyboardVersion?: string;
  readMeFile?: string;
  graphicFile?: string;
  executeProgram?: string;
  msiFileName?: string;
  msiOptions?: string;
}

export interface KpsFileInfo {
  name?: KpsFileInfoItem;
  copyright?: KpsFileInfoItem;
  author?: KpsFileInfoItem;
  webSite?: KpsFileInfoItem;
  version?: KpsFileInfoItem;
}

export interface KpsFileInfoItem {
  _: string;
  $: { URL: string };
}

export interface KpsFileContentFiles {
  file: KpsFileContentFile[] | KpsFileContentFile;
}

export interface KpsFileContentFile {
  name: string;
  description: string;
  copyLocation: string;
  fileType: string;
}

export interface KpsFileLexicalModel {
  name: string;
  iD: string;
  languages: KpsFileLanguages;
}

export interface KpsFileLexicalModels {
  lexicalModel: KpsFileLexicalModel[] | KpsFileLexicalModel;
}

export interface KpsFileLanguages {
  language: KpsFileLanguage[] | KpsFileLanguage;
}

export interface KpsFileLanguage {
  _: string;
  $: { ID: string }
}

export interface KpsFileKeyboard {
  name: string;     /// the descriptive name of the keyboard
  iD: string;       /// the keyboard identifier, equal to the basename of the keyboard file sans extension
  version: string;
  oSKFont?: string;
  displayFont?: string;
  rTL?: string;
  languages?: KpsFileLanguages;
}

export interface KpsFileKeyboards {
  keyboard: KpsFileKeyboard[] | KpsFileKeyboard;
}

export interface KpsFileStartMenu {
  folder?: string;
  addUninstallEntry?: string;
  items?: KpsFileStartMenuItems;
}

export interface KpsFileStartMenuItem {
  name: string;
  fileName: string;
  arguments?: string;
  icon?: string;
  location?: string;
}

export interface KpsFileStartMenuItems {
  item: KpsFileStartMenuItem[] | KpsFileStartMenuItem;
}

export interface KpsFileStrings {
  //TODO: validate this structure
  string: string[] | string;
}
