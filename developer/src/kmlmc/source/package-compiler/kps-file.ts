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

interface KpsPackage {
  /**
   * <Package> -- the root element.
   */
  package: KpsFile;
}

interface KpsFile {
  system: KpsFileSystem;
  options: KpsFileOptions;
  info?: KpsFileInfo;
  files?: KpsFileContentFiles;
  keyboards?: KpsFileKeyboards;
  lexicalModels?: KpsFileLexicalModels;
  startMenu?: KpsFileStartMenu;
  strings?: KpsFileStrings;
}

interface KpsFileSystem {
  keymanDeveloperVersion: string;
  fileVersion: string;
}

interface KpsFileOptions {
  followKeyboardVersion?: string;
  readmeFile?: string;
  graphicFile?: string;
  executeProgram?: string;
  msiFilename?: string;
  msiOptions?: string;
}

interface KpsFileInfo {
  name?: KpsFileInfoItem;
  copyright?: KpsFileInfoItem;
  author?: KpsFileInfoItem;
  website?: KpsFileInfoItem;
  version?: KpsFileInfoItem;
}

interface KpsFileInfoItem {
  _: string;
  $: { URL: string };
}

interface KpsFileContentFiles {
  file: KpsFileContentFile[] | KpsFileContentFile;
}

interface KpsFileContentFile {
  name: string;
  description: string;
  copyLocation: string;
  fileType: string;
}

interface KpsFileLexicalModel {
  name: string;
  iD: string;
  languages: KpsFileLanguages;
}

interface KpsFileLexicalModels {
  lexicalModel: KpsFileLexicalModel[] | KpsFileLexicalModel;
}

interface KpsFileLanguages {
  language: KpsFileLanguage[] | KpsFileLanguage;
}

interface KpsFileLanguage {
  _: string;
  $: { ID: string }
}

interface KpsFileKeyboard {
  name: string;
  iD: string;
  version: string;
  oskFont?: string;
  displayFont?: string;
  rtl?: boolean;
  languages?: KpsFileLanguages;
}

interface KpsFileKeyboards {
  keyboard: KpsFileKeyboard[] | KpsFileKeyboard;
}

interface KpsFileStartMenu {
  folder?: string;
  addUninstallEntry?: string;
  items?: KpsFileStartMenuItems;
}

interface KpsFileStartMenuItem {
  name: string;
  filename: string;
  arguments?: string;
  icon?: string;
  location?: string;
}

interface KpsFileStartMenuItems {
  item: KpsFileStartMenuItem[] | KpsFileStartMenuItem;
}

interface KpsFileStrings {
  //TODO: validate this structure
  string: string[] | string;
}
