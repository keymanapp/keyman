export interface KmpJsonFile {
  system: KmpJsonFileSystem;
  options: KmpJsonFileOptions;
  info?: KmpJsonFileInfo;
  files?: KmpJsonFileContentFile[];
  lexicalModels?: KmpJsonFileLexicalModel[];
  startMenu?: KmpJsonFileStartMenu;
  keyboards?: KmpJsonFileKeyboard[];
}

export interface KmpJsonFileSystem {
  keymanDeveloperVersion: string;
  fileVersion: string;
}

export interface KmpJsonFileOptions {
  readmeFile?: string;
  graphicFile?: string;
  executeProgram?: string;
  msiFilename?: string;
  msiOptions?: string;
}

export interface KmpJsonFileInfo {
  website?: KmpJsonFileInfoItem;
  version?: KmpJsonFileInfoItem;
  name?: KmpJsonFileInfoItem;
  copyright?: KmpJsonFileInfoItem;
  author?: KmpJsonFileInfoItem;
}

export interface KmpJsonFileInfoItem {
  description: string;
  url?: string;
}

export interface KmpJsonFileContentFile {
  name: string;
  description: string;
  copyLocation?: number;
}

export interface KmpJsonFileLexicalModel {
  name: string;
  id: string;
  languages: KmpJsonFileLanguage[];
}

export interface KmpJsonFileLanguage {
  name: string;
  id: string;
}

export interface KmpJsonFileKeyboard {
  name: string;
  id: string;
  version: string;
  oskFont?: string;
  displayFont?: string;
  rtl?: boolean;
  languages?: KmpJsonFileLanguage[];
}

export interface KmpJsonFileStartMenu {
  folder?: string;
  addUninstallEntry?: boolean;
  items?: KmpJsonFileStartMenuItem[];
}

export interface KmpJsonFileStartMenuItem {
  name: string;
  filename: string;
  arguments?: string;
  icon?: string;
  location?: string;
}
