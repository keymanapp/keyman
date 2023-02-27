//
// Version 1.0 of Keyman Developer Project .kpj file
//

export class KeymanDeveloperProject {
  options: KeymanDeveloperProjectOptions;
  files: KeymanDeveloperProjectFile[];
  constructor() {
    this.options = new KeymanDeveloperProjectOptions();
    this.files = [];
  }
};

export enum KeymanDeveloperProjectType {
  Keyboard,
  LexicalModel
};

export type KeymanDeveloperProjectVersion =
  "1.0" |   // Keyman Developer <17.0: All files referenced in .kpj
  "2.0";    // Keyman Developer 17.0+: Files in sub-folders implicitly included

export class KeymanDeveloperProjectOptions {
  buildPath: string;
  compilerWarningsAsErrors: boolean = false;
  warnDeprecatedCode: boolean = true;
  checkFilenameConventions: boolean = true;
  projectType: KeymanDeveloperProjectType;
  version: KeymanDeveloperProjectVersion;
};

export class KeymanDeveloperProjectFile {
  id: string;
  filename: string;
  filePath: string;
  fileVersion: string;
  fileType: string; // always file extension of filename
  details: KeymanDeveloperProjectFileDetail_Kmn & KeymanDeveloperProjectFileDetail_Kps;
  childFiles: KeymanDeveloperProjectFile[];
  constructor() {
    this.details = {};
    this.childFiles = [];
  }
};

export class KeymanDeveloperProjectFileDetail_Kps {
  name?: string;
  copyright?: string;
  version?: string;
};

export class KeymanDeveloperProjectFileDetail_Kmn {
  name?: string;
  copyright?: string;
  message?: string;
};
