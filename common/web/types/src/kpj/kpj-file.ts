/*
 * Keyman Developer Project File
 */


// These interfaces match XML read by xml2js. Use `KeymanDeveloperProject`
// returned by `KPJFileReader.transform()` for all processing.

export interface KPJFile {
  KeymanDeveloperProject: KPJFileProject;
}

export interface KPJFileProject {
  Options?: KPJFileOptions;
  Files?: KPJFileFiles;
};

export interface KPJFileOptions {
  BuildPath?: string;
  CompilerWarningsAsErrors?: string; // default False
  WarnDeprecatedCode?: string; // default True
  CheckFilenameConventions?: string; // default True
  ProjectType?: 'keyboard' | 'lexicalmodel';
  Version?: '1.0' | '2.0'; // default 1.0
};

export interface KPJFileFiles {
  File?: KPJFileFile[];
};

export interface KPJFileFile {
  ID?: string;
  Filename?: string;
  Filepath?: string;
  FileVersion?: string;
  FileType?: string;
  Details?: KPJFileFileDetail_Kmn & KPJFileFileDetail_Kps;
  ParentFileID?: string;
};

export interface KPJFileFileDetail_Kmn {
  Name?: string;
  Copyright?: string;
  Message?: string;
};

export interface KPJFileFileDetail_Kps {
  Name?: string;
  Copyright?: string;
  Version?: string;
};
