/*
 * Keyman Developer Project File
 */


// These interfaces match XML read by xml2js. Use `KeymanDeveloperProject`
// returned by `KPJFileReader.transform()` for all processing.

export interface KPJFile {
  KeymanDeveloperProject: KPJFileProject;
}

export interface KPJFileProject {
  Options?: KPJFileOptions;   // Required
  Files?: KPJFileFiles;       // Required in 1.0, optional and always ignored in 2.0
};

export interface KPJFileOptions {
  BuildPath?: string;                         // default '' in 1.0, '$PROJECTPATH/build' in 2.0
  SourcePath?: string;                        // default '' in 1.0, '$PROJECTPATH/source' in 2.0
  CompilerWarningsAsErrors?: string;          // default False
  WarnDeprecatedCode?: string;                // default True
  SkipMetadataFiles?: string;                 // default True for 1.0, False for 2.0
  CheckFilenameConventions?: string;          // default False
  ProjectType?: 'keyboard' | 'lexicalmodel';  // default 'keyboard'
  Version?: '1.0' | '2.0';                    // default 1.0
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
