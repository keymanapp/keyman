//
// Version 1.0 of Keyman Developer Project .kpj file
//

import { KeymanFileTypes } from '../main.js';
import { CompilerCallbacks } from '../util/compiler-interfaces.js';

export class KeymanDeveloperProject {
  options: KeymanDeveloperProjectOptions;
  files: KeymanDeveloperProjectFile[];
  projectPath: string = '';

  constructor(private projectFilename: string, version: KeymanDeveloperProjectVersion, private callbacks: CompilerCallbacks) {
    this.projectPath = this.callbacks.path.dirname(this.projectFilename);
    this.options = new KeymanDeveloperProjectOptions(version);
    this.files = [];
  }
  /**
   * Adds .kmn, .xml, .kps to project based on options.sourcePath
   * @param projectFilename Full path to project.kpj (even if the file doesn't exist)
   */
  populateFiles() {
    if(this.options.version != '2.0') {
      throw new Error('populateFiles can only be called on a v2.0 project');
    }
    let sourcePath = this.resolveProjectPath(this.options.sourcePath);
    let files = this.callbacks.fs.readdirSync(sourcePath);
    for(let filename of files) {
      let fullPath = this.callbacks.path.join(sourcePath, filename);
      if(KeymanFileTypes.filenameIs(filename, KeymanFileTypes.Source.LdmlKeyboard)) {
        if(!this.callbacks.fs.readFileSync(fullPath, 'utf-8').match(/ldmlKeyboard\.dtd/)) {
          // Skip this .xml because we assume it isn't really a keyboard .xml
          continue;
        }
      }
      if(KeymanFileTypes.sourceTypeFromFilename(filename) !== null) {
        let file = new KeymanDeveloperProjectFile20(fullPath, this.callbacks);
        this.files.push(file);
      }
    }

    this.addMetadataFile();
  }

  public isKeyboardProject() {
    return !!this.files.find(file => file.fileType == KeymanFileTypes.Source.KeymanKeyboard || file.fileType == KeymanFileTypes.Source.LdmlKeyboard);
  }

  public isLexicalModelProject() {
    return !!this.files.find(file => file.fileType == KeymanFileTypes.Source.Model);
  }

  public addMetadataFile() {
    const infoFile =
      this.callbacks.path.join(this.projectPath,
      this.callbacks.path.basename(this.projectFilename, KeymanFileTypes.Source.Project) +
      (this.isLexicalModelProject() ? KeymanFileTypes.Source.ModelInfo : KeymanFileTypes.Source.KeyboardInfo));
    this.files.push(new KeymanDeveloperProjectFile20(infoFile, this.callbacks));
  }

  private resolveProjectPath(p: string): string {
    // Replace placeholders in the target path
    return p.replace('$PROJECTPATH', this.projectPath);
  }

  resolveInputFilePath(file: KeymanDeveloperProjectFile): string {
    return this.callbacks.resolveFilename(this.projectFilename, file.filePath);
  }

  resolveOutputFilePath(file: KeymanDeveloperProjectFile, sourceExt: string, targetExt: string): string {
    // Roughly corresponds to Delphi TProject.GetTargetFileName
    let p = this.options.version == '1.0' ?
      this.options.buildPath || '$SOURCEPATH' :
      this.options.buildPath;

    // Replace placeholders in the target path
    if(this.options.version == '1.0') {
      // TODO: do we need to support $VERSION?
      // $SOURCEPATH only supported in 1.0 projects
      p = p.replace('$SOURCEPATH', this.callbacks.path.dirname(this.resolveInputFilePath(file)));
    }

    p = this.resolveProjectPath(p);

    let f = file.filename.replace(new RegExp(`\\${sourceExt}$`, 'i'), targetExt);
    return this.callbacks.path.normalize(this.callbacks.path.join(p, f));
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
  sourcePath: string;
  compilerWarningsAsErrors: boolean = false;
  warnDeprecatedCode: boolean = true;
  checkFilenameConventions: boolean = true;
  projectType: KeymanDeveloperProjectType = KeymanDeveloperProjectType.Keyboard;
  readonly version: KeymanDeveloperProjectVersion;
  constructor(version: KeymanDeveloperProjectVersion) {
    this.version = version;
    switch(version) {
      case "1.0":
        this.buildPath = '';
        this.sourcePath = '';
        break;
      case "2.0":
        this.buildPath = '$PROJECTPATH/build';
        this.sourcePath = '$PROJECTPATH/source';
        break;
      default:
        throw new Error('Invalid version');
    }
  }
};

export type KeymanDeveloperProjectFile = KeymanDeveloperProjectFile10 | KeymanDeveloperProjectFile20;

export class KeymanDeveloperProjectFile10 {
  readonly id: string;           // 1.0 only
  readonly filename: string;
  readonly filePath: string;
  readonly fileVersion: string;  // 1.0 only
  /**
   * file extension of filename; warning: .model.ts is technically not the fileType because of 2 periods
   * @deprecated use `getFileType()`
   */
  readonly fileType: KeymanFileTypes.Any; // 1.0 only
  details: KeymanDeveloperProjectFileDetail_Kmn & KeymanDeveloperProjectFileDetail_Kps; // 1.0 only
  childFiles: KeymanDeveloperProjectFile[]; // 1.0 only
  constructor(id: string, filename: string, filePath: string, fileVersion:string, fileType: KeymanFileTypes.Any) {
    this.details = {};
    this.childFiles = [];
    this.id = id;
    this.filename = filename;
    this.filePath = filePath;
    this.fileVersion = fileVersion;
    this.fileType = fileType;
  }
  getFileType() {
    return KeymanFileTypes.sourceTypeFromFilename(this.filename);
  }
};

export type KeymanDeveloperProjectFileType20 = KeymanFileTypes.Source;

export class KeymanDeveloperProjectFile20 {
  readonly filename: string;
  readonly filePath: string;
  /**
   * file extension of filename, but .model.ts is technically not the ext because of 2 periods
   * @deprecated TODO: remove this from 2.0 or make it private
   */
  readonly fileType: KeymanFileTypes.Source;
  constructor(filePath: string, private callbacks: CompilerCallbacks) {
    this.filename = this.callbacks.path.basename(filePath);
    this.filePath = filePath;
    this.fileType = KeymanFileTypes.sourceTypeFromFilename(this.filename);
  }
  getFileType() {
    return this.fileType;
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
