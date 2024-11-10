//
// Version 1.0 and 2.0 of Keyman Developer Project .kpj file
//

import { KeymanFileTypes } from '@keymanapp/common-types';
import { CompilerAsyncCallbacks, CompilerPathCallbacks } from "../../compiler-callbacks.js";

export class KeymanDeveloperProject {
  options: KeymanDeveloperProjectOptions;
  files: KeymanDeveloperProjectFile[];
  projectPath: string = '';
  readonly projectFile: KeymanDeveloperProjectFile;

  get projectFilename() {
    return this._projectFilename;
  }

  constructor(
    private _projectFilename: string,
    version: KeymanDeveloperProjectVersion,
    protected callbacks: CompilerAsyncCallbacks
  ) {
    this.projectPath = this.callbacks.path.dirname(this._projectFilename);
    this.options = new KeymanDeveloperProjectOptions(version);
    this.files = [];
    this.projectFile = new KeymanDeveloperProjectFile20(_projectFilename, this.callbacks.path);
  }
  /**
   * Adds .kmn, .xml, .kps to project based on options.sourcePath
   * @param projectFilename Full path to project.kpj (even if the file doesn't exist)
   */
  async populateFiles(): Promise<boolean> {
    if(this.options.version != '2.0') {
      throw new Error('populateFiles can only be called on a v2.0 project');
    }
    const sourcePath = this.resolveProjectPath(this.options.sourcePath);
    if(!await this.callbacks.fsAsync.exists(sourcePath)) {
      return false;
    }
    const files = await this.callbacks.fsAsync.readdir(sourcePath);
    for(const file of files.filter(file => file.type == 'file')) {
      const fullPath = this.callbacks.path.join(sourcePath, file.filename);
      if(KeymanFileTypes.filenameIs(file.filename, KeymanFileTypes.Source.LdmlKeyboard)) {
        try {
          const data = await this.callbacks.fsAsync.readFile(fullPath);
          const text = new TextDecoder().decode(data);
          if(!text?.match(/<keyboard3/)) {
            // Skip this .xml because we assume it isn't really a keyboard .xml
            continue;
          }
        } catch(e) {
          // We'll just silently skip this file because we were not able to load it,
          // so let's hope it wasn't a real LDML keyboard XML :-)
          continue;
        }
      }
      if(KeymanFileTypes.sourceTypeFromFilename(file.filename) !== null) {
        const file = new KeymanDeveloperProjectFile20(fullPath, this.callbacks.path);
        this.files.push(file);
      }
    }
    return this.files.length > 0;
  }

  public isKeyboardProject() {
    return !!this.files.find(file => file.fileType == KeymanFileTypes.Source.KeymanKeyboard || file.fileType == KeymanFileTypes.Source.LdmlKeyboard);
  }

  public isLexicalModelProject() {
    return !!this.files.find(file => file.fileType == KeymanFileTypes.Source.Model);
  }

  private resolveProjectPath(p: string): string {
    // Replace placeholders in the target path
    return p.replace('$PROJECTPATH', this.projectPath);
  }

  resolveBuildPath(): string {
    // Roughly corresponds to Delphi TProject.GetTargetFileName
    let p = this.options.version == '1.0' ?
      this.options.buildPath || '$SOURCEPATH' :
      this.options.buildPath;

    // Replace placeholders in the target path
    if(this.options.version == '1.0') {
      // TODO: do we need to support $VERSION?
      // $SOURCEPATH only supported in 1.0 projects
      p = p.replace('$SOURCEPATH', this.callbacks.path.dirname(this._projectFilename));
    }

    p = this.resolveProjectPath(p);

    return p;
  }

  getOutputFilePath(type: KeymanFileTypes.Binary) {
    const p = this.resolveBuildPath();
    const f = this.callbacks.path.basename(this._projectFilename, KeymanFileTypes.Source.Project) + type;
    return this.callbacks.path.normalize(this.callbacks.path.join(p, f));
  }

  resolveInputFilePath(file: KeymanDeveloperProjectFile): string {
    return this.callbacks.fsAsync.resolveFilename(this._projectFilename, file.filePath);
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

    const f = file.filename.replace(new RegExp(`\\${sourceExt}$`, 'i'), targetExt);
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
  checkFilenameConventions: boolean = false;  // missing option defaults to False
  /**
   * Skip building .keyboard_info and .model_info files, for example in
   * unit tests or for legacy keyboards
   */
  skipMetadataFiles: boolean;
  projectType: KeymanDeveloperProjectType = KeymanDeveloperProjectType.Keyboard;
  readonly version: KeymanDeveloperProjectVersion;
  constructor(version: KeymanDeveloperProjectVersion) {
    this.version = version;
    switch(version) {
      case "1.0":
        this.buildPath = '';
        this.sourcePath = '';
        this.skipMetadataFiles = true;
        break;
      case "2.0":
        this.buildPath = '$PROJECTPATH/build';
        this.sourcePath = '$PROJECTPATH/source';
        this.skipMetadataFiles = false;
        break;
      default:
        throw new Error('Invalid version');
    }
  }
};

export interface KeymanDeveloperProjectFile {
  get filename(): string;
  get fileType(): string;
  readonly filePath: string;
};

export class KeymanDeveloperProjectFile10 implements KeymanDeveloperProjectFile {
  get filename(): string {
    return this.path.basename(this.filePath);
  }
  get fileType(): string {
    return KeymanFileTypes.fromFilename(this.filename);
  }
  details: KeymanDeveloperProjectFileDetail_Kmn & KeymanDeveloperProjectFileDetail_Kps; // 1.0 only
  childFiles: KeymanDeveloperProjectFile[]; // 1.0 only

  constructor(public readonly id: string, public readonly filePath: string, public readonly fileVersion:string, private path: CompilerPathCallbacks) {
    this.details = {};
    this.childFiles = [];
  }
};

export type KeymanDeveloperProjectFileType20 = KeymanFileTypes.Source;

export class KeymanDeveloperProjectFile20 implements KeymanDeveloperProjectFile {
  get filename(): string {
    return this.path.basename(this.filePath);
  }
  get fileType() {
    return KeymanFileTypes.fromFilename(this.filename);
  }
  constructor(public readonly filePath: string, private path: CompilerPathCallbacks) {
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
