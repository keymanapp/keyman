import * as fs from 'fs'; //TODO
import { CompilerCallbacks, CompilerLogLevel } from "@keymanapp/common-types";
import { fileURLToPath } from 'url';

export interface GeneratorOptions /* not inheriting from CompilerBaseOptions */ {
  /**
   * Reporting level to console, used by NodeCompilerCallbacks (not used in
   * compiler modules; all messages are still reported to the internal log)
   */
  logLevel?: CompilerLogLevel;
  targets: string[];
  outPath: string;
  name: string;
  copyright?: string;
  version: string;
  languageTags: string[];
  author?: string;
  icon: Boolean;
  description?: string;
};

export class AbstractGenerator {
  /**
   * id for the keyboard or model, aka the basename sans extension, foldername
   * for the project
   */
  protected get id() { return this._id }
  private _id: string;

  /**
   * extension of options.copyright including copyright year
   */
  protected fullCopyright: string = '';

  /**
   * identifiers for lines to include when transforming template files, filled
   * by child classes
   */
  protected get includedPrefixes() { return this._includedPrefixes; }
  private _includedPrefixes: string[];

  /**
   * tokens to rewrite in output files
   */
  protected get tokenMap() { return this._tokenMap; }
  private _tokenMap: {[index:string]: string};

  /**
   * map of all files to be transformed, filled by this class and subclasses
   */
  protected get filenameMap() { return this._filenameMap; }
  private _filenameMap: {[index:string]: string};

  /**
   * base path for template files in this module
   */
  protected get templateBasePath() { return this._templateBasePath; }
  private _templateBasePath: string;

  protected static readonly SPath_Source = 'source/';
  protected static readonly SFile_WelcomeHTM = `${this.SPath_Source}welcome.htm`;
  protected static readonly SFile_ReadmeHTM = `${this.SPath_Source}readme.htm`;
  protected static readonly SFile_HistoryMD = 'HISTORY.md';
  protected static readonly SFile_LicenseMD = 'LICENSE.md';
  protected static readonly SFile_ReadmeMD = 'README.md';

  constructor(protected callbacks: CompilerCallbacks, protected options: GeneratorOptions) {}

  async init(id: string): Promise<boolean> {
    this._id = id;
    this._includedPrefixes = [];
    this._filenameMap = {};
    this._tokenMap = {};

    this._templateBasePath = this.callbacks.path.join(
        this.callbacks.path.dirname(fileURLToPath(import.meta.url)),
        'template'
      )
    ;


    // These files are currently always included, for all project types
    this.filenameMap[AbstractGenerator.SFile_WelcomeHTM] = AbstractGenerator.SFile_WelcomeHTM;
    this.filenameMap[AbstractGenerator.SFile_ReadmeHTM] = AbstractGenerator.SFile_ReadmeHTM;
    this.filenameMap[AbstractGenerator.SFile_HistoryMD] = AbstractGenerator.SFile_HistoryMD;
    this.filenameMap[AbstractGenerator.SFile_LicenseMD] = AbstractGenerator.SFile_LicenseMD;
    this.filenameMap[AbstractGenerator.SFile_ReadmeMD] = AbstractGenerator.SFile_ReadmeMD;
    return true;
  }

  /**
   * Create folders and verify accessibility
   * @returns
   */
  async run(): Promise<boolean> {

    if(this.targetPathExists()) {
      // TODO yammer
      console.error('target path exists');
      return false;
    }
    if(!this.createTargetPath()) {
      // TODO yammer
      console.error('failed creating target path');
      return false;
    }

    return true;
  }

  protected readonly targetPath = () => this.callbacks.path.join(this.options.outPath,this.id);

  private readonly targetPathExists = () => this.callbacks.fs.existsSync(this.targetPath());

  private createTargetPath() {
    /*TODO: this.callbacks.*/fs.mkdirSync(this.callbacks.path.join(this.targetPath(),'source'), {recursive:true});
    /*if(!) {
      // TODO log exception
      // raise EKeyboardProjectTemplate.Create('Could not create destination path '+BasePath+ID);
      return false;
    }*/
    return true;
  }
}

