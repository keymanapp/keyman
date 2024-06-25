import { CompilerCallbacks, CompilerLogLevel, KeymanCompilerArtifact, KeymanCompilerArtifacts, KeymanCompilerResult } from "@keymanapp/common-types";
import { GeneratorMessages } from './generator-messages.js';


/**
 * @public
 * Options for the Keyman Developer project generator
 */
export interface GeneratorOptions /* not inheriting from CompilerBaseOptions */ {
  /**
   * Reporting level to console, used by NodeCompilerCallbacks (not used in
   * compiler modules; all messages are still reported to the internal log)
   */
  logLevel?: CompilerLogLevel;
  /**
   * identifier (basename) of the keyboard or model
   */
  id: string;
  /**
   * supported platforms, only used in Keyman keyboard project generator
   */
  targets?: string[];
  /**
   * output path where project folder will be created
   */
  outPath: string;
  /**
   * descriptive name of the keyboard or lexical model
   */
  name?: string;
  /**
   * name of the copyright holder for the keyboard or lexical model (do not
   * include (c) symbol or date)
   */
  copyright?: string;
  /**
   * version of the keyboard or model, 1.0 default for Keyman keyboard, lexical
   * model, 1.0.0 default for LDML keyboard
   */
  version?: string;
  /**
   * array of bcp 47 tags which are supported by the keyboard or lexical model
   */
  languageTags?: string[];
  /**
   * name of the author of the keyboard
   */
  author?: string;
  /**
   * set to true to generate an icon for a Keyman keyboard with the first
   * characters of the first specified BCP 47 tag
   */
  icon?: boolean;
  /**
   * description of the keyboard, Markdown permissible
   */
  description?: string;
  /**
   * version of Keyman to reference in source files, defaults to KEYMAN_VERSION.VERSION
   */
  keymanVersion?: string;
};

/**
 * @public
 * Internal in-memory build artifacts from a successful generation
 */
export interface GeneratorArtifacts extends KeymanCompilerArtifacts {
  /**
   * Generated project files to be written to disk
   */
  [name:string]: KeymanCompilerArtifact;
};

/**
 * @public
 * Result of a successful generation
 */
export interface GeneratorResult extends KeymanCompilerResult {
  /**
   * Internal in-memory build artifacts from a successful compilation. Caller
   * can write these to disk with {@link AbstractGenerator.write}
   */
  artifacts: GeneratorArtifacts;
};

/**
 * @public
 * Common functionality for generating projects. Do not instantiate
 * this class, rather instantiate a subclass
 */
export class AbstractGenerator {
  /**
   * id for the keyboard or model, aka the basename sans extension, foldername
   * for the project
   */
  // protected get id() { return this._id }
  // private _id: string;

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

  protected get callbacks(): CompilerCallbacks { return this._callbacks; }
  private _callbacks: CompilerCallbacks;
  protected get options(): GeneratorOptions { return this._options; }
  private _options: GeneratorOptions;

  /**
   * Initialize the generator. Copies options.
   * @param callbacks - Callbacks for external interfaces, including message
   *                    reporting and file io
   * @param options   - Generator options
   * @returns           false if initialization fails
   */
  public async init(callbacks: CompilerCallbacks, options: GeneratorOptions): Promise<boolean> {
    this._callbacks = callbacks;
    this._options = {...options};
    // this._id = options.id;
    this._includedPrefixes = [];
    this._filenameMap = {};
    this._tokenMap = {};

    this._templateBasePath = this.callbacks.path.join(
      this.callbacks.path.dirname(this.callbacks.fileURLToPath(import.meta.url)),
      'template'
    );

    // These files are currently always included, for all project types
    this.filenameMap[AbstractGenerator.SFile_WelcomeHTM] = AbstractGenerator.SFile_WelcomeHTM;
    this.filenameMap[AbstractGenerator.SFile_ReadmeHTM] = AbstractGenerator.SFile_ReadmeHTM;
    this.filenameMap[AbstractGenerator.SFile_HistoryMD] = AbstractGenerator.SFile_HistoryMD;
    this.filenameMap[AbstractGenerator.SFile_LicenseMD] = AbstractGenerator.SFile_LicenseMD;
    this.filenameMap[AbstractGenerator.SFile_ReadmeMD] = AbstractGenerator.SFile_ReadmeMD;

    return true;
  }

  /**
   * Write artifacts from a successful compile to disk, via callbacks methods.
   * The artifacts written will include all files from the project, across
   * multiple folders. Folders will be created as needed
   *
   * @param artifacts - object containing artifact binary data to write out
   * @returns true on success
   */
  public async write(artifacts: GeneratorArtifacts): Promise<boolean> {
    // TODO: this is a little poor because it is carrying state over from the
    // previous 'run' call, rather than figuring out the target path from the
    // artifacts. Probably should be looking at highest common folder, and then
    // failing if that path exists
    if(this.targetPathExists()) {
      this.callbacks.reportMessage(GeneratorMessages.Error_OutputPathAlreadyExists({outPath: this.targetPath()}));
      return false;
    }

    for(const key of Object.keys(artifacts)) {
      const a = artifacts[key];
      const path = this.callbacks.path.dirname(a.filename);
      try {
        this.callbacks.fs.mkdirSync(path, {recursive: true});
      /* c8 ignore next 4 */
      } catch(e) {
        this.callbacks.reportMessage(GeneratorMessages.Error_CannotCreateFolder({folderName:path, e}));
        return false;
      }
      try {
        this.callbacks.fs.writeFileSync(a.filename, a.data);
      } catch(e) {
        this.callbacks.reportMessage(GeneratorMessages.Error_CannotWriteOutputFile({filename:a.filename, e}));
        return false;
      }
    }
    return true;
  }

  protected readonly targetPath = () => this.callbacks.path.join(this.options.outPath,this.options.id);
  private readonly targetPathExists = () => this.callbacks.fs.existsSync(this.targetPath());

  /**
   * @internal
   */
  public readonly unitTestEndpoints = {
    targetPath: this.targetPath
  }
  /**
   * @internal
   */
  public get test_tokenMap() { return this._tokenMap; }
}

