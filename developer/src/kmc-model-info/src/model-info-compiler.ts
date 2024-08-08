/**
 * Builds a source .model_info file with metadata extracted from .kps file and
 * compiled files to produce a comprehensive .model_info file.
 */

import { minKeymanVersion } from "./min-keyman-version.js";
import { ModelInfoFile } from "./model-info-file.js";
import { KmpJsonFile } from "@keymanapp/common-types";
import { ModelInfoCompilerMessages } from "./model-info-compiler-messages.js";
import { CompilerCallbacks, CompilerOptions, KeymanCompiler, KeymanCompilerArtifact, KeymanCompilerArtifacts, KeymanCompilerResult,KeymanUrls, isValidEmail, validateMITLicense } from "@keymanapp/developer-utils";

/* c8 ignore start */
/**
 * @public
 * Description of sources and metadata required to build a .model_info file
 */
export class ModelInfoSources {
  /** The identifier for the model */
  model_id: string;

  /** The data from the .kps file, transformed to kmp.json */
  kmpJsonData: KmpJsonFile.KmpJsonFile;

  /** The path in the keymanapp/lexical-models repo where this model may be found */
  sourcePath: string;

  /** The compiled model filename and relative path (.js) */
  modelFileName: string;

  /** The compiled package filename and relative path (.kmp) */
  kmpFileName: string;

  /** The source package filename and relative path (.kps) */
  kpsFilename: string;

  /** Last modification date for files in the project folder 'YYYY-MM-DDThh:mm:ssZ' */
  lastCommitDate?: string;

  /** Return an error if project does not meet requirements of lexical-models repository */
  forPublishing: boolean;
};
/* c8 ignore stop */

/**
 * @public
 * Options for the .model_info compiler
 */
export interface ModelInfoCompilerOptions extends CompilerOptions {
  /**
   * Description of sources and metadata required to build a .model_info file
   */
  sources: ModelInfoSources;
};

/**
 * @public
 * Internal in-memory build artifacts from a successful compilation
 */
export interface ModelInfoCompilerArtifacts extends KeymanCompilerArtifacts {
  /**
   * Binary model info filedata and filename - used by keyman.com
   */
  model_info: KeymanCompilerArtifact;
};

/**
 * @public
 * Build artifacts from the .model_info compiler
 */
export interface ModelInfoCompilerResult extends KeymanCompilerResult {
  /**
   * Internal in-memory build artifacts from a successful compilation. Caller
   * can write these to disk with {@link ModelInfoCompiler.write}
   */
  artifacts: ModelInfoCompilerArtifacts;
};

/**
 * @public
 * Compiles source data from a lexical model project to a .model_info. The
 * compiler does not read or write from filesystem or network directly, but
 * relies on callbacks for all external IO.
 */
export class ModelInfoCompiler implements KeymanCompiler {
  private callbacks: CompilerCallbacks;
  private options: ModelInfoCompilerOptions;

  constructor() {
  }

  /**
   * Initialize the compiler.
   * Copies options.
   * @param callbacks - Callbacks for external interfaces, including message
   *                    reporting and file io
   * @param options   - Compiler options
   * @returns false if initialization fails
   */
  public async init(callbacks: CompilerCallbacks, options: ModelInfoCompilerOptions): Promise<boolean> {
    this.callbacks = callbacks;
    this.options = {...options};
    return true;
  }

  /**
   * Builds .model_info file with metadata from the model and package source
   * file. Returns an object containing binary artifacts on success. The files
   * are passed in by name, and the compiler will use callbacks as passed to the
   * {@link ModelInfoCompiler.init} function to read any input files by disk.
   *
   * This function is intended for use within the lexical-models repository.
   * While many of the parameters could be deduced from each other, they are
   * specified here to reduce the number of places the filenames are
   * constructed.
   *
   * @param infile  - Path to source file. Path will be parsed to find relative
   *                  references in the .kpj file, such as .model.ts or
   *                  .model.kps file
   * @param outfile - Path to output file. The file will not be written to, but
   *                  will be included in the result for use by
   *                  {@link ModelInfoCompiler.write}.
   * @returns         Binary artifacts on success, null on failure.
   *
   * @param sources - Details on files from which to extract additional metadata
   */
  public async run(inputFilename: string, outputFilename?: string): Promise<ModelInfoCompilerResult> {
    const sources = this.options.sources;

    /*
      * Model info looks like this:
      *
      *  {
      *    "name": "Example Template Model"
      *    "license": "mit",
      *    "version": "1.0.0",
      *    "languages": ["en"],
      *    "authorName": "Example Author",
      *    "authorEmail": "nobody@example.com",
      *    "description": "Example wordlist model"
      *  }
      *
      * For full documentation, see:
      * https://help.keyman.com/developer/cloud/model_info/1.0/
      */

    let jsFile: string = null;

    if(sources.modelFileName) {
      jsFile = this.loadJsFile(sources.modelFileName);
      if(!jsFile) {
         return null;
      }
    }


    let model_info: ModelInfoFile = {
      languages: [],
    };

    //
    // Build .model_info file -- some fields have "special" behaviours -- see below
    // https://api.keyman.com/schemas/model_info.source.json and
    // https://api.keyman.com/schemas/model_info.distribution.json
    // https://help.keyman.com/developer/cloud/model_info/1.0
    //

    model_info.id = sources.model_id;
    model_info.name = sources.kmpJsonData.info.name.description;

    // License

    if(!sources.kmpJsonData.options?.licenseFile) {
      this.callbacks.reportMessage(ModelInfoCompilerMessages.Error_NoLicenseFound());
      return null;
    }

    if(!this.isLicenseMIT(this.callbacks.resolveFilename(sources.kpsFilename, sources.kmpJsonData.options.licenseFile))) {
      return null;
    }

    model_info.license = 'mit';

    const author = sources.kmpJsonData.info.author;
    model_info.authorName = author?.description ?? '';

    if (author?.url) {
      // we strip the mailto: from the .kps file for the .model_info
      const match = author.url.match(/^(mailto\:)?(.+)$/);
      /* c8 ignore next 3 */
      if (match === null) {
        this.callbacks.reportMessage(ModelInfoCompilerMessages.Error_InvalidAuthorEmail({email:author.url}));
        return null;
      }

      if(!isValidEmail(match[2])) {
        this.callbacks.reportMessage(ModelInfoCompilerMessages.Error_InvalidAuthorEmail({email:author.url}));
        return null;
      }

      model_info.authorEmail = match[2];
    }

    // description

    if(sources.kmpJsonData.info.description?.description) {
      model_info.description = sources.kmpJsonData.info.description.description.trim();
    }

    // isRTL -- this is a little bit of a heuristic from a compiled .js
    //          which may need modification if compilers change

    if(jsFile?.match(/("?)isRTL("?):\s*true/)) {
      model_info.isRTL = true;
    }

    // extract the language identifiers from the language metadata
    // arrays for each of the lexical models in the kmp.json file,
    // and merge into a single array of identifiers in the
    // .model_info file.

    model_info.languages = sources.kmpJsonData.lexicalModels.reduce((a, e) => [].concat(a, e.languages.map((f) => f.id)), []);

    // If a last commit date is not given, then just use the current time
    model_info.lastModifiedDate = sources.lastCommitDate ?? (new Date).toISOString();

    model_info.packageFilename = this.callbacks.path.basename(sources.kmpFileName);
    model_info.packageFileSize = this.callbacks.fileSize(sources.kmpFileName);
    if(model_info.packageFileSize === undefined) {
      this.callbacks.reportMessage(ModelInfoCompilerMessages.Error_FileDoesNotExist({filename:sources.kmpFileName}));
      return null;
    }

    model_info.jsFilename = this.callbacks.path.basename(sources.modelFileName);
    model_info.jsFileSize = this.callbacks.fileSize(sources.modelFileName);
    if(model_info.jsFileSize === undefined) {
      this.callbacks.reportMessage(ModelInfoCompilerMessages.Error_FileDoesNotExist({filename:sources.modelFileName}));
      return null;
    }

    model_info.packageIncludes = sources.kmpJsonData.files.filter((e) => !!e.name.match(/.[ot]tf$/i)).length ? ['fonts'] : [];
    model_info.version = sources.kmpJsonData.info.version.description;
    model_info.minKeymanVersion = minKeymanVersion;
    model_info.helpLink = KeymanUrls.HELP_MODEL(model_info.id);

    if(sources.sourcePath) {
      model_info.sourcePath = sources.sourcePath;
    }

    const jsonOutput = JSON.stringify(model_info, null, 2);
    const data = new TextEncoder().encode(jsonOutput);
    const result: ModelInfoCompilerResult = {
      artifacts: {
        model_info: {
          data,
          filename: outputFilename ?? inputFilename.replace(/\.kpj$/, '.model_info')
        }
      }
    };

    return result;
  }

  /**
   * Write artifacts from a successful compile to disk, via callbacks methods.
   * The artifacts written may include:
   *
   * - .model_info file - metadata file used by keyman.com
   *
   * @param artifacts - object containing artifact binary data to write out
   * @returns true on success
   */
  public async write(artifacts: ModelInfoCompilerArtifacts): Promise<boolean> {
    this.callbacks.fs.writeFileSync(artifacts.model_info.filename, artifacts.model_info.data);
    return true;
  }

  private isLicenseMIT(filename: string) {
    const data = this.callbacks.loadFile(filename);
    if(!data) {
      this.callbacks.reportMessage(ModelInfoCompilerMessages.Error_LicenseFileIsMissing({filename}));
      return false;
    }

    let license = null;
    try {
      license = new TextDecoder().decode(data);
    } catch(e) {
      this.callbacks.reportMessage(ModelInfoCompilerMessages.Error_LicenseFileIsDamaged({filename}));
      return false;
    }
    if(!license) {
      this.callbacks.reportMessage(ModelInfoCompilerMessages.Error_LicenseFileIsDamaged({filename}));
      return false;
    }
    const message = validateMITLicense(license);
    if(message != null) {
      this.callbacks.reportMessage(ModelInfoCompilerMessages.Error_LicenseIsNotValid({filename, message}));
      return false;
    }
    return true;
  }

  private loadJsFile(filename: string) {
    const data = this.callbacks.loadFile(filename);
    if(!data) {
      this.callbacks.reportMessage(ModelInfoCompilerMessages.Error_FileDoesNotExist({filename}));
      return null;
    }
    const text = new TextDecoder('utf-8', {fatal: true}).decode(data);
    return text;
  }
}