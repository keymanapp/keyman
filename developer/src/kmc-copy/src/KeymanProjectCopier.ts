/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Copy or rename a keyboard or lexical model project
 */

/* copy a project to a new folder, new name */

import { CompilerCallbacks, CompilerLogLevel, KeymanCompiler, KeymanCompilerArtifact, KeymanCompilerArtifacts, KeymanCompilerResult } from "@keymanapp/developer-utils";

import { CopierMessages } from "./copier-messages.js";

// kmc copy release/k/khmer_angkor -o release/m/marc_khmer

/**
 * @public
 * Options for the Keyman Developer project copier
 */
export interface CopierOptions /* not inheriting from CompilerBaseOptions */ {
  /**
   * Reporting level to console, used by NodeCompilerCallbacks (not used in
   * compiler modules; all messages are still reported to the internal log)
   */
  logLevel?: CompilerLogLevel;
  /**
   * output path where project folder will be created
   */
  outPath: string;
  /**
   * rename: rename existing files rather than copy
   */
  rename: boolean;
  /**
   * dryRun: show what would happen
   */
  dryRun: boolean;
};

/**
 * @public
 * Internal in-memory build artifacts from a successful copy
 */
export interface CopierArtifacts extends KeymanCompilerArtifacts {
  /**
   * Copied project files to be written to disk
   */
  [name:string]: KeymanCompilerArtifact;
};

/**
 * @public
 * Result of a successful copy
 */
export interface CopierResult extends KeymanCompilerResult {
  /**
   * Internal in-memory build artifacts from a successful compilation. Caller
   * can write these to disk with {@link KeymanProjectCopier.write}
   */
  artifacts: CopierArtifacts;
};

export class KeymanProjectCopier implements KeymanCompiler {
  options: CopierOptions;
  callbacks: CompilerCallbacks;

  async init(callbacks: CompilerCallbacks, options: CopierOptions): Promise<boolean> {
    this.callbacks = callbacks;
    this.options = options;
    return true;
  }

  /**
   * Copy a Keyman project. Returns an object containing binary
   * artifacts on success. The files are passed in by name, and the compiler
   * will use callbacks as passed to the {@link KeymanProjectCopier.init}
   * function to read any input files by disk.
   * @returns         Binary artifacts on success, null on failure.
   */
  async run(source: string): Promise<CopierResult> {
    return null;
  }

  private readonly targetPathExists = () => this.callbacks.fs.existsSync(this.options.outPath);

  // TODO-COPY: consolidate with kmc-generate.write
  /**
   * Write artifacts from a successful compile to disk, via callbacks methods.
   * The artifacts written will include all files from the project, across
   * multiple folders. Folders will be created as needed
   *
   * @param artifacts - object containing artifact binary data to write out
   * @returns true on success
   */
  public async write(artifacts: CopierArtifacts): Promise<boolean> {
    // TODO-GENERATE: this is a little poor because it is carrying state over from the
    // previous 'run' call, rather than figuring out the target path from the
    // artifacts. Probably should be looking at highest common folder, and then
    // failing if that path exists
    if(this.targetPathExists()) {
      this.callbacks.reportMessage(CopierMessages.Error_OutputPathAlreadyExists({outPath: this.options.outPath}));
      return false;
    }

    for(const key of Object.keys(artifacts)) {
      const a = artifacts[key];
      const path = this.callbacks.path.dirname(a.filename);
      try {
        this.callbacks.reportMessage(CopierMessages.Info_CreatingFolder({path}));
        if(!this.options.dryRun) {
          this.callbacks.fs.mkdirSync(path, {recursive: true});
        }
      /* c8 ignore next 4 */
      } catch(e) {
        this.callbacks.reportMessage(CopierMessages.Error_CannotCreateFolder({folderName:path, e}));
        return false;
      }
      try {
        this.callbacks.reportMessage(CopierMessages.Info_WritingFile({filename:a.filename}));
        if(!this.options.dryRun) {
          this.callbacks.fs.writeFileSync(a.filename, a.data);
        }
      } catch(e) {
        this.callbacks.reportMessage(CopierMessages.Error_CannotWriteOutputFile({filename:a.filename, e}));
        return false;
      }
    }
    return true;
  }
}