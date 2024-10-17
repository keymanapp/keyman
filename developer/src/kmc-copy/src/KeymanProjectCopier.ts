/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Copy or rename a keyboard or lexical model project
 */

/* copy a project to a new folder, new name */

import { CompilerCallbacks, CompilerLogLevel, KeymanCompiler, KeymanCompilerArtifact, KeymanCompilerArtifacts, KeymanCompilerResult, KeymanDeveloperProject, KeymanDeveloperProjectFile, KeymanDeveloperProjectOptions, KPJFileReader, KPJFileWriter, KpsFileReader, KpsFileWriter } from "@keymanapp/developer-utils";

import { CopierMessages } from "./copier-messages.js";
import { KeymanFileTypes } from "@keymanapp/common-types";

// TODO-COPY: remove references to 'path.relative'
import * as path from 'node:path';

// kmc copy release/k/khmer_angkor -o release/m/marc_khmer


// TODO-COPY: cleanup the docs below
  /*

     Files and file locations
     ------------------------

     Project folder is the one that contains .kpj file. It and subfolders are
     considered 'special'.

     * All source-type files explicitly referenced in .kpj will be copied to new
       folder /source, and added to reference
       - source-type files: kmn, kps, model.ts, xml (LDML)
     * All subfiles referenced by .kmn, .model.ts will be copied to new folder
       /source
     * All other files in the project tree, if not already referenced, will be
       copied to new folder
     * All referenced files in .kmn, .kpj, .model.ts will be renamed /
       moved as appropriate
     * .kps references to other files will _not_ be modified. .kps references
       to .kmx, .model.js, .js with corresponding .kmn, .xml, or .model.ts, will be
       updated to point to build/
     * .kpj options will be updated to use fixed source/ and build/ folders.

  */

       // files to skip from copy are listed in processedFiles, copy all others exc. .kpj.user, project.options.buildPath
    // watch out for file collisions

    // ldml project
    // .kpj {update .xml, .kps}
    // .xml
    // .kps {update .kmx, .kvk, Keyboards}

    // all remaining files in the folder
    // don't copy .kpj.user
    // don't copy output path (per project)

    // model project
    // .kpj {update .model.ts, .model.kps}
    // .model.ts
    // .model.kps {update .model.js, Models}

    // all remaining files in the folder
    // don't copy .kpj.user
    // don't copy output path (per project)

    // phase 1: copy all files, unmodified, but with filenames transformed

    // keyman project - any type
    // .kpj {update .kps, .kmn}
    // .kmn {update .kvks, .keyman-touch-layout, .ico?, kmwhelp, ...}
    // .xml {no update required}
    // .kvks {update kmn back-link?}
    // .keyman-touch-layout
    // .kps {update .kmx, .kvk, .js, .ico?, Keyboards}
    // .ico {if matching id}
    // .kpj {update .model.ts, .model.kps}
    // .model.ts
    // .model.kps {update .model.js, Models}

    // all remaining files in the project (no unreferenced files?)
    // don't copy .kpj.user
    // don't copy output path (per project)


    // When copying a project, we want to consider:
    // a. 'source' file with matching ID in source/ subfolder --> rename, copy to source/ in target
    // b. 'source' file with matching ID outside project tree --> copy to source/ in target
    // c. other file in source/ subfolder --> keep in same location in target
    // d. other file outside project tree --> move to source/ in target? (except fonts + related?)


// TODO-COPY: safely handle / and \ paths (normalize all to / first?)

interface ProcessedFile {
  file: KeymanDeveloperProjectFile;
  newFilename: string;
}

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

  sourceId: string;
  outputId: string;

  async init(callbacks: CompilerCallbacks, options: CopierOptions): Promise<boolean> {
    this.callbacks = callbacks;
    this.options = options;
    return true;
  }

  /**
   *
   * @param source
   * @returns   temp - path to temporary folder to be removed on success or failure, projectSource - path to kpj
   */
  async getSourceProject(source: string): Promise<{temp: string, projectSource: string}> {
    let temp: string = null, projectSource: string = null;

    if(source.startsWith('github:')) {
      // TODO-COPY: temp = downloadFromGithub(source);
      projectSource = temp; // TODO-COPY: +.kpj
    } else if(source.startsWith('cloud:')) {
      // TODO-COPY: temp = downloadFromKeymanCloud(source);
      projectSource = temp; // TODO-COPY: +.kpj
    } else if(this.callbacks.fs.existsSync(source) && source.endsWith('.kpj')) {
      // already the project file
      projectSource = source;
    } else {
      projectSource = this.callbacks.path.join(source, this.callbacks.path.basename(source) + '.kpj');
      if(!this.callbacks.fs.existsSync(projectSource)) {
        this.callbacks.reportMessage(CopierMessages.Error_CannotFindInputProject({project: source}));
        projectSource = null;
      }
    }
    return { temp, projectSource };
  }

  /**
   * Copy a Keyman project. Returns an object containing binary
   * artifacts on success. The files are passed in by name, and the compiler
   * will use callbacks as passed to the {@link KeymanProjectCopier.init}
   * function to read any input files by disk.
   * @param   source  Source file or folder to copy. Can be a local file or folder, github:repo, or cloud:path/id
   * @returns         Binary artifacts on success, null on failure.
   */
  async run(source: string): Promise<CopierResult> {
    const { temp, projectSource } = await this.getSourceProject(source);
    if(!projectSource) {
      // error will have been raised in getSourceProject
      return null;
    }

    if(this.options.outPath.endsWith('/') || this.options.outPath.endsWith('\\')) {
      this.options.outPath = this.options.outPath.substring(0, this.options.outPath.length - 1);
    }

    this.sourceId = this.callbacks.path.basename(projectSource, '.kpj');
    this.outputId = this.callbacks.path.basename(this.options.outPath);

    // TODO-COPY: validate outputId, outpath

    // TODO-COPY: handle .model.js, .model.ts extensions -- these will not match with the baseid pattern (can we use KeymanFileTypes to avoid?)

    // copy project file
    const project = this.loadProjectFromFile(projectSource);
    if(!project) {
      // loadProjectFromFile already reported errors
      return null;
    }

    const result: CopierResult = {
      artifacts: {}
    };
    let success: boolean = false;

    success = this.copyProjectFile(project, projectSource, this.options.outPath, projectSource, result);

    // TODO-COPY: consider adding standalone repo files, .gitignore, .github action etc

    if(temp) {
      // TODO-COPY: this.cleanupTemp(temp);
    }

    if(!success) {
      return null;
    }

    return result;
  }

  isKeymanKeyboardProject(project: KeymanDeveloperProject): boolean {
    return !!project.files.find(file => file.filePath.toLowerCase().endsWith(KeymanFileTypes.Source.KeymanKeyboard));
  }

  copiers: {[index in KeymanFileTypes.Source]: (project: KeymanDeveloperProject, filename: string, outputPath: string, source: string, result: CopierResult) => boolean} = {
    ".kpj": this.copyProjectFile.bind(this),
    ".kmn": this.copyKmnSourceFile.bind(this),
    ".kps": this.copyKpsSourceFile.bind(this),
    ".kvks": this.copySourceFile.bind(this),
    ".keyman-touch-layout": this.copySourceFile.bind(this),
    ".xml": this.copySourceFile.bind(this),
    ".model.ts": this.copyModelTsSourceFile.bind(this),
  };

  copyProjectFile(project: KeymanDeveloperProject, filename: string, outputPath: string, source: string, result: CopierResult): boolean {
    for(const file of project.files) {
      const copier = this.copiers[<KeymanFileTypes.Source> file.fileType] ?? this.copyGenericFile.bind(this);

      const subOutputPath = this.calculateNewFilePath(project.projectPath, file.filePath);
      const subFilename = project.resolveInputFilePath(file);

      // Ignore errors because we will continue to do a best effort
      copier(project, subFilename, subOutputPath, source, result);
    }

    // Rewrite the project

    project.options = new KeymanDeveloperProjectOptions('2.0');
    const writer = new KPJFileWriter();
    const data = new TextEncoder().encode(writer.write(project));

    result.artifacts[filename] = {
      data,
      filename: this.generateNewFilename(filename, outputPath)
    };

    return true;
  }

  copySourceFile(project: KeymanDeveloperProject, filename: string, outputPath: string, source: string, result: CopierResult): boolean {
    return this.copyGenericFile(project, filename, this.callbacks.path.join(this.options.outPath, 'source'), source, result);
  }

  copyGenericFile(project: KeymanDeveloperProject, filename: string, outputPath: string, source: string, result: CopierResult): boolean {
    if(result.artifacts[filename]) {
      return true;
    }

    const data = this.callbacks.loadFile(filename);
    if(!data) {
      // We'll add an null artifact and do a best effort, rather
      // than fail the process altogether.
      result.artifacts[filename] = {
        data: null,
        filename: this.generateNewFilename(filename, outputPath)
      };

      // Only warn that the file is missing if it isn't an output file,
      // because a clean project won't contain any outputs (binaries)
      if(KeymanFileTypes.binaryTypeFromFilename(filename) === null) {
        this.callbacks.reportMessage(CopierMessages.Warn_FileNotFound({filename, newFilename: result.artifacts[filename].filename}));
      }

      return false;
    }

    result.artifacts[filename] = {
      data,
      filename: this.generateNewFilename(filename, outputPath)
    };

    return true;
  }

  copyModelTsSourceFile(project: KeymanDeveloperProject, filename: string, outputPath: string, source: string, result: CopierResult): boolean {
    if(!this.copySourceFile(project, filename, outputPath, source, result)) {
      return false;
    }

    // Replace wordlist references and update files

    const lines = new TextDecoder().decode(result.artifacts[filename].data);

    const sources = lines.match(/(sources:\s*\[)([^\]]+)(\])/m);
    if(!sources) {
      this.callbacks.reportMessage(CopierMessages.Warn_NoWordlistsFound({filename}));
      return true;
    }

    const wordlistString = sources[2];
    const wordlists = wordlistString.matchAll(/(['"])(.+)(\1)/g);

    let index = 0;
    let out: string = '';
    for(const wordlist of wordlists) {
      const wordlistFilename = wordlist[2];
      const newRelativePath = this.copySubFileAndGetRelativePath(project, filename, wordlistFilename, outputPath, source, result);

      out += wordlistString.substring(index, wordlist.index) + wordlist[1] + newRelativePath + wordlist[3];
      index = wordlist.index + wordlist[0].length;
    }
    out += wordlistString.substring(index);
    const outLines = lines.substring(0, sources.index ?? 0) + sources[1] + out + sources[3] + lines.substring((sources.index ?? 0) + sources[0].length);

    result.artifacts[filename].data = new TextEncoder().encode(outLines);

    return true;
  }

  copySubFileAndGetRelativePath(project: KeymanDeveloperProject, parentFilename: string, originalSubfilename: string, outputPath: string, source: string, result: CopierResult): string {
    const subFilename = this.callbacks.resolveFilename(parentFilename, originalSubfilename);
    const subFilenameAbsolute = this.callbacks.resolveFilename(project.projectPath, subFilename);
    const subFilenameRelative = path.relative(project.projectPath, subFilenameAbsolute);

    if(this.callbacks.path.isAbsolute(subFilenameRelative) || subFilenameRelative.startsWith('..')) {
      // Reference outside the project structure, do not attempt to normalize or copy,
      // but we do need to update references for the new output path
      return path.relative(result.artifacts[parentFilename].filename, subFilenameAbsolute);
    } else {
      const subOutputPath = this.callbacks.path.join(outputPath, this.callbacks.path.dirname(originalSubfilename));
      this.copyGenericFile(project, subFilename, subOutputPath, source, result);

      // Even if the subfile is missing, we'll still continue the overall copy
      return path.relative(this.callbacks.path.dirname(result.artifacts[parentFilename].filename), result.artifacts[subFilename].filename);
    }
  }

  copyKmnSourceFile(project: KeymanDeveloperProject, filename: string, outputPath: string, source: string, result: CopierResult): boolean {
    if(!this.copySourceFile(project, filename, outputPath, source, result)) {
      return false;
    }

    // Replace file references for known files
    const stores = ['&BITMAP', '&DISPLAYMAP', '&INCLUDECODES', '&KMW_EMBEDCSS', '&KMW_EMBEDJS', '&KMW_HELPFILE', '&LAYOUTFILE', '&VISUALKEYBOARD'];
    const storeRegexes = stores.map(m => ({ id: m, re: new RegExp(`^\\s*store\\s*\\(\\s*${m}\\s*\\)\\s*(['"])(.+?)(\\1)`, 'i')}));
    const replacementRegex = /^(\s*store\s*\(\s*&[a-z0-9-_]+\s*\)\s*(['"]))(.+?)(\2.*)$/i;

    const lines = new TextDecoder().decode(result.artifacts[filename].data).replaceAll(/\r\n/g, '\n').replaceAll(/\r/g, '\n').split('\n');
    for(let i = 0; i < lines.length; i++) {
      const line = lines[i];
      for(const r of storeRegexes) {
        const m = line.match(r.re);
        if(m) {
          const newRelativePath = this.copySubFileAndGetRelativePath(project, filename, m[2], outputPath, source, result);
          lines[i] = line.replace(replacementRegex, `$1${newRelativePath}$4`);
        }
      }
    }

    // rewrite fixups inside the source file
    result.artifacts[filename].data = new TextEncoder().encode(lines.join('\n'));

    return true;
  }

  copyKpsSourceFile(project: KeymanDeveloperProject, filename: string, outputPath: string, source: string, result: CopierResult): boolean {
    if(!this.copySourceFile(project, filename, outputPath, source, result)) {
      return false;
    }

    const reader = new KpsFileReader(this.callbacks);
    const kps = reader.read(result.artifacts[filename].data);
    if(!kps) {
      this.callbacks.reportMessage(CopierMessages.Error_PackageFileCouldNotBeRead({filename}));
      return false;
    }

    if(kps.Package?.Files?.File.length) {
      for(const file of kps.Package.Files.File) {
        const subFilename = this.callbacks.resolveFilename(filename, file.Name);
        const newRelativePath = this.copySubFileAndGetRelativePath(project, filename, file.Name, outputPath, source, result);

        const remap = (ref?: string) => ref == subFilename ? newRelativePath : ref;

        // Update all other file links in the .kps
        kps.Package.Options.GraphicFile = remap(kps.Package.Options.GraphicFile);
        kps.Package.Options.LicenseFile = remap(kps.Package.Options.LicenseFile);
        kps.Package.Options.MSIFileName = remap(kps.Package.Options.MSIFileName);
        kps.Package.Options.ReadMeFile = remap(kps.Package.Options.ReadMeFile);
        kps.Package.Options.WelcomeFile = remap(kps.Package.Options.WelcomeFile);
        if(kps.Package?.Keyboards?.Keyboard?.length) {
          for(const keyboard of kps.Package.Keyboards?.Keyboard) {
            keyboard.DisplayFont = remap(keyboard.DisplayFont);
            keyboard.OSKFont = remap(keyboard.OSKFont);
            if(keyboard.WebDisplayFonts?.Font?.length) {
              for(const font of keyboard.WebDisplayFonts.Font) {
                font.$.Filename = remap(font.$.Filename);
              }
            }
            if(keyboard.WebOSKFonts?.Font?.length) {
              for(const font of keyboard.WebOSKFonts.Font) {
                font.$.Filename = remap(font.$.Filename);
              }
            }

            // Rewrite keyboard id
            if(keyboard.ID == this.sourceId) {
              keyboard.ID = this.outputId;
            }
          }
        }

        if(kps.Package?.LexicalModels?.LexicalModel?.length) {
          for(const model of kps.Package.LexicalModels?.LexicalModel) {
            // Rewrite keyboard id
            if(model.ID == this.sourceId) {
              model.ID = this.outputId;
            }
          }
        }

        // Rewrite
        file.Name = newRelativePath;
      }
    }

    // Rewrite the .kps artifact

    const writer = new KpsFileWriter();
    const output = writer.write(kps);
    result.artifacts[filename].data = new TextEncoder().encode(output);

    return true;
  }

  processedFiles: ProcessedFile[] = [];

  splitFilename(filename: string): {ext: string, path: string, base: string} {
    // We have a special case for .model.kps, as this is not a specific file
    // type in KeymanFileTypes, but we still need to transform the file basename
    const ext = filename.endsWith('.model.kps') ? '.model.kps' : KeymanFileTypes.fromFilename(filename);
    const path = this.callbacks.path.dirname(filename);
    const base = this.callbacks.path.basename(filename, ext);
    return { ext, path, base };
  }

  /**
   * renames matching filename to the output filename pattern, and prepends the
   * outputPath
   * @param filename
   * @param outputPath
   * @returns
   */
  generateNewFilename(filename: string, outputPath: string): string {
    const {ext, base} = this.splitFilename(filename);
    const newFilename = base == this.sourceId
      ? this.callbacks.path.join(outputPath, this.outputId + ext)
      : this.callbacks.path.join(outputPath, base + ext);

    return newFilename;
  }

  calculateNewFilePath(oldProjectFilename: string, oldFilePath: string) {
    // Do not change filename here, just its path

    const {ext, base} = this.splitFilename(oldFilePath);
    const filename = base + ext;

    const sourceType = KeymanFileTypes.sourceTypeFromFilename(oldFilePath);

    if(sourceType === null) {
      // Standard metadata files -- root of project
      if(filename === KeymanFileTypes.HISTORY_MD || filename === KeymanFileTypes.README_MD || filename === KeymanFileTypes.LICENSE_MD) {
        return this.options.outPath;
      }
      // Other files referenced in project, maintain original path
      return this.callbacks.path.dirname(path.relative(oldProjectFilename, oldFilePath));
    }

    if(sourceType == KeymanFileTypes.Source.Project) {
      // Project file -- root of project
      return this.options.outPath;
    }

    // source type is valid
    return this.callbacks.path.join(this.options.outPath, 'source');
  }

  private loadProjectFromFile(filename: string): KeymanDeveloperProject {
    const kpjData = this.callbacks.loadFile(filename);
    const reader = new KPJFileReader(this.callbacks);
    let kpj = null;
    try {
      kpj = reader.read(kpjData);
      if(!kpj) {
        this.callbacks.reportMessage(CopierMessages.Error_ProjectFileCouldNotBeRead({filename}));
        return null;
      }

      if(kpj.KeymanDeveloperProject?.Options?.Version && kpj.KeymanDeveloperProject.Options.Version != "1.0" && kpj.KeymanDeveloperProject.Options.Version != "2.0") {
        this.callbacks.reportMessage(CopierMessages.Error_UnsupportedProjectVersion({filename, version: kpj.KeymanDeveloperProject.Options.Version}));
        return null;
      }
      reader.validate(kpj);
    } catch(e) {
      this.callbacks.reportMessage(CopierMessages.Error_InvalidProjectFile({filename, message: (e ?? '').toString()}));
      return null;
    }
    const project = reader.transform(filename, kpj);
    return project;
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
    // TODO-COPY: see also kmc-genereate; this is a little poor because it is carrying state over from the
    // previous 'run' call, rather than figuring out the target path from the
    // artifacts. Probably should be looking at highest common folder, and then
    // failing if that path exists
    if(this.targetPathExists()) {
      this.callbacks.reportMessage(CopierMessages.Error_OutputPathAlreadyExists({outPath: this.options.outPath}));
      return false;
    }

    for(const key of Object.keys(artifacts)) {
      const a = artifacts[key];
      if(a.data == null) {
        // We can have non-existent files (e.g. build artifacts), for which we
        // have recorded filename changes for other linkages, but don't want to
        // write them out
        continue;
      }
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