/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Copy a keyboard or lexical model project
 */

import { CompilerCallbacks, CompilerLogLevel, KeymanCompiler, KeymanCompilerArtifact, KeymanCompilerArtifacts, KeymanCompilerResult, KeymanDeveloperProject, KeymanDeveloperProjectOptions, KPJFileReader, KPJFileWriter, KpsFileReader, KpsFileWriter } from "@keymanapp/developer-utils";
import { KeymanFileTypes } from "@keymanapp/common-types";

import { CopierMessages } from "./copier-messages.js";


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
  /*
   * TODO-COPY rename: rename existing files rather than copy

  rename: boolean;
  */
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
   * The target path for the copy, equal to the normalized output path.
   * This folder must not exist before `write()`. This pattern is
   * used to avoid name collisions with other artifacts
   */
  ['kmc-copy:outputPath']: KeymanCompilerArtifact;
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
  outPath: string;

  public async init(callbacks: CompilerCallbacks, options: CopierOptions): Promise<boolean> {
    this.callbacks = callbacks;
    this.options = options;
    return true;
  }

  /**
   * Copy a Keyman project. Returns an object containing binary
   * artifacts on success. The files are passed in by name, and the compiler
   * will use callbacks as passed to the {@link KeymanProjectCopier.init}
   * function to read any input files by disk.
   * @param   source  Source file or folder to copy. Can be a local file or folder, github:repo, or cloud:path/id
   * @returns         Binary artifacts on success, null on failure.
   */
  public async run(source: string): Promise<CopierResult> {

    if(!this.verifyOutputPath()) {
      return null;
    }

    const { temp, projectSource } = await this.getSourceProject(source);
    if(!projectSource) {
      // error will have been raised in getSourceProject
      return null;
    }

    // TODO-COPY: validate outputId is valid for the project type?
    this.outputId = this.callbacks.path.basename(this.outPath);
    this.sourceId = this.callbacks.path.basename(projectSource, '.kpj');

    // copy project file
    const project = this.loadProjectFromFile(projectSource);
    if(!project) {
      // loadProjectFromFile already reported errors
      // TODO-COPY: cleanup temp
      return null;
    }

    const result: CopierResult = { artifacts: {
      'kmc-copy:outputPath': {
        data: null,
        filename: this.outPath
      }
    } };
    const success = this.copyProjectFile(project, projectSource, this.outPath, projectSource, result);

    if(temp) {
      // TODO-COPY: this.cleanupTemp(temp);
    }

    return success ? result : null;
  }

  /**
   *
   * @param source
   * @returns   temp - path to temporary folder to be removed on success or failure, projectSource - path to kpj
   */
  private async getSourceProject(source: string): Promise<{temp: string, projectSource: string}> {
    let temp: string = null, projectSource: string = null;

    if(source.startsWith('github:')) {
      // TODO-COPY: temp = downloadFromGithub(source);
      projectSource = temp; // TODO-COPY: +.kpj
    } else if(source.startsWith('cloud:')) {
      // TODO-COPY: temp = downloadFromKeymanCloud(source);
      projectSource = temp; // TODO-COPY: +.kpj
    } else if(this.callbacks.fs.existsSync(source) && source.endsWith('.kpj')) {
      // already the project file
      projectSource = this.normalizePath(source);
    } else {
      source = this.normalizePath(source);
      projectSource = this.normalizePath(this.callbacks.path.join(source, this.callbacks.path.basename(source) + '.kpj'));
      if(!this.callbacks.fs.existsSync(projectSource)) {
        this.callbacks.reportMessage(CopierMessages.Error_CannotFindInputProject({project: source}));
        projectSource = null;
      }
    }
    return { temp, projectSource };
  }

  private normalizePath(path: string): string {
    return path.replaceAll(/\\/g, '/');
  }

  private verifyOutputPath() {
    this.outPath = this.normalizePath(this.options.outPath);

    if(this.outPath.endsWith('/')) {
      this.outPath = this.outPath.substring(0, this.outPath.length - 1);
    }

    if(this.callbacks.fs.existsSync(this.outPath)) {
      this.callbacks.reportMessage(CopierMessages.Error_OutputPathAlreadyExists({outPath: this.outPath}));
      return false;
    }

    return true;
  }

  private copiers: {
    [index in KeymanFileTypes.Source]: (
      project: KeymanDeveloperProject, filename: string, outputPath: string, source: string, result: CopierResult
    ) => boolean
  } = {
    ".kpj": this.copyProjectFile.bind(this),
    ".kmn": this.copyKmnSourceFile.bind(this),
    ".kps": this.copyKpsSourceFile.bind(this),
    ".kvks": this.copySourceFile.bind(this),
    ".keyman-touch-layout": this.copySourceFile.bind(this),
    ".xml": this.copySourceFile.bind(this),
    ".model.ts": this.copyModelTsSourceFile.bind(this),
  };

  private copyFolder(inputPath: string, outputPath: string, ignores: (string | RegExp)[], result: CopierResult) {
    // TODO-COPY: watch out for file collisions when copying project files -- after renames
    const files = this.callbacks.fs.readdirSync(inputPath);
    for(const file of files) {
      const fullPath = this.normalizePath(this.callbacks.path.join(inputPath, file));
      const isIgnored = ignores.find(
        i => (typeof i == 'string' && i == fullPath) || (fullPath.match(i))
      );
      if(isIgnored) {
        continue;
      }
      if(this.callbacks.isDirectory(fullPath)) {
        this.copyFolder(fullPath, this.callbacks.path.join(outputPath, file), ignores, result);
      } else if(!result.artifacts[fullPath]) {
        result.artifacts[fullPath] = {
          data: this.callbacks.loadFile(fullPath),
          filename: this.generateNewFilename(fullPath, outputPath)
        }
      }
    }
  }

  private copyProjectFile(project: KeymanDeveloperProject, filename: string, outputPath: string, source: string, result: CopierResult): boolean {
    for(const file of project.files) {
      const copier = this.copiers[<KeymanFileTypes.Source> file.fileType] ?? this.copyGenericFile.bind(this);

      const subOutputPath = this.normalizePath(this.calculateNewFilePath(project.projectPath, file.filePath));
      const subFilename = this.normalizePath(project.resolveInputFilePath(file));

      // Ignore errors because we will continue to do a best effort
      copier(project, subFilename, subOutputPath, source, result);
    }

    if(project.options.version == "2.0") {
      // For version 2.0 projects, we also copy every file in the folder
      // except for project.buildpath and .kpj.user
      this.copyFolder(project.projectPath, outputPath, [/\.kpj\.user/, project.resolveBuildPath()], result);
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

  private copyGenericFile(project: KeymanDeveloperProject, filename: string, outputPath: string, source: string, result: CopierResult): boolean {
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

  private copySourceFile(project: KeymanDeveloperProject, filename: string, outputPath: string, source: string, result: CopierResult): boolean {
    return this.copyGenericFile(project, filename, this.callbacks.path.join(this.outPath, 'source'), source, result);
  }

  private copyModelTsSourceFile(project: KeymanDeveloperProject, filename: string, outputPath: string, source: string, result: CopierResult): boolean {
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

  private copyKmnSourceFile(project: KeymanDeveloperProject, filename: string, outputPath: string, source: string, result: CopierResult): boolean {
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

  private copyKpsSourceFile(project: KeymanDeveloperProject, filename: string, outputPath: string, source: string, result: CopierResult): boolean {
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
        // const subFilename = this.callbacks.resolveFilename(filename, file.Name);
        const newRelativePath = this.copySubFileAndGetRelativePath(project, filename, file.Name, outputPath, source, result);

        const remap = (ref?: string) => ref == file.Name ? newRelativePath : ref;

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

  private copySubFileAndGetRelativePath(project: KeymanDeveloperProject, parentFilename: string, originalSubfilename: string, outputPath: string, source: string, result: CopierResult): string {
    originalSubfilename = this.normalizePath(originalSubfilename);
    const subFilename = this.normalizePath(this.callbacks.resolveFilename(parentFilename, originalSubfilename));
    const subFilenameAbsolute = this.normalizePath(this.callbacks.resolveFilename(project.projectPath, subFilename));
    const subFilenameRelative = this.normalizePath(this.callbacks.path.relative(project.projectPath, subFilenameAbsolute));

    if(this.callbacks.path.isAbsolute(subFilenameRelative) || subFilenameRelative.startsWith('..')) {
      // Reference outside the project structure, do not attempt to normalize or copy,
      // but we do need to update references for the new output path
      return this.normalizePath(this.callbacks.path.relative(result.artifacts[parentFilename].filename, subFilenameAbsolute));
    } else {
      const subOutputPath = this.normalizePath(this.callbacks.path.join(outputPath, this.callbacks.path.dirname(originalSubfilename)));
      this.copyGenericFile(project, subFilename, subOutputPath, source, result);

      // Even if the subfile is missing, we'll still continue the overall copy
      return this.normalizePath(this.callbacks.path.relative(this.callbacks.path.dirname(result.artifacts[parentFilename].filename), result.artifacts[subFilename].filename));
    }
  }

  private splitFilename(filename: string): {ext: string, path: string, base: string} {
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
  private generateNewFilename(filename: string, outputPath: string): string {
    const {ext, base} = this.splitFilename(filename);
    const newFilename = base == this.sourceId
      ? this.callbacks.path.join(outputPath, this.outputId + ext)
      : this.callbacks.path.join(outputPath, base + ext);

    return this.normalizePath(newFilename);
  }

  private calculateNewFilePath(oldProjectFilename: string, oldFilePath: string) {
    // Do not change filename here, just its path

    const {ext, base} = this.splitFilename(oldFilePath);
    const filename = base + ext;

    const sourceType = KeymanFileTypes.sourceTypeFromFilename(oldFilePath);

    if(sourceType === null) {
      // Standard metadata files -- root of project
      if(filename === KeymanFileTypes.HISTORY_MD || filename === KeymanFileTypes.README_MD || filename === KeymanFileTypes.LICENSE_MD) {
        return this.outPath;
      }
      // Other files referenced in project, maintain original path
      return this.callbacks.path.join(this.outPath, this.callbacks.path.dirname(oldFilePath));
    }

    if(sourceType == KeymanFileTypes.Source.Project) {
      // Project file -- root of project
      return this.outPath;
    }

    // source type is valid
    return this.callbacks.path.join(this.outPath, 'source');
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

  /**
   * Write artifacts from a successful compile to disk, via callbacks methods.
   * The artifacts written will include all files from the project, across
   * multiple folders. Folders will be created as needed
   *
   * @param artifacts - object containing artifact binary data to write out
   * @returns true on success
   */
  public async write(artifacts: CopierArtifacts): Promise<boolean> {
    if(this.callbacks.fs.existsSync(artifacts["kmc-copy:outputPath"].filename)) {
      this.callbacks.reportMessage(CopierMessages.Error_OutputPathAlreadyExists({outPath: this.outPath}));
      return false;
    }

    const pathsCreated: string[] = [];

    if(this.options.dryRun) {
      this.callbacks.reportMessage(CopierMessages.Info_DryRun({outPath: this.outPath}));
    }

    for(const key of Object.keys(artifacts)) {
      const a = artifacts[key];
      if(a.data == null) {
        // We can have non-existent files (e.g. build artifacts), for which we
        // have recorded filename changes for other linkages, but don't want to
        // write them out
        // also 'copier base path' has null data
        continue;
      }
      const path = this.callbacks.path.dirname(a.filename);
      try {
        if(this.options.dryRun) {
          if(!pathsCreated.includes(path)) {
            this.callbacks.reportMessage(CopierMessages.Info_DryRunCreatingFolder({path}));
            pathsCreated.push(path);
          }
        } else {
          this.callbacks.fs.mkdirSync(path, {recursive: true});
        }
      /* c8 ignore next 4 */
      } catch(e) {
        this.callbacks.reportMessage(CopierMessages.Error_CannotCreateFolder({folderName:path, e}));
        return false;
      }
      try {
        if(this.options.dryRun) {
          this.callbacks.reportMessage(CopierMessages.Info_DryRunWritingFile({filename:a.filename}));
        } else {
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