/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Copy a keyboard or lexical model project
 */

import { CloudUrls, GitHubUrls, CompilerCallbacks, CompilerLogLevel, KeymanCompiler, KeymanCompilerArtifact, KeymanCompilerArtifacts, KeymanCompilerResult, KeymanDeveloperProject, KeymanDeveloperProjectOptions, KPJFileReader, KPJFileWriter, KpsFileReader, KpsFileWriter } from "@keymanapp/developer-utils";
import { KeymanFileTypes } from "@keymanapp/common-types";

import { CopierMessages } from "./copier-messages.js";
import { CopierAsyncCallbacks } from "./copier-async-callbacks.js";
import { GitHubRef, KeymanCloudSource } from "./cloud.js";

type CopierFunction = (
  project: KeymanDeveloperProject, filename: string, outputPath: string, source: string, result: CopierResult
) => Promise<boolean>;

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
  asyncCallbacks: CopierAsyncCallbacks;

  sourceId: string;
  outputId: string;
  outPath: string;

  githubRef: GitHubRef;
  cloudSource: KeymanCloudSource;
  isLocalOrigin(): boolean {
    return this.githubRef == undefined;
  }
  relocateExternalFiles: boolean = false; // TODO-COPY: support

  public async init(callbacks: CompilerCallbacks, options: CopierOptions): Promise<boolean> {
    if(!callbacks || !options) {
      return false;
    }
    this.callbacks = callbacks;
    this.options = options;
    this.cloudSource = new KeymanCloudSource(this.callbacks);
    return true;
  }

  /**
   * Copy a Keyman project. Returns an object containing binary
   * artifacts on success. The files are passed in by name, and the compiler
   * will use callbacks as passed to the {@link KeymanProjectCopier.init}
   * function to read any input files by disk.
   * @param   source  Source file or folder to copy. Can be a local file or folder, https://github.com/.../repo[/path], or cloud:id
   * @returns         Binary artifacts on success, null on failure.
   */
  public async run(source: string): Promise<CopierResult> {

    if(!this.verifyOutputPath()) {
      return null;
    }

    let projectSource: string;
    const projectSourceOrRef = await this.getSourceProject(source);
    if(!projectSourceOrRef) {
      // error will have been raised in getSourceProject
      return null;
    }

    if(typeof projectSourceOrRef == 'string') {
      projectSource = projectSourceOrRef;
    } else {
      projectSource = projectSourceOrRef.path;
      this.githubRef = projectSourceOrRef;
    }

    this.asyncCallbacks = new CopierAsyncCallbacks(this.callbacks, this.githubRef);

    // TODO-COPY: validate outputId is valid for the project type?
    this.outputId = this.callbacks.path.basename(this.outPath);
    this.sourceId = this.callbacks.path.basename(projectSource, KeymanFileTypes.Source.Project);

    // copy project file
    const project = await this.loadProjectFromFile(projectSource);
    if(!project) {
      // loadProjectFromFile already reported errors
      return null;
    }

    const result: CopierResult = { artifacts: {
      'kmc-copy:outputPath': {
        data: null,
        filename: this.outPath
      }
    } };
    const success = await this.copyProjectFile(project, projectSource, this.outPath, projectSource, result);

    return success ? result : null;
  }

  /**
   * Resolve the source project file to either a local filesystem file,
   * or a reference on GitHub
   * @param source
   * @returns  path to .kpj (either local or remote)
   */
  private async getSourceProject(source: string): Promise<string | GitHubRef> {
    if(source.match(GitHubUrls.GITHUB_URI_OPTIONAL_PROTOCOL) || source.match(GitHubUrls.GITHUB_RAW_URI)) {
      // `[https://]github.com/owner/repo/[tree|blob|raw]/[refs/...]/branch/path/to/kpj`, referencing a .kpj file
      return await this.getGitHubSourceProject(source);
    } else if(source.match(CloudUrls.CLOUD_URI) || source.match(CloudUrls.KEYMANCOM_CLOUD_URI)) {
      // `cloud:id`, referencing a Keyman Cloud keyboard
      return await this.getCloudSourceProject(source);
    } else if(this.callbacks.fs.existsSync(source) && source.endsWith(KeymanFileTypes.Source.Project) && !this.callbacks.isDirectory(source)) {
      // referencing a local filesystem project file
      return this.getLocalFileProject(source)
    } else {
      // Referencing a folder, or a missing local file
      return this.getLocalFolderProject(source);
    }
  }

  /**
   * Resolve source path to the contained project file; the project
   * file must have the same basename as the folder in this case
   * @param source
   * @returns
   */
  private getLocalFolderProject(source: string): string {
    source = this.normalizePath(source);
    const projectSource = this.normalizePath(this.callbacks.path.join(source, this.callbacks.path.basename(source) + KeymanFileTypes.Source.Project));
    if(!this.callbacks.fs.existsSync(projectSource)) {
      this.callbacks.reportMessage(CopierMessages.Error_CannotFindInputProject({project: source}));
      return null;
    }
    return projectSource;
  }

  /**
   * Resolve source path to the input .kpj filename, folder name
   * is not relevant when .kpj filename is passed in
   * @param source
   * @returns
   */
  private getLocalFileProject(source: string): string {
    return this.normalizePath(source);
  }

  /**
   * Resolve path to GitHub source, which must be in the following format:
   *   `[https://]github.com/owner/repo/branch/path/to/kpj`
   * The path must be fully qualified, referencing the .kpj file; it
   * cannot just be the folder where the .kpj is found
   * @param source
   * @returns a promise: GitHub reference to the source for the keyboard, or null on failure
   */
  private async getGitHubSourceProject(source: string): Promise<GitHubRef> {
    const parts: GitHubUrls.GitHubRegexMatchArray =
      GitHubUrls.GITHUB_URI_OPTIONAL_PROTOCOL.exec(source) ??
      GitHubUrls.GITHUB_RAW_URI.exec(source);
    if(!parts) {
      throw new Error('Expected GITHUB_URI_OPTIONAL_PROTOCOL or GITHUB_RAW_URI to match');
    }

    const ref: GitHubRef = new GitHubRef(parts);

    if(!ref.branch) {
      ref.branch = await this.cloudSource.getDefaultBranchFromGitHub(ref);
      if(!ref.branch) {
        this.callbacks.reportMessage(CopierMessages.Error_CouldNotFindDefaultBranchOnGitHub({ref: ref.toString()}));
        return null;
      }
    }
    if(!ref.path) {
      ref.path = '/';
    }
    if(!ref.path.startsWith('/')) {
      ref.path = '/' + ref.path;
    }

    if(ref.path != '/') {
      if(!ref.path.endsWith('.kpj')) {
        // Assumption, project filename matches folder name
        if(ref.path.endsWith('/')) {
          ref.path = ref.path.substring(0, ref.path.length-1);
        }
        ref.path = ref.path + '/' + this.callbacks.path.basename(ref.path) + '.kpj';
      }
    }

    return ref;
  }

  /**
   * Resolve path to Keyman Cloud source (which is on GitHub), which must be in
   * the following format:
   *   `cloud:keyboard_id`, or
   *   `cloud:model_id`, or
   *   `https://keyman.com/keyboards/keyboard_id`
   * The `keyboard_id` parameter should be a valid id (a-z0-9_), as found at
   * https://keyman.com/keyboards; alternatively if it is a model_id, it should
   * have the format author.bcp47.uniq
   * @param source
   * @returns a promise: GitHub reference to the source for the keyboard, or null on failure
   */
  private async getCloudSourceProject(source: string): Promise<GitHubRef> {
    const parts = CloudUrls.CLOUD_URI.exec(source) ?? CloudUrls.KEYMANCOM_CLOUD_URI.exec(source);
    if(!parts) {
      throw new Error('Expected CLOUD_URI or KEYMANCOM_CLOUD_URI to match');
    }

    const id: string = parts.groups.id;
    const isModel = /^[^.]+\.[^.]+\.[^.]+$/.test(id);
    const remote = await this.cloudSource.getSourceFromKeymanCloud(id, isModel);
    if(!remote) {
      return null;
    }

    // append project filename .kpj
    remote.path += '/' + this.callbacks.path.basename(remote.path) + KeymanFileTypes.Source.Project;
    return remote;
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
    [index in KeymanFileTypes.Source]: CopierFunction
  } = {
    ".kpj": this.copyProjectFile.bind(this),
    ".kmn": this.copyKmnSourceFile.bind(this),
    ".kps": this.copyKpsSourceFile.bind(this),
    ".kvks": this.copySourceFile.bind(this),
    ".keyman-touch-layout": this.copySourceFile.bind(this),
    ".xml": this.copySourceFile.bind(this),
    ".model.ts": this.copyModelTsSourceFile.bind(this),
  };

  private async copyFolder(inputPath: string, outputPath: string, ignorePatterns: (string | RegExp)[], result: CopierResult) {
    // TODO-COPY: watch out for file collisions when copying project files -- after renames
    const files = await this.asyncCallbacks.fsAsync.readdir(inputPath);
    for(const {filename,type} of files) {
      const fullPath = this.normalizePath(this.callbacks.path.join(inputPath, filename));
      if(ignorePatterns.find( pattern => (typeof pattern == 'string' && pattern == fullPath) || (fullPath.match(pattern)) )) {
        continue;
      }
      if(type == 'dir') {
        await this.copyFolder(fullPath, this.callbacks.path.join(outputPath, filename), ignorePatterns, result);
      } else if(!result.artifacts[fullPath]) {
        result.artifacts[fullPath] = {
          data: await this.asyncCallbacks.fsAsync.readFile(fullPath),
          filename: this.generateNewFilename(fullPath, outputPath)
        }
      }
    }
  }

  private resolveFilename(base: string, filename: string) {
    if(this.isLocalOrigin()) {
      return this.callbacks.resolveFilename(base, filename);
    }

    // GitHub resolveFilename
    if(filename.startsWith('/')) {
      return filename;
    }

    let result = this.normalizePath(this.callbacks.path.normalize(this.callbacks.path.join(this.callbacks.path.dirname(base), filename)));
    if(!result.startsWith('/')) result = '/' + result;
    return result;
  }

  private async copyProjectFile(project: KeymanDeveloperProject, filename: string, outputPath: string, source: string, result: CopierResult): Promise<boolean> {
    for(const file of project.files) {
      const normalizedFilePath = this.normalizePath(file.filePath);
      const subOutputPath = this.normalizePath(this.calculateNewFilePath(normalizedFilePath));
      const subFilename = this.normalizePath(this.resolveFilename(project.projectFilename, normalizedFilePath));
      const copier = this.copiers[<KeymanFileTypes.Source> file.fileType] ?? this.copyGenericFile.bind(this);
      // Ignore errors because we will continue to do a best effort
      await copier(project, subFilename, subOutputPath, source, result);
    }

    if(project.options.version == "2.0") {
      // For version 2.0 projects, we also copy every file in the folder
      // except for project.buildpath and .kpj.user
      await this.copyFolder(project.projectPath, outputPath, [/\.kpj\.user$/, project.resolveBuildPath()], result);
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

  private async copyGenericFile(_project: KeymanDeveloperProject, filename: string, outputPath: string, _source: string, result: CopierResult): Promise<boolean> {
    if(result.artifacts[filename]) {
      return true;
    }

    const data = await this.asyncCallbacks.fsAsync.readFile(filename);
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

  private async copySourceFile(project: KeymanDeveloperProject, filename: string, _outputPath: string, source: string, result: CopierResult): Promise<boolean> {
    return await this.copyGenericFile(project, filename, this.callbacks.path.join(this.outPath, 'source'), source, result);
  }

  private async copyModelTsSourceFile(project: KeymanDeveloperProject, filename: string, outputPath: string, source: string, result: CopierResult): Promise<boolean> {
    if(!await this.copySourceFile(project, filename, outputPath, source, result)) {
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
      const newRelativePath = await this.copySubFileAndGetRelativePath(project, filename, wordlistFilename, outputPath, source, result);

      out += wordlistString.substring(index, wordlist.index) + wordlist[1] + newRelativePath + wordlist[3];
      index = wordlist.index + wordlist[0].length;
    }
    out += wordlistString.substring(index);
    const outLines = lines.substring(0, sources.index ?? 0) + sources[1] + out + sources[3] + lines.substring((sources.index ?? 0) + sources[0].length);

    result.artifacts[filename].data = new TextEncoder().encode(outLines);

    return true;
  }

  private async copyKmnSourceFile(project: KeymanDeveloperProject, filename: string, outputPath: string, source: string, result: CopierResult): Promise<boolean> {
    if(!await this.copySourceFile(project, filename, outputPath, source, result)) {
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
          const newRelativePath = await this.copySubFileAndGetRelativePath(project, filename, m[2], outputPath, source, result);
          lines[i] = line.replace(replacementRegex, `$1${newRelativePath}$4`);
        }
      }
    }

    // rewrite fixups inside the source file
    result.artifacts[filename].data = new TextEncoder().encode(lines.join('\n'));

    return true;
  }

  private async copyKpsSourceFile(project: KeymanDeveloperProject, filename: string, outputPath: string, source: string, result: CopierResult): Promise<boolean> {
    if(!await this.copySourceFile(project, filename, outputPath, source, result)) {
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
        const newRelativePath = await this.copySubFileAndGetRelativePath(project, filename, file.Name, outputPath, source, result);

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

  private async copySubFileAndGetRelativePath(project: KeymanDeveloperProject, parentFilename: string, originalSubfilename: string, outputPath: string, source: string, result: CopierResult): Promise<string> {
    originalSubfilename = this.normalizePath(originalSubfilename);
    const subFilename = this.normalizePath(this.resolveFilename(parentFilename, originalSubfilename));
    const subFilenameAbsolute = this.normalizePath(this.resolveFilename(project.projectPath, subFilename));
    const subFilenameRelative = this.normalizePath(this.callbacks.path.relative(project.projectPath, subFilenameAbsolute));

    let subOutputPath: string;
    if(this.callbacks.path.isAbsolute(subFilenameRelative) || subFilenameRelative.startsWith('..')) {
      if(this.isLocalOrigin() && !this.relocateExternalFiles) {
        // Reference outside the project structure, do not attempt to normalize or copy,
        // but we do need to update references for the new output path
        return this.normalizePath(this.callbacks.path.relative(result.artifacts[parentFilename].filename, subFilenameAbsolute));
      } else {
        // Reference outside the project structure, we will copy into a subfolder of the project;
        // should be relative to root of remote repo, e.g. release/k/khmer_angkor references release/shared/fonts/...,
        // so should go external/release/shared/fonts/...
        subOutputPath = this.normalizePath(this.callbacks.path.join(this.options.outPath, 'external', this.callbacks.path.dirname(subFilenameAbsolute)));
      }
    } else {
      subOutputPath = this.normalizePath(this.callbacks.path.join(outputPath, this.callbacks.path.dirname(originalSubfilename)));
    }

    await this.copyGenericFile(project, subFilename, subOutputPath, source, result);

    // Even if the subfile is missing, we'll still continue the overall copy
    return this.normalizePath(this.callbacks.path.relative(this.callbacks.path.dirname(result.artifacts[parentFilename].filename), result.artifacts[subFilename].filename));
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

  private calculateNewFilePath(oldFilePath: string) {
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

  private async loadProjectFromFile(filename: string): Promise<KeymanDeveloperProject> {
    const kpjData = await this.asyncCallbacks.fsAsync.readFile(filename);
    const reader = new KPJFileReader(this.asyncCallbacks);
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
    const project = await reader.transform(filename, kpj);

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
  /* c8 ignore start */
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
  /* c8 ignore stop */

  /** @internal */
  public unitTestEndPoints = {
    getGithubSourceProject: this.getGitHubSourceProject.bind(this),
    getCloudSourceProject: this.getCloudSourceProject.bind(this)
  };

}
