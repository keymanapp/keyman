/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Run regression tests on a keyboard project
 */

import { CompilerCallbacks, KeymanCompiler, KeymanCompilerArtifact,
  KeymanCompilerArtifacts, KeymanCompilerResult, KeymanDeveloperProject,
  CompilerBaseOptions, RegressionTestSourceFileReader, ProjectLoader,
  RegressionTestSourceFile } from "@keymanapp/developer-utils";
import { KeymanFileTypes, USVirtualKeyCodes } from "@keymanapp/common-types";
import { KeymanCoreModule, km_core_factory } from "./core-wrapper.js";
import { km_core_keyboard, km_core_state } from "./import/core/keymancore.js";

// import { TesterMessages } from "./tester-messages.js";

export const
KM_CORE_MODIFIER_NONE        = 0,
KM_CORE_MODIFIER_LCTRL       = 1 << 0,
KM_CORE_MODIFIER_RCTRL       = 1 << 1,
KM_CORE_MODIFIER_LALT        = 1 << 2,
KM_CORE_MODIFIER_RALT        = 1 << 3,
KM_CORE_MODIFIER_SHIFT       = 1 << 4,
KM_CORE_MODIFIER_CTRL        = 1 << 5,
KM_CORE_MODIFIER_ALT         = 1 << 6,
/*
  KM_CORE_MODIFIER_META        = 1 << 7,    // Either Meta-key flag (tentative).  Not usable by keyboards currently
                                           // Used internally (currently, only by KMW) to ensure Meta-key
                                           // shortcuts safely bypass rules
                                           // Meta key = Command key on macOS, Windows key on Windows
*/
KM_CORE_MODIFIER_CAPS        = 1 << 8,
KM_CORE_MODIFIER_NOCAPS      = 1 << 9;


/**
 * @public
 * Options for the Keyman Developer project copier
 */
export interface TesterOptions extends CompilerBaseOptions {
  /**
   * output path where test results will be created
   */
  outPath?: string;
  /**
   * filter for test names
   */
  testNames?: string; //TODO-KMC-TEST
};

/**
 * @public
 * Internal in-memory build artifacts from a test run
 */
export interface TesterArtifacts extends KeymanCompilerArtifacts {
  /**
   * The target path for the copy, equal to the normalized output path.
   * This folder must not exist before `write()`. This pattern is
   * used to avoid name collisions with other artifacts
   */
  ['kmc-test:outputPath']: KeymanCompilerArtifact;
  /**
   * Test result files to be written to disk
  */
  [name:string]: KeymanCompilerArtifact;
};

/**
 * @public
 * Result of a successful copy
 */
export interface TesterResult extends KeymanCompilerResult {
  /**
   * Internal in-memory build artifacts from a successful compilation. Caller
   * can write these to disk with {@link KeymanTester.write}
   */
  artifacts: TesterArtifacts;
};

export class KeymanTester implements KeymanCompiler {
  options: TesterOptions;
  callbacks: CompilerCallbacks;

  sourceId: string;
  outputId: string;
  outPath: string;

  keyboard: km_core_keyboard;
  state: km_core_state;
  keyboardFilename: string;
  id: string;
  km_core: KeymanCoreModule;

  public async init(callbacks: CompilerCallbacks, options: TesterOptions): Promise<boolean> {
    if(!callbacks || !options) {
      return false;
    }
    this.callbacks = callbacks;
    this.options = options;

    // Instanatiate our KM_Core
    this.km_core = await km_core_factory(this.callbacks);
    if(!this.km_core) {
      // TODO-KMC-TEST: errors
      console.error('km_core inst');
      return false;
    }
    return true;
  }

  /**
   * Tests a Keyman project. Returns an object containing binary
   * artifacts on success. The files are passed in by name, and the compiler
   * will use callbacks as passed to the {@link KeymanProjectCopier.init}
   * function to read any input files by disk.
   * @param   source  Source file or folder to copy. Can be a local file or folder, https://github.com/.../repo[/path], or cloud:id
   * @returns         Binary artifacts on success, null on failure.
   */
  public async run(source: string): Promise<TesterResult> {

    const projectSource = await this.getSourceProject(source);

    this.sourceId = this.callbacks.path.basename(projectSource, KeymanFileTypes.Source.Project);

    // copy project file
    const project = await ProjectLoader.loadProjectFromFile(projectSource, this.callbacks);
    if(!project) {
      // TODO-KMC-TEST loadProjectFromFile already reported errors
      console.error('failed to load project');
      return null;
    }

    const testPath = this.callbacks.path.join(project.projectPath, 'test');
    const testFiles = this.callbacks.fs.readdirSync(testPath);
    if(!testFiles) {
      console.error('failed to populate test files');
      return null;
    }

    const result: TesterResult = { artifacts: {
      'kmc-test:outputPath': {
        data: null,
        filename: this.outPath
      }
    } };

    // console.dir(testFiles);
    let success: boolean = true;
    // TODO-KMC-TEST: for now, we'll assume the test files are under test/
    // TODO-KMC-TEST: need to register the file extension

    // success = await this.testFile(project,
      // this.callbacks.path.join(project.projectPath, 'test', testFiles[0]));

    for (const file of testFiles) {
      success = await this.testFile(project,
        this.callbacks.path.join(project.projectPath, 'test', file)) || success;
    }

    if(this.keyboard) {
      this.km_core.keyboard_dispose(this.keyboard);
      this.keyboard = null;
    }
    if(this.state) {
      this.km_core.state_dispose(this.state);
      this.state = null;
    }

    return success ? result : null;
  }

  private async testFile(project: KeymanDeveloperProject, filename: string): Promise<boolean> {
    // load the test file
      // TODO-KMC-TEST
      // console.log(`Starting test ${filename}`);

    const content = this.callbacks.fs.readFileSync(filename);
    if(!content) {
      // TODO-KMC-TEST
      console.error('failed to read');
      return false;
    }

    if(RegressionTestSourceFileReader.isRegressionTestFile(content)) {
      return await this.runRegressionTestFile(project, filename, content);
    } // TODO-KMC-TEST else if isLdmlTestFile(content) ...
    return true;
  }

  private async runRegressionTestFile(project: KeymanDeveloperProject, filename: string,
    content: Uint8Array
  ): Promise<boolean> {
    const reader = new RegressionTestSourceFileReader();
    const regtest = reader.read(content);
    if(!regtest) {
      // TODO-KMC-TEST: errors
      console.error('Failed to read regtest');
      return false;
    }
    // assumes that build/keyboard exists
    if(!regtest.info.keyboard) {
      // TODO-KMC-TEST: errors
      console.error('Failed to read keyboard');
      return false;
    }
    if(regtest.info.beginmode != 'Unicode') {
      // TODO-KMC-TEST: errors
      console.error('Only unicode tests supported');
      return false;
    }
    const keyboardFilename = this.callbacks.path.basename(regtest.info.keyboard);

    if(keyboardFilename != this.keyboardFilename) {
      if(!this.loadKeyboard(project, keyboardFilename)) {
        return false;
      }
    }

    // Run the tests (TODO-KMC-TEST: deadkeys)

    this.km_core.state_context_set_if_needed(this.state, '');

    const result = this.runRegressionTest(this.km_core, this.state, regtest);

    if(result) {
      console.log(`   ✔ ${this.id} test ${this.callbacks.path.basename(filename, '.xml')}`);
    } else {
      console.log(`   ❌ ${this.id} test ${this.callbacks.path.basename(filename, '.xml')}`);
    }

    return result;
  }

  loadKeyboard(project: KeymanDeveloperProject, keyboardFilename: string) {
    if(this.keyboard) {
      this.km_core.keyboard_dispose(this.keyboard);
      this.keyboard = null;
    }
    if(this.state) {
      this.km_core.state_dispose(this.state);
      this.state = null;
    }
    this.id = keyboardFilename.replace(/\.kmx$/, '');
    const fullKeyboardPath = this.callbacks.path.join(project.resolveBuildPath(), keyboardFilename);
    if(!this.callbacks.fs.existsSync(fullKeyboardPath)) {
      // TODO-KMC-TEST: errors
      console.error('File not exist');
      return false;
    }

    const keyboardData = this.callbacks.fs.readFileSync(fullKeyboardPath);
    this.keyboard = this.km_core.keyboard_load_from_blob(this.id, keyboardData)?.object;
    if(!this.keyboard) {
      // TODO-KMC-TEST: errors
      console.error('!keyboard');
      return false;
    } //TODO-KMC-TEST

    this.state = this.km_core.state_create(this.keyboard, [])?.object;
    if(!this.state) {
      // TODO-KMC-TEST: errors
      console.error('!state');
      return false;
    }

    this.keyboardFilename = keyboardFilename;
    return true;
  }

  regTestShiftToCore(shiftState: RegressionTestSourceFile.RegTestShiftState[]) {
    let result: number = 0;
    for(const shift of shiftState) {
      switch(shift) {
        case RegressionTestSourceFile.RegTestShiftState.shift: result |= KM_CORE_MODIFIER_SHIFT; break;
        case RegressionTestSourceFile.RegTestShiftState.ctrl: result |= KM_CORE_MODIFIER_CTRL; break;
        case RegressionTestSourceFile.RegTestShiftState.rctrl: result |= KM_CORE_MODIFIER_RCTRL; break;
        case RegressionTestSourceFile.RegTestShiftState.alt: result |= KM_CORE_MODIFIER_ALT; break;
        case RegressionTestSourceFile.RegTestShiftState.altgr: result |= KM_CORE_MODIFIER_RALT; break;
        case RegressionTestSourceFile.RegTestShiftState.caps: result |= KM_CORE_MODIFIER_CAPS; break;
      }
    }
    return result;

  }
  runRegressionTest(km_core: KeymanCoreModule, state: km_core_state, regtest: RegressionTestSourceFile.RegressionTestSourceFile) {
    let textStore: string[] = [];
    // console.dir(regtest.events, {depth:10});
    for(const event of regtest.events) {
      // event.
      const vkey: number = (<any>USVirtualKeyCodes)[event.key.vkey]; //TODO-KMC-TEST <any>?
      const shift: number = this.regTestShiftToCore(event.key.shiftstate);
      // console.log(` [${shift} ${event.key.vkey}] -> ${event.postcontext.join('')}`);
      if(!vkey) {
        console.dir(regtest.events, {depth:10});
        console.error('vkey missing');
        return false; //TODO-KMC-TEST
      }
      if(km_core.process_event(state, vkey, shift, 1, 0) != 0) {
        console.dir(regtest.events, {depth:10});
        console.error('process_event failed');
        return false; //TODO-KMC-TEST
      }
      const actions = km_core.state_get_actions(state);
      if(!actions) {
        console.dir(regtest.events, {depth:10});
        console.error('state_get_actions failed');
        return false; // TODO-KMC-TEST
      }

      textStore.splice(textStore.length - actions.code_points_to_delete);
      textStore = textStore.concat(...[...actions.output]);
      // TODO-KMC-TEST use km_core.state_get_context to support deadkeys
      if(event.postcontext.join('') != textStore.join('')) {
        // console.dir(regtest.events, {depth:10});
        return false;
      }
    }

    // TODO-KMC-TEST need a specialized callback for pretty reporting of test results
    return true;
  }

  // TODO-KMC-TEST: A lot of these functions seem pretty generic and probably
  // belong in a developer-utils helper module

  /**
   * Resolve the source project file to either a local filesystem file,
   * or a reference on GitHub
   * @param source
   * @returns  path to .kpj (either local or remote)
   */
  private getSourceProject(source: string): string {
    if(this.callbacks.fs.existsSync(source) && source.endsWith(KeymanFileTypes.Source.Project) && !this.callbacks.isDirectory(source)) {
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
      // TODO-KMC-TEST this.callbacks.reportMessage(TesterMessages.Error_CannotFindInputProject({project: source}));
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

  private normalizePath(path: string): string {
    return path.replaceAll(/\\/g, '/');
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
  public async write(artifacts: TesterArtifacts): Promise<boolean> {
    if(this.callbacks.fs.existsSync(artifacts["kmc-test:outputPath"].filename)) {
      // TODO-KMC-TEST: this.callbacks.reportMessage(TesterMessages.Error_OutputPathAlreadyExists({outPath: this.outPath}));
      return false;
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
        this.callbacks.fs.mkdirSync(path, {recursive: true});
      /* c8 ignore next 4 */
      } catch(e) {
        // TODO-KMC-TEST: this.callbacks.reportMessage(TesterMessages.Error_CannotCreateFolder({folderName:path, e}));
        return false;
      }
      try {
        this.callbacks.fs.writeFileSync(a.filename, a.data);
      } catch(e) {
        // TODO-KMC-TEST: this.callbacks.reportMessage(TesterMessages.Error_CannotWriteOutputFile({filename:a.filename, e}));
        return false;
      }
    }
    return true;
  }
  /* c8 ignore stop */

  /** @internal */
  public unitTestEndPoints = {
  };

}
