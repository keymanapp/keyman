import { CompilerCallbacks, KeymanDeveloperProject, KPJFileReader } from '@keymanapp/common-types';
import { NodeCompilerCallbacks } from '../util/NodeCompilerCallbacks.js';
import { KeymanDeveloperProjectFile } from '../../../../../common/web/types/src/kpj/keyman-developer-project.js';
import { BuildCommandOptions } from '../commands/build.js';
import { buildKmnKeyboard } from './buildKmnKeyboard.js';
import * as path from 'path';
import { buildLdmlKeyboard } from './buildLdmlKeyboard.js';
import { buildModel } from './buildModel.js';
import { buildPackage } from './buildPackage.js';

export async function buildProject(infile: string, options: BuildCommandOptions): Promise<boolean> {
  let builder = new ProjectBuilder(infile, options);
  return builder.run();
}

class ProjectBuilder {
  callbacks: CompilerCallbacks = new NodeCompilerCallbacks();
  infile: string;
  options: BuildCommandOptions;
  project: KeymanDeveloperProject;

  constructor(infile: string, options: BuildCommandOptions) {
    this.infile = path.resolve(infile);
    this.options = options;
  }

  async run(): Promise<boolean> {
    if(this.options.outFile) {
      // TODO: callbacks.reportMessage
      console.error('--out-file should not be specified for project builds');
      return false;
    }

    this.project = this.loadProject();
    if(!this.project) {
      return false;
    }

    // Build all Keyman keyboards in the project
    if(!await this.buildProjectTargets(buildKmnKeyboard, '.kmn', '.kmx')) {
      return false;
    }

    // Build all LDML keyboards in the project
    if(!await this.buildProjectTargets(buildLdmlKeyboard, '.xml', '.kmx')) {
      return false;
    }

    // Build all models in the project
    if(!await this.buildProjectTargets(buildModel, '.model.ts', '.model.js')) {
      return false;
    }

    // Build all packages in the project
    if(!await this.buildProjectTargets(buildPackage, '.kps', '.kmp')) {
      return false;
    }

    return true;
  }

  loadProject(): KeymanDeveloperProject {
    // TODO: callbacks.reportMessage on exceptions
    // TODO: version 2.0 projects are folder-based and scan source/ folder for valid
    // files. .kpj need not exist, but if it does, is used just for options.
    const kpjData = this.callbacks.loadFile(null, this.infile);
    const reader = new KPJFileReader();
    const kpj = reader.read(kpjData);
    const schema = this.callbacks.loadKpjJsonSchema();
    try {
      reader.validate(kpj, schema);
    } catch(e) {
      // TODO: callbacks.reportMessage
      console.error(e);
      return null;
    }
    const project = reader.transform(kpj);
    return project;
  }

  async buildProjectTargets(
    buildTarget: (infile: string, options: BuildCommandOptions) => Promise<boolean>,
    fileType: '.xml'|'.kmn'|'.model.ts'|'.kps',
    outputFileType: '.kmx'|'.model.js'|'.kmp'
  ): Promise<boolean> {
    let result = true;
    for(let file of this.project.files) {
      if(file.fileType.toLowerCase() == fileType) {
        result = await this.buildTarget(file, buildTarget, fileType, outputFileType) && result;
      }
    }
    return result;
  }

  async buildTarget(
    file: KeymanDeveloperProjectFile,
    buildTarget: (infile: string, options: BuildCommandOptions) => Promise<boolean>,
    fileType: '.xml'|'.kmn'|'.model.ts'|'.kps',
    outputFileType: '.kmx'|'.model.js'|'.kmp'
  ): Promise<boolean> {
    const options = {...this.options};
    options.outFile = this.resolveOutputFilePath(file, fileType, outputFileType);
    const infile = this.resolveInputFilePath(file);
    // TODO: callbacks.reportMessage, improve logging and make consistent
    console.log(`Building ${infile}\n  Output ${options.outFile}`);
    this.callbacks.forceDirectories(path.dirname(options.outFile));
    let result = await buildTarget(infile, options);
    if(result) {
      console.log(`${path.basename(infile )} built successfully.`);
    } else {
      console.log(`${path.basename(infile)} failed to build.`);
    }
    return result;
  }

  resolveInputFilePath(file: KeymanDeveloperProjectFile): string {
    let p = path.dirname(this.infile);
    return path.normalize(path.join(p, file.filePath));
  }

  resolveOutputFilePath(file: KeymanDeveloperProjectFile, sourceExt: string, targetExt: string): string {
    // Matches Delphi TProject.GetTargetFileName
    let p = this.project.options.buildPath || '$SOURCEPATH';

    // Replace placeholders in the target path
    // TODO: do we need to support $VERSION?
    p = p.replace('$SOURCEPATH', path.dirname(this.resolveInputFilePath(file)));
    p = p.replace('$PROJECTPATH', path.dirname(this.infile));
    let f = file.filename.replace(new RegExp(`\\${sourceExt}$`, 'i'), targetExt);
    return path.normalize(path.join(p, f));
  }
}