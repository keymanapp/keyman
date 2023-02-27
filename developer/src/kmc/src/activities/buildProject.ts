import { CompilerCallbacks, KeymanDeveloperProject, KPJFileReader } from '@keymanapp/common-types';
import { NodeCompilerCallbacks } from '../util/NodeCompilerCallbacks.js';
import { KeymanDeveloperProjectFile } from '../../../../../common/web/types/src/kpj/keyman-developer-project.js';
import { BuildCommandOptions } from '../commands/build.js';
import { buildKmnKeyboard } from './buildKmnKeyboard.js';
import * as path from 'path';
import * as fs from 'fs';
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

    // TODO: generate .keyboard_info from .kps + etc (and support merge of
    // $PROJECTPATH/.keyboard_info for version 1.0 projects)

    return true;
  }

  loadProject(): KeymanDeveloperProject {
    // TODO: callbacks.reportMessage on exceptions
    // TODO: version 2.0 projects are folder-based and scan source/ folder for valid
    // files. .kpj need not exist, but if it does, is used just for options.

    this.infile = path.resolve(this.infile.replace(/\\/g, '/'));

    if(fs.statSync(this.infile).isDirectory()) {
      // This is a project folder, look for folder-name.kpj
      this.infile = path.join(this.infile, path.basename(this.infile) + '.kpj');
    }

    const project = fs.existsSync(this.infile) ?
      this.loadProjectFromFile() :
      this.loadDefaultProjectFromFolder();

    return project;
  }

  loadDefaultProjectFromFolder() {
    // The folder does not contain a .kpj, so construct a default 2.0 .kpj
    const project = new KeymanDeveloperProject('2.0');
    project.populateFiles(this.infile);
    return project;
  }

  loadProjectFromFile(): KeymanDeveloperProject {
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
    const project = reader.transform(this.infile, kpj);
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
    options.outFile = this.project.resolveOutputFilePath(this.infile, file, fileType, outputFileType);
    const infile = this.project.resolveInputFilePath(this.infile, file);
    // TODO: callbacks.reportMessage, improve logging and make consistent
    console.log(`Building ${infile}\n  Output ${options.outFile}`);

    fs.mkdirSync(path.dirname(options.outFile), {recursive:true});

    let result = await buildTarget(infile, options);
    if(result) {
      console.log(`${path.basename(infile )} built successfully.`);
    } else {
      console.log(`${path.basename(infile)} failed to build.`);
    }
    return result;
  }

}