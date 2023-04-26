import * as path from 'path';
import * as fs from 'fs';
import { CompilerCallbacks, KeymanDeveloperProject, KPJFileReader } from '@keymanapp/common-types';
import { KeymanDeveloperProjectFile } from '../../../../../../common/web/types/src/kpj/keyman-developer-project.js';
import { BuildActivity, BuildActivityOptions } from './BuildActivity.js';
import { buildActivities } from './buildActivities.js';
import { InfrastructureMessages } from '../../messages/messages.js';

export const PROJECT_EXTENSION = '.kpj';

export class BuildProject extends BuildActivity {
  public get name(): string { return 'Project'; }
  public get sourceExtension(): string { return PROJECT_EXTENSION; }
  public get compiledExtension(): string { return null; }
  public get description(): string  { return 'Build a keyboard or lexical model project'; }
  public async build(infile: string, callbacks: CompilerCallbacks, options: BuildActivityOptions): Promise<boolean> {
    let builder = new ProjectBuilder(infile, callbacks, options);
    return builder.run();
  }
}

class ProjectBuilder {
  callbacks: CompilerCallbacks;
  infile: string;
  options: BuildActivityOptions;
  project: KeymanDeveloperProject;

  constructor(infile: string, callbacks: CompilerCallbacks, options: BuildActivityOptions) {
    this.infile = path.resolve(infile);
    this.callbacks = callbacks;
    this.options = options;
  }

  async run(): Promise<boolean> {
    if(this.options.outFile) {
      this.callbacks.reportMessage(InfrastructureMessages.Error_OutFileNotValidForProjects());
      return false;
    }

    this.project = this.loadProject();
    if(!this.project) {
      return false;
    }

    // Go through the various file types and build them
    for(let builder of buildActivities) {
      if(builder.sourceExtension == PROJECT_EXTENSION) {
        // We don't support nested projects
        continue;
      }

      if(!await this.buildProjectTargets(builder)) {
        return false;
      }
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
    const project = new KeymanDeveloperProject(this.infile, '2.0', this.callbacks);
    project.populateFiles();
    return project;
  }

  loadProjectFromFile(): KeymanDeveloperProject {
    const kpjData = this.callbacks.loadFile(this.infile);
    const reader = new KPJFileReader(this.callbacks);
    const kpj = reader.read(kpjData);
    const schema = this.callbacks.loadSchema('kpj');
    try {
      reader.validate(kpj, schema);
    } catch(e) {
      this.callbacks.reportMessage(InfrastructureMessages.Error_InvalidProjectFile({message: (e??'').toString()}));
      return null;
    }
    const project = reader.transform(this.infile, kpj);
    return project;
  }

  async buildProjectTargets(activity: BuildActivity): Promise<boolean> {
    let result = true;
    for(let file of this.project.files) {
      if(file.fileType.toLowerCase() == activity.sourceExtension) {
        result = await this.buildTarget(file, activity) && result;
      }
    }
    return result;
  }

  async buildTarget(file: KeymanDeveloperProjectFile, activity: BuildActivity): Promise<boolean> {
    const options = {...this.options};
    options.outFile = this.project.resolveOutputFilePath(file, activity.sourceExtension, activity.compiledExtension);
    const infile = this.project.resolveInputFilePath(file);
    this.callbacks.reportMessage(InfrastructureMessages.Info_BuildingFile({filename: infile}));

    fs.mkdirSync(path.dirname(options.outFile), {recursive:true});

    let result = await activity.build(infile, this.callbacks, options);
    if(result) {
      this.callbacks.reportMessage(InfrastructureMessages.Info_FileBuiltSuccessfully({filename: path.basename(infile)}));
    } else {
      this.callbacks.reportMessage(InfrastructureMessages.Info_FileNotBuiltSuccessfully({filename: path.basename(infile)}));
    }
    return result;
  }

}