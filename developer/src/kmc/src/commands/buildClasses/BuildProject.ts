import * as path from 'path';
import * as fs from 'fs';
import { CompilerCallbacks, CompilerFileCallbacks, CompilerOptions, KeymanDeveloperProject, KeymanDeveloperProjectFile, KeymanFileTypes } from '@keymanapp/common-types';
import { BuildActivity } from './BuildActivity.js';
import { buildActivities, buildKeyboardInfoActivity, buildModelInfoActivity } from './buildActivities.js';
import { InfrastructureMessages } from '../../messages/infrastructureMessages.js';
import { loadProject } from '../../util/projectLoader.js';

export class BuildProject extends BuildActivity {
  public get name(): string { return 'Project'; }
  public get sourceExtension(): KeymanFileTypes.Source { return KeymanFileTypes.Source.Project; }
  public get compiledExtension(): KeymanFileTypes.Binary { return null; }
  public get description(): string  { return 'Build a keyboard or lexical model project'; }
  public async build(infile: string, callbacks: CompilerCallbacks, options: CompilerOptions): Promise<boolean> {
    let builder = new ProjectBuilder(infile, callbacks, options);
    return builder.run();
  }
}

class ProjectBuilder {
  callbacks: CompilerCallbacks;
  infile: string;
  options: CompilerOptions;
  project: KeymanDeveloperProject;

  constructor(infile: string, callbacks: CompilerCallbacks, options: CompilerOptions) {
    this.infile = path.resolve(infile);
    this.callbacks = new CompilerFileCallbacks(infile, options, callbacks);
    this.options = options;
  }

  async run(): Promise<boolean> {
    if(this.options.outFile) {
      this.callbacks.reportMessage(InfrastructureMessages.Error_OutFileNotValidForProjects());
      return false;
    }

    this.project = loadProject(this.infile, this.callbacks);
    if(!this.project) {
      return false;
    }

    // Go through the various file types and build them
    for(let builder of buildActivities) {
      if(builder.sourceExtension == KeymanFileTypes.Source.Project) {
        // We don't support nested projects
        continue;
      }

      if(!await this.buildProjectTargets(builder)) {
        return false;
      }
    }

    // Build project metadata
    if(!this.project.options.skipMetadataFiles) {
      if(!await (this.buildProjectTargets(
          this.project.isKeyboardProject()
          ? buildKeyboardInfoActivity
          : buildModelInfoActivity))) {
        return false;
      }
    }

    return true;
  }

  async buildProjectTargets(activity: BuildActivity): Promise<boolean> {
    if(activity.sourceExtension == KeymanFileTypes.Source.Project) {
      return await this.buildTarget(this.project.projectFile, activity);
    }

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
    options.checkFilenameConventions = this.project.options.checkFilenameConventions ?? this.options.checkFilenameConventions;
    const infile = this.project.resolveInputFilePath(file);

    const buildFilename = path.relative(process.cwd(), infile).replace(/\\/g, '/');
    const callbacks = new CompilerFileCallbacks(buildFilename, options, this.callbacks);
    callbacks.reportMessage(InfrastructureMessages.Info_BuildingFile({filename: buildFilename}));

    fs.mkdirSync(path.dirname(options.outFile), {recursive:true});

    let result = await activity.build(infile, callbacks, options);

    // check if we had a message that causes the build to be a failure
    // note: command line option here, if set, overrides project setting
    result = result && !callbacks.hasFailureMessage(this.options.compilerWarningsAsErrors ?? this.project.options.compilerWarningsAsErrors);

    if(result) {
      callbacks.reportMessage(InfrastructureMessages.Info_FileBuiltSuccessfully({filename: buildFilename}));
    } else {
      callbacks.reportMessage(InfrastructureMessages.Info_FileNotBuiltSuccessfully({filename: buildFilename}));
    }

    return result;
  }

}