import * as path from 'path';
import * as fs from 'fs';
import { CompilerCallbacks, CompilerFileCallbacks, KeymanDeveloperProject, KeymanDeveloperProjectFile, KeymanDeveloperProjectType, KeymanFileTypes } from '@keymanapp/common-types';
import { BuildActivity } from './BuildActivity.js';
import { buildActivities, buildKeyboardInfoActivity, buildModelInfoActivity } from './buildActivities.js';
import { InfrastructureMessages } from '../../messages/infrastructureMessages.js';
import { loadProject } from '../../util/projectLoader.js';
import { ExtendedCompilerOptions } from 'src/util/extendedCompilerOptions.js';
import { getOption } from '@keymanapp/developer-utils';

export class BuildProject extends BuildActivity {
  public get name(): string { return 'Project'; }
  public get sourceExtension(): KeymanFileTypes.Source { return KeymanFileTypes.Source.Project; }
  public get compiledExtension(): KeymanFileTypes.Binary { return null; }
  public get description(): string  { return 'Build a keyboard or lexical model project'; }
  public async build(infile: string, outfile: string, callbacks: CompilerCallbacks, options: ExtendedCompilerOptions): Promise<boolean> {
    if(outfile) {
      callbacks.reportMessage(InfrastructureMessages.Error_OutFileNotValidForProjects());
      return false;
    }

    let builder = new ProjectBuilder(infile, callbacks, options);
    return builder.run();
  }
}

class ProjectBuilder {
  callbacks: CompilerCallbacks;
  infile: string;
  options: ExtendedCompilerOptions;
  project: KeymanDeveloperProject;

  constructor(infile: string, callbacks: CompilerCallbacks, options: ExtendedCompilerOptions) {
    this.infile = path.resolve(infile);
    this.callbacks = new CompilerFileCallbacks(infile, options, callbacks);
    this.options = options;
  }

  async run(): Promise<boolean> {
    this.project = loadProject(this.infile, this.callbacks);
    if(!this.project) {
      return false;
    }

    // Give a hint if the project is v1.0
    if(this.project.options.version != '2.0') {
      if(getOption("prompt to upgrade projects", true)) {
        this.callbacks.reportMessage(InfrastructureMessages.Hint_ProjectIsVersion10());
      }
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
    if(this.options.forPublishing || !this.project.options.skipMetadataFiles) {
      let activity: BuildActivity = null;

      // Determine activity type first of all by examining files in the project
      // Then if that is inconclusive, examine the declared project type
      if(this.project.isKeyboardProject()) {
        activity = buildKeyboardInfoActivity;
      } else if(this.project.isLexicalModelProject()) {
        activity = buildModelInfoActivity;
      } else if(this.project.options.projectType == KeymanDeveloperProjectType.Keyboard) {
        activity = buildKeyboardInfoActivity;
      } else if(this.project.options.projectType == KeymanDeveloperProjectType.LexicalModel) {
        activity = buildModelInfoActivity;
      } else {
        // If we get here, then we are not sure what type of project this is, so
        // we'll assume keyboard
        activity = buildKeyboardInfoActivity;
      }

      if(!await (this.buildProjectTargets(activity))) {
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
    const outfile = this.project.resolveOutputFilePath(file, activity.sourceExtension, activity.compiledExtension);
    options.checkFilenameConventions = this.project.options.checkFilenameConventions ?? this.options.checkFilenameConventions;
    const infile = this.project.resolveInputFilePath(file);

    const buildFilename = path.relative(process.cwd(), infile).replace(/\\/g, '/');
    const callbacks = new CompilerFileCallbacks(buildFilename, options, this.callbacks);
    callbacks.reportMessage(InfrastructureMessages.Info_BuildingFile({filename: infile, relativeFilename:buildFilename}));

    fs.mkdirSync(path.dirname(outfile), {recursive:true});

    let result = await activity.build(infile, outfile, callbacks, options);

    // check if we had a message that causes the build to be a failure
    // note: command line option here, if set, overrides project setting
    result = result && !callbacks.hasFailureMessage(this.options.compilerWarningsAsErrors ?? this.project.options.compilerWarningsAsErrors);

    if(result) {
      callbacks.reportMessage(InfrastructureMessages.Info_FileBuiltSuccessfully({filename: infile, relativeFilename:buildFilename}));
    } else {
      callbacks.reportMessage(InfrastructureMessages.Info_FileNotBuiltSuccessfully({filename: infile, relativeFilename: buildFilename}));
    }

    return result;
  }

}