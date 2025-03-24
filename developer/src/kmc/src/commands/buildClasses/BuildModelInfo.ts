import * as fs from 'fs';
import * as path from 'path';
import { BuildActivity } from './BuildActivity.js';
import { KeymanFileTypes } from '@keymanapp/common-types';
import { CompilerCallbacks } from '@keymanapp/developer-utils';
import { ModelInfoCompiler } from '@keymanapp/kmc-model-info';
import { KmpCompiler } from '@keymanapp/kmc-package';
import { loadProject } from '../../util/projectLoader.js';
import { InfrastructureMessages } from '../../messages/infrastructureMessages.js';
import { calculateSourcePath } from '../../util/calculateSourcePath.js';
import { getLastGitCommitDate } from '../../util/getLastGitCommitDate.js';
import { ExtendedCompilerOptions } from '../../util/extendedCompilerOptions.js';

export class BuildModelInfo extends BuildActivity {
  public get name(): string { return 'Lexical model metadata'; }
  public get sourceExtension(): KeymanFileTypes.Source { return KeymanFileTypes.Source.Project; }
  public get compiledExtension(): KeymanFileTypes.Binary { return KeymanFileTypes.Binary.ModelInfo; }
  public get description(): string { return 'Build a lexical model metadata file'; }

  /**
   * Compiles a build/ .model_info from a source .model_info file and
   * corresponding model and package data files. Data not provided in the
   * .model_info file will be extracted from the other source files.
   * @param infile a .kpj file or a .model_info file. When a .model_info file is
   *               given, will look for a .kpj in the same folder.
   * @param callbacks
   * @param options
   * @returns
   */
  public async build(infile: string, _outfile: string, callbacks: CompilerCallbacks, options: ExtendedCompilerOptions): Promise<boolean> {
    if(!KeymanFileTypes.filenameIs(infile, KeymanFileTypes.Source.Project)) {
      // Even if the project file does not exist, we use its name as our reference
      // in order to avoid ambiguity
      throw new Error(`BuildModelInfo called with unexpected file type ${infile}`);
    }

    const project = await loadProject(infile, callbacks);
    if(!project) {
      // Error messages will be reported by loadProject
      return false;
    }

    const model = project.files.find(file => file.fileType == KeymanFileTypes.Source.Model);
    if(!model) {
      callbacks.reportMessage(InfrastructureMessages.Error_FileTypeNotFound({ext: KeymanFileTypes.Source.Model}));
      return false;
    }

    const kps = project.files.find(file => file.fileType == KeymanFileTypes.Source.Package);
    if(!kps) {
      callbacks.reportMessage(InfrastructureMessages.Error_FileTypeNotFound({ext: KeymanFileTypes.Source.Package}));
      return false;
    }

    let kmpCompiler = new KmpCompiler();
    if(!await kmpCompiler.init(callbacks, options)) {
      // Errors will have been emitted by KmpCompiler
      return false;
    }

    let kmpJsonData = kmpCompiler.transformKpsToKmpObject(project.resolveInputFilePath(kps));
    if(!kmpJsonData) {
      // Errors will have been emitted by KmpCompiler
      return false;
    }

    const historyPath = path.join(project.projectPath, KeymanFileTypes.HISTORY_MD);
    const lastCommitDate = getLastGitCommitDate(fs.existsSync(historyPath) ? historyPath : project.projectPath);
    const sources = {
      model_id: path.basename(project.projectPath, KeymanFileTypes.Source.Project),
      kmpJsonData,
      sourcePath: calculateSourcePath(infile),
      modelFileName: project.resolveOutputFilePath(model, KeymanFileTypes.Source.Model, KeymanFileTypes.Binary.Model),
      kmpFileName: project.resolveOutputFilePath(kps, KeymanFileTypes.Source.Package, KeymanFileTypes.Binary.Package),
      kpsFilename: project.resolveInputFilePath(kps),
      lastCommitDate,
      forPublishing: !!options.forPublishing,
    };

    // Note: should we always ignore the passed-in output filename for .model_info?
    const outputFilename = project.getOutputFilePath(KeymanFileTypes.Binary.ModelInfo);

    const compiler = new ModelInfoCompiler();
    return await super.runCompiler(compiler, infile, outputFilename, callbacks, {...options, sources});
  }
}