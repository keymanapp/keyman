import * as fs from 'fs';
import { BuildActivity } from './BuildActivity.js';
import { CompilerCallbacks, CompilerOptions, KeymanFileTypes } from '@keymanapp/common-types';
import { writeMergedModelMetadataFile } from '@keymanapp/kmc-model-info';
import { KmpCompiler } from '@keymanapp/kmc-package';
import { loadProject } from '../../util/projectLoader.js';
import { InfrastructureMessages } from '../../messages/messages.js';
import { calculateSourcePath } from '../../util/calculateSourcePath.js';

export class BuildModelInfo extends BuildActivity {
  public get name(): string { return 'Lexical model metadata'; }
  public get sourceExtension(): KeymanFileTypes.Source { return KeymanFileTypes.Source.ModelInfo; }
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
  public async build(infile: string, callbacks: CompilerCallbacks, options: CompilerOptions): Promise<boolean> {
    if(KeymanFileTypes.filenameIs(infile, KeymanFileTypes.Source.ModelInfo)) {
      // We are given a .model_info but need to use the project file in the
      // same folder, so that we can find the related files.
      infile = KeymanFileTypes.replaceExtension(infile, KeymanFileTypes.Source.ModelInfo, KeymanFileTypes.Source.Project);
    }
    const project = loadProject(infile, callbacks);
    if(!project) {
      // Error messages will be reported by loadProject
      return false;
    }

    const metadata = project.files.find(file => file.fileType == KeymanFileTypes.Source.ModelInfo);
    if(!metadata) {
      callbacks.reportMessage(InfrastructureMessages.Error_FileTypeNotFound({ext: KeymanFileTypes.Source.ModelInfo}));
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

    let kmpCompiler = new KmpCompiler(callbacks);
    let kmpJsonData = kmpCompiler.transformKpsToKmpObject(project.resolveInputFilePath(kps));
    if(!kmpJsonData) {
      // Errors will have been emitted by KmpCompiler
      return false;
    }

    const data = writeMergedModelMetadataFile(
      project.resolveInputFilePath(metadata),
      callbacks,
      {
        model_id: callbacks.path.basename(metadata.filename, KeymanFileTypes.Source.ModelInfo),
        kmpJsonData,
        sourcePath: calculateSourcePath(infile),
        modelFileName: project.resolveOutputFilePath(model, KeymanFileTypes.Source.Model, KeymanFileTypes.Binary.Model),
        kmpFileName: project.resolveOutputFilePath(kps, KeymanFileTypes.Source.Package, KeymanFileTypes.Binary.Package),
      }
    );

    if(data == null) {
      // Error messages have already been emitted by writeMergedModelMetadataFile
      return false;
    }

    fs.writeFileSync(
      project.resolveOutputFilePath(metadata, KeymanFileTypes.Source.ModelInfo, KeymanFileTypes.Binary.ModelInfo),
      data
    );

    return true;
  }
}