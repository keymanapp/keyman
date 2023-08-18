import * as fs from 'fs';
import * as path from 'path';
import { BuildActivity } from './BuildActivity.js';
import { CompilerCallbacks, CompilerOptions, KeymanDeveloperProject, KeymanFileTypes } from '@keymanapp/common-types';
import { KeyboardInfoCompiler } from '@keymanapp/kmc-keyboard-info';
import { loadProject } from '../../util/projectLoader.js';
import { InfrastructureMessages } from '../../messages/infrastructureMessages.js';
import { calculateSourcePath } from '../../util/calculateSourcePath.js';
import { getLastGitCommitDate } from '../../util/getLastGitCommitDate.js';

export class BuildKeyboardInfo extends BuildActivity {
  public get name(): string { return 'Keyboard metadata'; }
  public get sourceExtension(): KeymanFileTypes.Source { return KeymanFileTypes.Source.KeyboardInfo; }
  public get compiledExtension(): KeymanFileTypes.Binary { return KeymanFileTypes.Binary.KeyboardInfo; }
  public get description(): string { return 'Build a keyboard metadata file'; }

  public async build(infile: string, callbacks: CompilerCallbacks, options: CompilerOptions): Promise<boolean> {
    const sourcePath = calculateSourcePath(infile);

    if(sourcePath.startsWith('legacy/')) {
      return this.buildLegacy(infile, callbacks);
    }

    if(KeymanFileTypes.filenameIs(infile, KeymanFileTypes.Source.KeyboardInfo)) {
      // We are given a .keyboard_info but need to use the project file in the
      // same folder, so that we can find the related files. This also supports
      // version 2.0 projects (where the .kpj file is optional).
      infile = KeymanFileTypes.replaceExtension(infile, KeymanFileTypes.Source.KeyboardInfo, KeymanFileTypes.Source.Project);
    }

    if(!callbacks.fs.existsSync(infile)) {
      // We cannot build a .keyboard_info if we don't have a repository-style project
      return false;
    }

    const project = loadProject(infile, callbacks);
    if(!project) {
      return false;
    }

    const metadata = this.findProjectFile(callbacks, project, KeymanFileTypes.Source.KeyboardInfo);
    if(!metadata) {
      // Project loader should always have added a metadata file
      return false;
    }

    const keyboard = this.findProjectFile(callbacks, project, KeymanFileTypes.Source.KeymanKeyboard);
    const kps = this.findProjectFile(callbacks, project, KeymanFileTypes.Source.Package);
    if(!keyboard || !kps)  {
      return false;
    }


    const jsFilename = project.resolveOutputFilePath(keyboard, KeymanFileTypes.Source.KeymanKeyboard, KeymanFileTypes.Binary.WebKeyboard);
    const lastCommitDate = getLastGitCommitDate(callbacks.path.dirname(project.resolveInputFilePath(metadata)));

    const compiler = new KeyboardInfoCompiler(callbacks);
    const data = compiler.writeMergedKeyboardInfoFile({
      kmpFilename:  project.resolveOutputFilePath(kps, KeymanFileTypes.Source.Package, KeymanFileTypes.Binary.Package),
      kpsFilename: project.resolveInputFilePath(kps),
      jsFilename: fs.existsSync(jsFilename) ? jsFilename : undefined,
      sourcePath,
      lastCommitDate
    });

    if(data == null) {
      // Error messages have already been emitted by KeyboardInfoCompiler
      return false;
    }

    fs.writeFileSync(
      project.resolveOutputFilePath(metadata, KeymanFileTypes.Source.KeyboardInfo, KeymanFileTypes.Binary.KeyboardInfo),
      data
    );

    return true;
  }

  /**
   * Special case for legacy .keyboard_info files: we just copy the static data into
   * the build/ folder.
   */
  private buildLegacy(infile: string, callbacks: CompilerCallbacks) {
    if(!KeymanFileTypes.filenameIs(infile, KeymanFileTypes.Source.KeyboardInfo)) {
      // We must have a .keyboard_info for legacy projects
      callbacks.reportMessage(InfrastructureMessages.Error_FileTypeNotRecognized({
        filename: infile,
        extensions: KeymanFileTypes.Source.KeyboardInfo
      }));
      return false;
    }

    // For legacy projects, we are fine to hardcode the output path to build/
    const outputFilename = path.join(path.dirname(infile), 'build', path.basename(infile));

    try {
      fs.copyFileSync(infile, outputFilename);
    } catch(e) {
      callbacks.reportMessage(InfrastructureMessages.Fatal_UnexpectedException({e}));
      return false;
    }

    return true;
  }

  findProjectFile(callbacks: CompilerCallbacks, project: KeymanDeveloperProject, ext: KeymanFileTypes.Source) {
    const file = project.files.find(file => file.fileType == ext);
    if(!file) {
      callbacks.reportMessage(InfrastructureMessages.Error_FileTypeNotFound({ext}));
    }
    return file;
  }
}
