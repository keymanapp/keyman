import * as fs from 'fs';
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

    const metadata = findProjectFile(callbacks, project, KeymanFileTypes.Source.KeyboardInfo);
    if(!metadata) {
      // Project loader should always have added a metadata file
      return false;
    }

    const keyboard = findProjectFile(callbacks, project, KeymanFileTypes.Source.KeymanKeyboard);
    const kps = findProjectFile(callbacks, project, KeymanFileTypes.Source.Package);
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
      sourcePath: calculateSourcePath(infile),
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
}

function findProjectFile(callbacks: CompilerCallbacks, project: KeymanDeveloperProject, ext: KeymanFileTypes.Source) {
  const file = project.files.find(file => file.fileType == ext);
  if(!file) {
    callbacks.reportMessage(InfrastructureMessages.Error_FileTypeNotFound({ext}));
  }
  return file;
}
