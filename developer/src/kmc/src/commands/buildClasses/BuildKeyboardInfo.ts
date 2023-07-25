import * as fs from 'fs';
import { BuildActivity } from './BuildActivity.js';
import { CompilerCallbacks, CompilerOptions, KeymanDeveloperProject, KeymanFileTypes } from '@keymanapp/common-types';
import { KeyboardInfoCompiler } from '@keymanapp/kmc-keyboard-info';
import { loadProject } from '../../util/projectLoader.js';
import { InfrastructureMessages } from '../../messages/messages.js';
import { KmpCompiler } from '@keymanapp/kmc-package';

const HelpRoot = 'https://help.keyman.com/keyboard/';

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
    const project = loadProject(infile, callbacks);
    if(!project) {
      return false;
    }

    const metadata = findProjectFile(callbacks, project, KeymanFileTypes.Source.KeyboardInfo);
    if(!metadata) {
      // Project loader should always have added a metadata file
      return false;
    }

    if(!fs.existsSync(project.resolveInputFilePath(metadata))) {
      // For now, if the metadata file does not exist, we won't attempt to build
      // it. One day in the future, when source metadata files become optional,
      // we'll need to skip this
      return true;
    }


    const keyboard = findProjectFile(callbacks, project, KeymanFileTypes.Source.KeymanKeyboard);
    const kps = findProjectFile(callbacks, project, KeymanFileTypes.Source.Package);
    if(!keyboard || !kps)  {
      return false;
    }

    let kmpCompiler = new KmpCompiler(callbacks);
    let kmpJsonData = kmpCompiler.transformKpsToKmpObject(project.resolveInputFilePath(kps));
    if(!kmpJsonData) {
      // Errors will have been emitted by KmpCompiler
      return false;
    }

    const keyboardFileNameJs = project.resolveOutputFilePath(keyboard, KeymanFileTypes.Source.KeymanKeyboard, KeymanFileTypes.Binary.WebKeyboard);
    const keyboard_id = callbacks.path.basename(metadata.filename, KeymanFileTypes.Source.KeyboardInfo);
    const compiler = new KeyboardInfoCompiler(callbacks);
    const data = compiler.writeMergedKeyboardInfoFile(project.resolveInputFilePath(metadata), {
      keyboard_id,
      kmpFileName:  project.resolveOutputFilePath(kps, KeymanFileTypes.Source.Package, KeymanFileTypes.Binary.Package),
      kmpJsonData,
      kpsFileName: project.resolveInputFilePath(kps),
      helpLink: HelpRoot + keyboard_id,
      keyboardFileNameJs: fs.existsSync(keyboardFileNameJs) ? keyboardFileNameJs : undefined,
      sourcePath: calculateSourcePath(infile)
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
  const file = project.files.find(file => file.getFileType() == ext);
  if(!file) {
    callbacks.reportMessage(InfrastructureMessages.Error_FileTypeNotFound({ext}));
  }
  return file;
}

function calculateSourcePath(infile: string): string {
  // Given "c/path/to/keyboards/release/k/keyboard/keyboard.kpj, we want to
  // extract "release/k/keyboard"

  infile = infile.replace(/\\/g, '/');
  const result = /([^\/]+)\/([^\/]+)\/([^\/]+)\/([^\/]+)\.kpj$/.exec(infile);
  if(!result) {
    throw new Error(`Invalid path ${infile}`);
  }
  return `${result[1]}/${result[2]}/${result[3]}`;
}
