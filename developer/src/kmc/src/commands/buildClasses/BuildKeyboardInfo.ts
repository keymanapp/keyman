import * as fs from 'fs';
import * as path from 'path';
import { BuildActivity } from './BuildActivity.js';
import { KeymanFileTypes } from '@keymanapp/common-types';
import { CompilerCallbacks } from '@keymanapp/developer-utils';
import { KeyboardInfoCompiler } from '@keymanapp/kmc-keyboard-info';
import { loadProject } from '../../util/projectLoader.js';
import { InfrastructureMessages } from '../../messages/infrastructureMessages.js';
import { calculateSourcePath } from '../../util/calculateSourcePath.js';
import { getLastGitCommitDate } from '../../util/getLastGitCommitDate.js';
import { ExtendedCompilerOptions } from 'src/util/extendedCompilerOptions.js';

export class BuildKeyboardInfo extends BuildActivity {
  public get name(): string { return 'Keyboard metadata'; }
  public get sourceExtension(): KeymanFileTypes.Source { return KeymanFileTypes.Source.Project; }
  public get compiledExtension(): KeymanFileTypes.Binary { return KeymanFileTypes.Binary.KeyboardInfo; }
  public get description(): string { return 'Build a keyboard metadata file'; }
  public async build(infile: string, _outfile: string, callbacks: CompilerCallbacks, options: ExtendedCompilerOptions): Promise<boolean> {
    if(!KeymanFileTypes.filenameIs(infile, KeymanFileTypes.Source.Project)) {
      // Even if the project file does not exist, we use its name as our reference
      // in order to avoid ambiguity
      throw new Error(`BuildKeyboardInfo called with unexpected file type ${infile}`);
    }

    const project = loadProject(infile, callbacks);
    if(!project) {
      // Error messages written by loadProject
      return false;
    }

    const kps = project.files.find(file => file.fileType == KeymanFileTypes.Source.Package);
    if(!kps)  {
      callbacks.reportMessage(InfrastructureMessages.Error_FileTypeNotFound({ext: KeymanFileTypes.Source.Package}));
      return false;
    }

    const keyboard = project.files.find(file => file.fileType == KeymanFileTypes.Source.KeymanKeyboard);
    const jsFilename = keyboard ? project.resolveOutputFilePath(keyboard, KeymanFileTypes.Source.KeymanKeyboard, KeymanFileTypes.Binary.WebKeyboard) : null;
    const historyPath = path.join(project.projectPath, KeymanFileTypes.HISTORY_MD);
    const lastCommitDate = getLastGitCommitDate(fs.existsSync(historyPath) ? historyPath : project.projectPath);
    const sources = {
      kmpFilename:  project.resolveOutputFilePath(kps, KeymanFileTypes.Source.Package, KeymanFileTypes.Binary.Package),
      kpsFilename: project.resolveInputFilePath(kps),
      jsFilename: jsFilename && fs.existsSync(jsFilename) ? jsFilename : undefined,
      sourcePath: calculateSourcePath(infile),
      lastCommitDate,
      forPublishing: !!options.forPublishing,
    };
    // Note: should we always ignore the passed-in output filename for .keyboard_info?
    const outputFilename = project.getOutputFilePath(KeymanFileTypes.Binary.KeyboardInfo);
    const compiler = new KeyboardInfoCompiler();
    return await super.runCompiler(compiler, infile, outputFilename, callbacks, {...options, sources});
  }
}
