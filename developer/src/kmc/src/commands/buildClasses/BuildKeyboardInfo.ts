import { BuildActivity } from './BuildActivity.js';
import { CompilerCallbacks, CompilerOptions, KeymanFileTypes } from '@keymanapp/common-types';
import { loadProject } from '../../util/projectLoader.js';

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

    // let outputFilename: string = this.getOutputFilename(infile, options);
    // let code = null;

    // Compile:
    // TODO

    // Output:
    // TODO fs.writeFileSync(outputFilename, code, 'utf8');

    return true;
  }
}