import * as path from 'path';
import { platform } from 'os';
import { KmnCompiler } from '@keymanapp/kmc-kmn';
import { KeymanFileTypes } from '@keymanapp/common-types';
import { CompilerOptions, CompilerCallbacks } from '@keymanapp/developer-utils';
import { BuildActivity } from './BuildActivity.js';

export class BuildKmnKeyboard extends BuildActivity {
  public get name(): string { return 'Keyman keyboard'; }
  public get sourceExtension(): KeymanFileTypes.Source { return KeymanFileTypes.Source.KeymanKeyboard; }
  public get compiledExtension(): KeymanFileTypes.Binary { return KeymanFileTypes.Binary.Keyboard; }
  public get description(): string { return 'Build a Keyman keyboard'; }
  public async build(infile: string, outfile: string, callbacks: CompilerCallbacks, options: CompilerOptions): Promise<boolean> {
    // We need to resolve paths to absolute paths before calling kmc-kmn
    infile = getPosixAbsolutePath(infile);
    if(outfile) {
      outfile = getPosixAbsolutePath(outfile);
    }

    const compiler = new KmnCompiler();
    return await super.runCompiler(compiler, infile, outfile, callbacks, options);
  }
}

function getPosixAbsolutePath(filename: string): string {
  if(platform() == 'win32') {
    // On Win32, we need to use backslashes for path.resolve to work
    filename = filename.replace(/\//g, '\\');
  }

  // Resolve to a fully qualified absolute path
  filename = path.resolve(filename);

  if(platform() == 'win32') {
    // Ensure that we convert the result back to posix-style paths which is what
    // kmc-kmn expects. On posix platforms, we assume paths have forward slashes
    // already
    filename = filename.replace(/\\/g, '/');
  }
  return filename;
}
