import * as path from 'path';
import { BuildCommandOptions } from '../commands/build.js';
import { Compiler } from '@keymanapp/kmc-kmn';

export async function buildKmnKeyboard(infile: string, options: BuildCommandOptions): Promise<boolean> {

  let compiler = new Compiler();
  if(!await compiler.init()) {
    return false;
  }

  infile = infile.replace(/\\/g, '/');
  infile = path.posix.normalize(infile);

  let outfile = (options.outFile ?? infile).replace(/\\/g, '/');
  outfile = path.posix.normalize(outfile).replace(/\.km.$/i, '.kmx');

  // TODO: Currently this only builds .kmn->.kmx, and targeting .js is as-yet unsupported
  // TODO: Support additional options compilerWarningsAsErrors, warnDeprecatedCode

  return compiler.run(infile, outfile, {
    saveDebug: options.debug,
    shouldAddCompilerVersion: options.compilerVersion,
  });
}