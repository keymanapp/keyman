import { spawnSync } from 'child_process';
import * as path from 'path';
import { BuildCommandOptions } from '../commands/build.js';
import { getDeveloperBinPath } from '../util/getDeveloperBinPath.js';

export function buildKmnKeyboard(infile: string, options: BuildCommandOptions) {
  // We'll call out to kmcomp.exe to build a .kmn keyboard into a .kmx
  const binRoot = getDeveloperBinPath();
  if(binRoot == null) {
    console.error('Could not locate Keyman Developer bin path');
    return false;
  }

  let args = ['-nologo'];
  if(options.debug) {
    args.push('-d');
  }
  args.push(infile);
  if(options.outFile) {
    args.push(options.outFile);
  }

  const kmcomp = path.join(binRoot, 'kmcomp.exe');
  let child = spawnSync(kmcomp, args, {
    encoding: 'utf8'
  });

  if(child.error) {
    console.error(child.error);
    return false;
  }

  console.log(child.stdout);
  if(child.stderr) {
    console.error(child.stderr);
  }

  return child.status === 0;
}