import { spawnSync } from 'child_process';
import * as path from 'path';
import { BuildCommandOptions } from '../commands/build.js';
import { getDeveloperBinPath } from '../util/getDeveloperBinPath.js';

export async function buildKmnKeyboard(infile: string, options: BuildCommandOptions): Promise<boolean> {
  // We'll call out to kmcomp.exe to build a .kmn keyboard into a .kmx Note:
  // kmcomp.exe will also build a .js if it is required, and may not actually
  // generate a .kmx if the output target defined in the .kmn is mobile/web
  // only. kmcomp.exe will also build the .kvk file.
  const binRoot = getDeveloperBinPath();
  if(binRoot == null) {
    console.error('Could not locate Keyman Developer bin path');
    return false;
  }

  let args = ['-nologo'];
  if(options.debug) {
    args.push('-d');
  }

  args.push(path.win32.normalize(infile));

  // .* target file name option allows us to specify .kmx output file and the
  // .js output file will also be generated as required, in the same way as when
  // we build a project. Note: this is a stop gap as we work to deprecate
  // kmcomp.exe and replace it with kmcmp (cross-platform .kmx compiler in C++)
  // + kmc-kmw (KMX->KMW transpiler in TypeScript) + kmc-kvk (KVK compiler in
  // TypeScript).
  let outfile = (options.outFile ?? infile).replace(/\\/g, '/');
  // For now, we normalize to posix, and then renormalize to win32 for launching
  // kmcomp, until we have a cross-platform alternative to use
  outfile = path.win32.normalize(options.outFile).replace(/\.km.$/i, '.*');
  args.push(outfile);

  const kmcomp = path.join(binRoot, 'kmcomp.exe');

  // We won't attempt to make this call cross-platform, yet.
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