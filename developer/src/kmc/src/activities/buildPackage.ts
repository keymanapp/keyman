import * as fs from 'fs';
import { BuildCommandOptions } from '../commands/build.js';
import KmpCompiler from '@keymanapp/kmc-package';

export async function buildPackage(infile: string, options: BuildCommandOptions): Promise<boolean> {

  let outputFilename: string = options.outFile ? options.outFile : infile.replace(/\.kps$/i, ".kmp");

  //
  // Load .kps source data
  //

  let kpsString: string = fs.readFileSync(infile, 'utf8');
  let kmpCompiler = new KmpCompiler();
  let kmpJsonData = kmpCompiler.transformKpsToKmpObject(kpsString, infile);

  //
  // Build the .kmp package file
  //

  let data = await kmpCompiler.buildKmpFile(infile, kmpJsonData);
  if(data) {
    fs.writeFileSync(outputFilename, data, 'binary');
  } else {
    // TODO error logging
    return false;
  }

  return true;
}