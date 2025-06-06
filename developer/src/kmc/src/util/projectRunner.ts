import { KeymanFileTypes } from '@keymanapp/common-types';
import { CompilerCallbacks } from '@keymanapp/developer-utils';
import { isProject, loadProject } from './projectLoader.js';

async function runProject(callbacks: CompilerCallbacks, filename: string, callback: (filename:string)=>Promise<boolean>): Promise<boolean> {
  const project = await loadProject(filename, callbacks);

  for(const file of project.files) {
    if(KeymanFileTypes.filenameIs(file.filename, KeymanFileTypes.Source.Project)) {
      // Don't accidentally recurse into projects
      continue;
    }
    if(!await callback(project.resolveInputFilePath(file))) {
      return false;
    }
  }
  return true;
}

export async function runOnFiles(callbacks: CompilerCallbacks, filenames: string[], callback: (filename:string)=>Promise<boolean>): Promise<boolean> {
  for(const filename of filenames) {
    const result = isProject(filename) ?
      await runProject(callbacks, filename, callback) :
      await callback(filename);
    if(!result) {
      return false;
    }
  }
  return true;
}