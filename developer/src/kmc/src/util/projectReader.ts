import * as fs from 'fs';
import { CompilerCallbacks, KeymanDeveloperProject, KeymanFileTypes, KPJFileReader } from "@keymanapp/common-types";
import { InfrastructureMessages } from '../messages/messages.js';

async function doRunProject(callbacks: CompilerCallbacks, project: KeymanDeveloperProject, basefile: string, callback: (filename:string)=>Promise<boolean>): Promise<boolean> {
  const files = project.files
    .map(file => callbacks.resolveFilename(basefile, file.filePath))
    .filter(file => !KeymanFileTypes.filenameIs(file, KeymanFileTypes.Source.Project)); // Don't accidentally recurse into projects
  for(let file of files) {
    if(!await callback(file)) {
      return false;
    }
  }
  return true;
}

export async function runProject(callbacks: CompilerCallbacks, filename: string, callback: (filename:string)=>Promise<boolean>): Promise<boolean> {
  const reader = new KPJFileReader(callbacks);
  const source = reader.read(callbacks.loadFile(filename));
  const project = reader.transform(filename, source);
  return await doRunProject(callbacks, project, filename, callback);
}

export async function runProjectFolder(callbacks: CompilerCallbacks, folder: string, callback: (filename:string)=>Promise<boolean>): Promise<boolean> {
  let kpjFile = callbacks.path.join(folder, callbacks.path.basename(folder) + KeymanFileTypes.Source.Project);

  if(callbacks.fs.existsSync(kpjFile)) {
    // TODO: callbacks.reportMessage(AnalyzerMessages.Info_ScanningFile({type:'project', name:kpjFile}));
    return await runProject(callbacks, kpjFile, callback);
  } else {
    // TODO: this.callbacks.reportMessage(AnalyzerMessages.Info_ScanningFile({type:'project folder', name:folder}));
    const project = new KeymanDeveloperProject(kpjFile, '2.0', callbacks);
    project.populateFiles();
    return await doRunProject(callbacks, project, folder, callback);
  }
}

export async function runOnFiles(callbacks: CompilerCallbacks, filenames: string[], callback: (filename:string)=>Promise<boolean>): Promise<boolean> {
  for(let filename of filenames) {
    if(!fs.existsSync(filename)) {
      callbacks.reportMessage(InfrastructureMessages.Error_FileDoesNotExist({filename}));
      continue;
    }

    // If infile is a directory, then we treat that as a project and build it
    if(fs.statSync(filename).isDirectory()) {
      if(!await runProjectFolder(callbacks, filename, callback)) {
        return false;
      }
    } else if(KeymanFileTypes.sourceTypeFromFilename(filename) == KeymanFileTypes.Source.Project) {
      if(!await runProject(callbacks, filename, callback)) {
        return false;
      }
    } else {
      if(!await callback(filename)) {
        return false;
      }
    }
  }
  return true;
}