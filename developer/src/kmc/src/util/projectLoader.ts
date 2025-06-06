
import * as path from 'path';
import * as fs from 'fs';
import { KeymanFileTypes } from "@keymanapp/common-types";
import { CompilerCallbacks, KeymanDeveloperProject, ProjectLoader } from "@keymanapp/developer-utils";
import { InfrastructureMessages } from "../messages/infrastructureMessages.js";

export const isProject = (filename: string): boolean =>
  fs.existsSync(filename) && (
    fs.statSync(filename).isDirectory() ||
    KeymanFileTypes.sourceTypeFromFilename(filename) == KeymanFileTypes.Source.Project
  );

export async function loadProject(infile: string, callbacks: CompilerCallbacks) {
  infile = path.resolve(infile.replace(/\\/g, '/'));

  if(fs.existsSync(infile) && fs.statSync(infile).isDirectory()) {
    // This is a project folder, look for folder-name.kpj
    infile = path.join(infile, path.basename(infile) + KeymanFileTypes.Source.Project);
  }

  // infile should be the .kpj
  if(!infile.endsWith(KeymanFileTypes.Source.Project)) {
    callbacks.reportMessage(InfrastructureMessages.Error_NotAProjectFile({filename:infile}));
    return null;
  }

  const project = callbacks.fs.existsSync(infile) ?
    await ProjectLoader.loadProjectFromFile(infile, callbacks) :
    await loadDefaultProjectFromFolder(infile, callbacks);

  return project;
}

async function loadDefaultProjectFromFolder(infile: string, callbacks: CompilerCallbacks): Promise<KeymanDeveloperProject> {
  // The folder does not contain a .kpj, so construct a default 2.0 .kpj
  const project = new KeymanDeveloperProject(infile, '2.0', callbacks);
  if(!await project.populateFiles()) {
    callbacks.reportMessage(InfrastructureMessages.Error_InvalidProjectFolder({folderName:path.dirname(infile)}));
    return null;
  }
  return project;
}

