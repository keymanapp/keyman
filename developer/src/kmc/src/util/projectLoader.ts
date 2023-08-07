
import * as path from 'path';
import * as fs from 'fs';
import { CompilerCallbacks, KeymanDeveloperProject, KeymanFileTypes, KPJFileReader } from "@keymanapp/common-types";
import { InfrastructureMessages } from "../messages/messages.js";

export const isProject = (filename: string): boolean =>
  fs.existsSync(filename) && (
    fs.statSync(filename).isDirectory() ||
    KeymanFileTypes.sourceTypeFromFilename(filename) == KeymanFileTypes.Source.Project
  );

export function loadProject(infile: string, callbacks: CompilerCallbacks) {
  // TODO: move path requirement out of here?
  infile = path.resolve(infile.replace(/\\/g, '/'));

  // TODO: move fs requirement out of here?
  if(fs.statSync(infile).isDirectory()) {
    // This is a project folder, look for folder-name.kpj
    infile = path.join(infile, path.basename(infile) + KeymanFileTypes.Source.Project);
  }

  // infile should be the .kpj
  if(!infile.endsWith(KeymanFileTypes.Source.Project)) {
    // TODO separate error code
    callbacks.reportMessage(InfrastructureMessages.Error_NotAProjectFile({filename:infile}));
    return null;
  }

  const project = callbacks.fs.existsSync(infile) ?
    loadProjectFromFile(infile, callbacks) :
    loadDefaultProjectFromFolder(infile, callbacks);

  return project;
}

function loadDefaultProjectFromFolder(infile: string, callbacks: CompilerCallbacks): KeymanDeveloperProject {
  // The folder does not contain a .kpj, so construct a default 2.0 .kpj
  const project = new KeymanDeveloperProject(infile, '2.0', callbacks);
  project.populateFiles();
  return project;
}

function loadProjectFromFile(infile: string, callbacks: CompilerCallbacks): KeymanDeveloperProject {
  const kpjData = callbacks.loadFile(infile);
  const reader = new KPJFileReader(callbacks);
  const kpj = reader.read(kpjData);
  try {
    reader.validate(kpj);
  } catch(e) {
    callbacks.reportMessage(InfrastructureMessages.Error_InvalidProjectFile({message: (e??'').toString()}));
    return null;
  }
  const project = reader.transform(infile, kpj);
  return project;
}
