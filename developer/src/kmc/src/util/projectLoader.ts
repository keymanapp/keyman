
import * as path from 'path';
import * as fs from 'fs';
import { KeymanFileTypes } from "@keymanapp/common-types";
import { CompilerCallbacks, KeymanDeveloperProject, KPJFileReader } from "@keymanapp/developer-utils";
import { InfrastructureMessages } from "../messages/infrastructureMessages.js";

export const isProject = (filename: string): boolean =>
  fs.existsSync(filename) && (
    fs.statSync(filename).isDirectory() ||
    KeymanFileTypes.sourceTypeFromFilename(filename) == KeymanFileTypes.Source.Project
  );

export function loadProject(infile: string, callbacks: CompilerCallbacks) {
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
    loadProjectFromFile(infile, callbacks) :
    loadDefaultProjectFromFolder(infile, callbacks);

  return project;
}

function loadDefaultProjectFromFolder(infile: string, callbacks: CompilerCallbacks): KeymanDeveloperProject {
  // The folder does not contain a .kpj, so construct a default 2.0 .kpj
  const project = new KeymanDeveloperProject(infile, '2.0', callbacks);
  if(!project.populateFiles()) {
    callbacks.reportMessage(InfrastructureMessages.Error_InvalidProjectFolder({folderName:path.dirname(infile)}));
    return null;
  }
  return project;
}

function loadProjectFromFile(infile: string, callbacks: CompilerCallbacks): KeymanDeveloperProject {
  const kpjData = callbacks.loadFile(infile);
  const reader = new KPJFileReader(callbacks);
  let kpj = null;
  try {
    kpj = reader.read(kpjData);
    if(kpj.KeymanDeveloperProject?.Options?.Version && kpj.KeymanDeveloperProject.Options.Version != "1.0" && kpj.KeymanDeveloperProject.Options.Version != "2.0") {
      callbacks.reportMessage(InfrastructureMessages.Error_UnsupportedProjectVersion({version: kpj.KeymanDeveloperProject.Options.Version}));
      return null;
    }
    reader.validate(kpj);
  } catch(e) {
    callbacks.reportMessage(InfrastructureMessages.Error_InvalidProjectFile({message: (e??'').toString()}));
    return null;
  }
  const project = reader.transform(infile, kpj);
  return project;
}
