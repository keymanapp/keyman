import { KeymanDeveloperProject } from "./types/kpj/keyman-developer-project.js";
import { KPJFileReader } from "./types/kpj/kpj-file-reader.js";
import { CompilerAsyncCallbacks, CompilerCallbacks } from "./compiler-callbacks.js";
import { DeveloperUtilsMessages } from "./developer-utils-messages.js";

// TODO-KMC-TEST: refactor asyncCallbacks, make it a first-class part of CompilerCallbacks
export async function loadProjectFromFile(infile: string, callbacks: CompilerCallbacks, asyncCallbacks: CompilerAsyncCallbacks = null): Promise<KeymanDeveloperProject> {
  const kpjData = await (asyncCallbacks ?? callbacks).fsAsync.readFile(infile);
  if(!kpjData) {
    callbacks.reportMessage(DeveloperUtilsMessages.Error_ProjectFileCouldNotBeRead());
    return null;
  }
  const reader = new KPJFileReader(callbacks);
  let kpj = null;
  try {
    kpj = reader.read(kpjData);
    if(!kpj) {
      callbacks.reportMessage(DeveloperUtilsMessages.Error_ProjectFileCouldNotBeRead());
      return null;
    }

    if(kpj.KeymanDeveloperProject?.Options?.Version && kpj.KeymanDeveloperProject.Options.Version != "1.0" && kpj.KeymanDeveloperProject.Options.Version != "2.0") {
      callbacks.reportMessage(DeveloperUtilsMessages.Error_UnsupportedProjectVersion({version: kpj.KeymanDeveloperProject.Options.Version}));
      return null;
    }
    reader.validate(kpj);
  } catch(e) {
    callbacks.reportMessage(DeveloperUtilsMessages.Error_InvalidProjectFile({message: (e??'').toString()}));
    return null;
  }
  const project = await reader.transform(infile, kpj);
  return project;
}

