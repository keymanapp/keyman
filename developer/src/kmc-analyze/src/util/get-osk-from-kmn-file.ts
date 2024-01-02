import { CompilerCallbacks, KMX, KmxFileReader } from "@keymanapp/common-types";
import { KmnCompiler } from "@keymanapp/kmc-kmn";

export async function getOskFromKmnFile(callbacks: CompilerCallbacks, filename: string): Promise<{
  kvksFilename?: string,
  touchLayoutFilename?: string
}> {
  let kvksFilename: string;
  let touchLayoutFilename: string;

  const kmnCompiler = new KmnCompiler();
  if(!await kmnCompiler.init(callbacks, {
    shouldAddCompilerVersion: false,
    saveDebug: false,
  })) {
    // kmnCompiler will report errors
    return null;
  }

  let result = await kmnCompiler.run(filename, null);

  if(!result) {
    // kmnCompiler will report any errors
    return null;
  }

  if(result.extra.kvksFilename) {
    kvksFilename = callbacks.resolveFilename(filename, result.extra.kvksFilename);
  }

  const reader = new KmxFileReader();
  const keyboard: KMX.KEYBOARD = reader.read(result.artifacts.kmx.data);
  const touchLayoutStore = keyboard.stores.find(store => store.dwSystemID == KMX.KMXFile.TSS_LAYOUTFILE);

  if(touchLayoutStore) {
    touchLayoutFilename = callbacks.resolveFilename(filename, touchLayoutStore.dpString);
  }

  return { kvksFilename, touchLayoutFilename }
}