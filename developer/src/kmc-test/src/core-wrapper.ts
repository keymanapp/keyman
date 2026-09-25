/*
 * Keyman is copyright (C) SIL Global. MIT License.
 */
import { CompilerCallbacks } from "@keymanapp/developer-utils";
import { MainModule as KeymanCoreModule } from "./import/core/keymancore.js";

export { KeymanCoreModule };

let _km_core: any = null;

export async function km_core_factory(callbacks: CompilerCallbacks): Promise<KeymanCoreModule> {
  if(_km_core) {
    return _km_core;
  }

  if(globalThis.process?.versions?.node) {
    return km_core_factory_node(callbacks);
  } else {
    return km_core_factory_web();
  }
}

/**
 * Load Keyman Core in node.js
 * @param callbacks
 * @returns
 */
async function km_core_factory_node(callbacks: CompilerCallbacks): Promise<KeymanCoreModule> {
  const url = await import('node:url');
  const filename = url.fileURLToPath(import.meta.url);
  const dirname = callbacks.path.join(callbacks.path.dirname(filename), 'import/core');

  const km_core_file = callbacks.path.join(dirname, 'km-core-node.mjs');
  const wasmdir = callbacks.path.dirname(km_core_file);
  const module = await import(url.pathToFileURL(km_core_file).toString());

  const createCoreProcessor = module.default;
  _km_core = await createCoreProcessor({
    locateFile: function (path: string, _scriptDirectory: string) {
      return wasmdir + '/' + path;
    }
  });

  return _km_core;
}

/**
 * Load Keyman Core in a web browser context
 * @returns
 */
async function km_core_factory_web(): Promise<KeymanCoreModule> {
  const dirname = import.meta.url.replace(/\/[^/]+$/, '') + '/import/core/';
  const filename = dirname + 'km-core.js';
  const module = await import(filename);
  const createCoreProcessor = module.default;
  _km_core = await createCoreProcessor({
    locateFile: function (path: string, _scriptDirectory: string) {
      return dirname + path;
    }
  });

  return _km_core;
}
