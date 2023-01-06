import { KEYBOARD, KMXFile } from "../../../../../common/web/types/src/kmx/kmx.js";
import CompilerOptions from "./compiler-options.js";

export let FTabStop: string;
export let nl: string;
export let FCompilerWarningsAsErrors = false;
export let fk: KEYBOARD;
export let FMnemonic: boolean;
export let options: CompilerOptions;

export function setupGlobals(_options: CompilerOptions, _tab: string, _nl: string, _keyboard: KEYBOARD) {
  options = _options;
  FTabStop = _tab;
  nl = _nl;
  fk = _keyboard;
}

export function IsKeyboardVersion10OrLater(): boolean {
  return fk.fileVersion >= KMXFile.VERSION_100;
}

export function IsKeyboardVersion14OrLater(): boolean {
  return fk.fileVersion >= KMXFile.VERSION_140;
}

export function IsKeyboardVersion15OrLater(): boolean {
  return fk.fileVersion >= KMXFile.VERSION_150;
}
