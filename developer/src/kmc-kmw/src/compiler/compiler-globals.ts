import { KMX, CompilerCallbacks } from "@keymanapp/common-types";
import CompilerOptions from "./compiler-options.js";

export let FTabStop: string;
export let nl: string;
export let FCompilerWarningsAsErrors = false;
export let fk: KMX.KEYBOARD;
export let FMnemonic: boolean;
export let options: CompilerOptions;
export let callbacks: CompilerCallbacks;

export function setupGlobals(_callbacks: CompilerCallbacks, _options: CompilerOptions, _tab: string, _nl: string, _keyboard: KMX.KEYBOARD) {
  callbacks = _callbacks;
  options = _options;
  FTabStop = _tab;
  nl = _nl;
  fk = _keyboard;
}

export function IsKeyboardVersion10OrLater(): boolean {
  return fk.fileVersion >= KMX.KMXFile.VERSION_100;
}

export function IsKeyboardVersion14OrLater(): boolean {
  return fk.fileVersion >= KMX.KMXFile.VERSION_140;
}

export function IsKeyboardVersion15OrLater(): boolean {
  return fk.fileVersion >= KMX.KMXFile.VERSION_150;
}
