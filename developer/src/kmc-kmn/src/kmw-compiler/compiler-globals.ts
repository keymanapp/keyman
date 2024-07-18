import { KMX, CompilerCallbacks, CompilerOptions } from "@keymanapp/common-types";
import { KmnCompilerResult } from "../compiler/compiler.js";

export let FTabStop: string;
export let nl: string;
export let FCompilerWarningsAsErrors = false;
export let kmxResult: KmnCompilerResult;
export let fk: KMX.KEYBOARD;
export let FMnemonic: boolean;
export let options: CompilerOptions;
export let callbacks: CompilerCallbacks;
export let kmnfile: string;
export let FUnreachableKeys: KMX.KEY[];
export let FCallFunctions: string[];
export let FFix183_LadderLength: number;

export function setupGlobals(
  _callbacks: CompilerCallbacks,
  _options: CompilerOptions,
  _tab: string,
  _nl: string,
  _kmxResult: KmnCompilerResult,
  _keyboard: KMX.KEYBOARD,
  _kmnfile: string
) {
  callbacks = _callbacks;
  options = _options;
  FTabStop = _tab;
  nl = _nl;
  kmxResult = _kmxResult;
  fk = _keyboard;
  kmnfile = _kmnfile;
  FUnreachableKeys = [];
  FCallFunctions = [];
  FFix183_LadderLength = 100; // TODO: option
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

export function IsKeyboardVersion17OrLater(): boolean {
  return fk.fileVersion >= KMX.KMXFile.VERSION_170;
}
