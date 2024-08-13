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

let _minimumKeymanVersion: number;

export function minimumKeymanVersion() {
  return _minimumKeymanVersion;
}

export function minimumKeymanVersionToString() {
  const major = (_minimumKeymanVersion & KMX.KMXFile.VERSION_MASK_MAJOR) >> 8;
  const minor = _minimumKeymanVersion & KMX.KMXFile.VERSION_MASK_MINOR;
  return `${major}.${minor}`;
}

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
  _minimumKeymanVersion = fk.fileVersion;
}

function isKeyboardVersionAutomaticallyDetermined() {
  return (fk.flags & KMX.KMXFile.KF_AUTOMATICVERSION) == KMX.KMXFile.KF_AUTOMATICVERSION;
}

/**
 * @see `verifyAndSetMinimumRequiredKeymanVersion10` for usage
 */
function verifyAndSetMinimumRequiredKeymanVersion(version: KMX.KMX_Version) {
  if(isKeyboardVersionAutomaticallyDetermined()) {
    _minimumKeymanVersion = Math.max(version, _minimumKeymanVersion);
    return true;
  }

  return _minimumKeymanVersion >= version;
}

/**
 * Verify that minimum supported Keyman version in the keyboard is version 10.0,
 * and upgrade to that version if possible and necessary.
 *
 * Will upgrade the minimum version to 10.0 if `KF_AUTOMATICVERSION` flag is set
 * for the keyboard, which correlates to having no `store(&version)` line in the
 * .kmn source file.
 *
 * @returns `true` if the version is now 10.0 or higher, `false` if a lower
 * version has been specified in the source file `store(&version)` line.
 */
export function verifyAndSetMinimumRequiredKeymanVersion10(): boolean {
  return verifyAndSetMinimumRequiredKeymanVersion(KMX.KMX_Version.VERSION_100);
}

/**
 * Verify that minimum supported Keyman version in the keyboard is version 14.0,
 * and upgrade to that version if possible and necessary.
 *
 * Will upgrade the minimum version to 14.0 if `KF_AUTOMATICVERSION` flag is set
 * for the keyboard, which correlates to having no `store(&version)` line in the
 * .kmn source file.
 *
 * @returns `true` if the version is now 14.0 or higher, `false` if a lower
 * version has been specified in the source file `store(&version)` line.
 */
export function verifyAndSetMinimumRequiredKeymanVersion14(): boolean {
  return verifyAndSetMinimumRequiredKeymanVersion(KMX.KMX_Version.VERSION_140);
}

/**
 * Verify that minimum supported Keyman version in the keyboard is version 15.0,
 * and upgrade to that version if possible and necessary.
 *
 * Will upgrade the minimum version to 15.0 if `KF_AUTOMATICVERSION` flag is set
 * for the keyboard, which correlates to having no `store(&version)` line in the
 * .kmn source file.
 *
 * @returns `true` if the version is now 15.0 or higher, `false` if a lower
 * version has been specified in the source file `store(&version)` line.
 */
export function verifyAndSetMinimumRequiredKeymanVersion15(): boolean {
  return verifyAndSetMinimumRequiredKeymanVersion(KMX.KMX_Version.VERSION_150);
}

/**
 * Verify that minimum supported Keyman version in the keyboard is version 17.0,
 * and upgrade to that version if possible and necessary.
 *
 * Will upgrade the minimum version to 17.0 if `KF_AUTOMATICVERSION` flag is set
 * for the keyboard, which correlates to having no `store(&version)` line in the
 * .kmn source file.
 *
 * @returns `true` if the version is now 17.0 or higher, `false` if a lower
 * version has been specified in the source file `store(&version)` line.
 */
export function verifyAndSetMinimumRequiredKeymanVersion17(): boolean {
  return verifyAndSetMinimumRequiredKeymanVersion(KMX.KMX_Version.VERSION_170);
}

export function isKeyboardVersion10OrLater(): boolean {
  return fk.fileVersion >= KMX.KMXFile.VERSION_100;
}

export function isKeyboardVersion14OrLater(): boolean {
  return fk.fileVersion >= KMX.KMXFile.VERSION_140;
}

export function isKeyboardVersion17OrLater(): boolean {
  return fk.fileVersion >= KMX.KMXFile.VERSION_170;
}
