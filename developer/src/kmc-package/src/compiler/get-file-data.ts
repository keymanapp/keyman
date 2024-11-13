/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2024-11-11
 */
import { CompilerCallbacks } from '@keymanapp/developer-utils';
import { PackageCompilerMessages } from './package-compiler-messages.js';

const FLO_SOURCE =    /^flo:(?<family>.+)$/;
const GITHUB_SOURCE = /^github:(?<name>[a-zA-Z0-9-].+)\/(?<repo>[\w\.-]+)\/raw\/(?<hash>[a-f0-9]{40})\/(?<path>.+)$/;

export interface KmpCompilerFileDataResult {
  data: Uint8Array;
  basename: string;
};

let floFamiliesCache: any = undefined; // set to null on error, otherwise an object from JSON

const FLO_FAMILIES_URL = 'https://fonts.languagetechnology.org/families.json';

async function getFloFamilies(callbacks: CompilerCallbacks) {
  if(floFamiliesCache === undefined) {
    try {
      floFamiliesCache = await callbacks.net.fetchJSON(FLO_FAMILIES_URL);
      /* c8 ignore next 12 */
      if(!floFamiliesCache) {
        callbacks.reportMessage(PackageCompilerMessages.Error_FloDataCouldNotBeRead({url: FLO_FAMILIES_URL}));
        floFamiliesCache = null;
      }
      else if(typeof floFamiliesCache != 'object') {
        callbacks.reportMessage(PackageCompilerMessages.Error_FloDataIsInvalidFormat({url: FLO_FAMILIES_URL}));
        floFamiliesCache = null;
      }
    } catch(e) {
      callbacks.reportMessage(PackageCompilerMessages.Error_FloDataCouldNotBeRead({url: FLO_FAMILIES_URL, e}))
      floFamiliesCache = null;
    }
  }
  return floFamiliesCache;
}

async function getFileDataFromFlo(callbacks: CompilerCallbacks, _kpsFilename: string, inputFilename: string, matches: RegExpExecArray): Promise<KmpCompilerFileDataResult> {
  const floFamilies = await getFloFamilies(callbacks);
  /* c8 ignore next 4 */
  if(!floFamilies) {
    // Error already reported by getFloFamilies
    return null;
  }

  const family = floFamilies[matches.groups.family];
  if(!family) {
    callbacks.reportMessage(PackageCompilerMessages.Error_FontNotFoundInFlo({filename: inputFilename, family: matches.groups.family}));
    return null;
  }

  if(!family.distributable) {
    callbacks.reportMessage(PackageCompilerMessages.Warn_FontFromFloIsNotFreelyDistributable(
      {filename: inputFilename, family: matches.groups.family}));
  }

  // TODO: consider .woff, .woff2 for web font inclusion
  const ttf = family.defaults?.ttf;
  if(!ttf) {
    callbacks.reportMessage(PackageCompilerMessages.Error_FontInFloDoesNotHaveDefaultTtf({filename: inputFilename, family: matches.groups.family}));
    return null;
  }

  const file = family.files[ttf];
  /* c8 ignore next 4 */
  if(!file) {
    callbacks.reportMessage(PackageCompilerMessages.Error_FontInFloHasBrokenDefaultTtf({filename: inputFilename, family: matches.groups.family}));
    return null;
  }

  if(!file.flourl && !file.url) {
    callbacks.reportMessage(PackageCompilerMessages.Error_FontInFloHasNoDownloadAvailable({filename: inputFilename, family: matches.groups.family}));
    return null;
  }

  const url = file.flourl ?? file.url;
  try {
    const data = await callbacks.net.fetchBlob(url);
    if(!data) {
      callbacks.reportMessage(PackageCompilerMessages.Error_FontFileCouldNotBeDownloaded({filename: inputFilename, url}));
      return null;
    }
    return { data, basename: ttf };
    /* c8 ignore next 4 */
  } catch(e) {
    callbacks.reportMessage(PackageCompilerMessages.Error_FontFileCouldNotBeDownloaded({filename: inputFilename, url, e}));
    return null;
  }
}

async function getFileDataFromGitHub(callbacks: CompilerCallbacks, _kpsFilename: string, inputFilename: string, matches: RegExpExecArray): Promise<KmpCompilerFileDataResult> {
  // /^github:(?<name>[a-zA-Z0-9-].+)\/(?<repo>[\w\.-]+)\/raw\/(?<hash>[a-f0-9]{40})\/(?<path>.+)$/
  const githubUrl = `https://github.com/${matches.groups.name}/${matches.groups.repo}/raw/${matches.groups.hash}/${matches.groups.path}`;
  try {
    const res = await callbacks.net.fetchBlob(githubUrl);
    if(!res) {
      callbacks.reportMessage(PackageCompilerMessages.Error_FontFileCouldNotBeDownloaded({filename: inputFilename, url: githubUrl}));
      return null;
    }
    return { data: res, basename: callbacks.path.basename(matches.groups.path) };
    /* c8 ignore next 4 */
  } catch(e) {
    callbacks.reportMessage(PackageCompilerMessages.Error_FontFileCouldNotBeDownloaded({filename: inputFilename, url: githubUrl, e}));
    return null;
  }
}

type KmpCompilerFileDataProc = (callbacks: CompilerCallbacks, kpsFilename: string, inputFilename: string, matches: RegExpExecArray) => Promise<KmpCompilerFileDataResult>;

interface KmpCompilerFileDataSource {
  regex: RegExp;
  proc: KmpCompilerFileDataProc;
};

export const EXTERNAL_FILE_DATA_SOURCES: KmpCompilerFileDataSource[] = [
  { regex: FLO_SOURCE, proc: getFileDataFromFlo },
  { regex: GITHUB_SOURCE, proc: getFileDataFromGitHub },
  // TODO: noto
];

/** @internal */
export const unitTestEndpoints = {
  GITHUB_SOURCE,
  getFileDataFromGitHub,
  FLO_SOURCE,
  getFileDataFromFlo,
};
