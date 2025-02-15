/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2024-11-11
 */
import { CompilerCallbacks, GitHubUrls } from '@keymanapp/developer-utils';
import { PackageCompilerMessages } from './package-compiler-messages.js';

/**
 * Matches a http or https URL (without checking anything other than protocol)
 */
const URL_SOURCE = /^http(?:s)?:\/\/(.+)$/;

/**
 * Matches a fonts.languagetechnology.org reference `flo:{id}`
 */
const FLO_SOURCE = /^flo:(?<family>[a-z0-9_-]+)$/;

export interface KmpCompilerFileDataResult {
  data: Uint8Array;
  basename: string;
};

/**
 * Test if a filename is a reference to a remote file. The remote file may be a
 * stable URL (e.g. a GitHub raw URL), or an unstable reference, e.g. 'flo:familyid'
 * @param inputFilename
 * @returns
 */
export function isLocalFile(inputFilename: string) {
  return !URL_SOURCE.test(inputFilename) && !FLO_SOURCE.test(inputFilename);
}

export async function getFileDataFromRemote(callbacks: CompilerCallbacks, inputFilename: string, sourceFilename: string): Promise<KmpCompilerFileDataResult> {
  const matches: GitHubUrls.GitHubRegexMatchArray = GitHubUrls.GITHUB_STABLE_SOURCE.exec(inputFilename);
  if(!matches) {
    callbacks.reportMessage(PackageCompilerMessages.Error_UriIsNotARecognizedStableGitHubUri({url: inputFilename}));
    return null;
  }

  const result = await getFileDataFromGitHub(callbacks, inputFilename, matches);
  if(!result) {
    // error reported in getFileDataFromGitHub
    return null;
  }

  if(sourceFilename) {
    if(!await checkSourceFile(callbacks, inputFilename, sourceFilename)) {
      /* c8 ignore next 3 */
      // error reported in checkSourceFile
      return null;
    }
  }

  return result;
}

async function getFileDataFromGitHub(callbacks: CompilerCallbacks, inputFilename: string, matches: GitHubUrls.GitHubRegexMatchArray): Promise<KmpCompilerFileDataResult> {
  // /^github:(?<name>[a-zA-Z0-9-].+)\/(?<repo>[\w\.-]+)\/raw\/(?<branch>[a-f0-9]{40})\/(?<path>.+)$/
  const githubUrl = `https://github.com/${matches.groups.owner}/${matches.groups.repo}/raw/${matches.groups.branch}/${matches.groups.path}`;
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

async function checkSourceFile(callbacks: CompilerCallbacks, inputFilename: string, sourceFilename: string): Promise<boolean> {
  let stableUrl: string;
  if(FLO_SOURCE.test(sourceFilename)) {
    stableUrl = await getFileStableRefFromFlo(callbacks, sourceFilename);
  } else if(GitHubUrls.GITHUB_URI.test(sourceFilename)) {
    stableUrl = await getFileStableRefFromGitHub(callbacks, sourceFilename);
  } else if(GitHubUrls.GITHUB_RAW_URI.test(sourceFilename)) {
    stableUrl = await getFileStableRefFromGitHub(callbacks, sourceFilename);
  } else {
    callbacks.reportMessage(PackageCompilerMessages.Error_InvalidSourceFileReference({source: sourceFilename, name: inputFilename}));
    return false;
  }

  if(!stableUrl) {
    // getFileStableUrlFromFlo/GitHub return at most a warning, so don't bail
    return true;
  }

  if(stableUrl != inputFilename) {
    callbacks.reportMessage(PackageCompilerMessages.Hint_SourceFileHasChanged({source: sourceFilename, name: inputFilename}));
  }

  return true;
}

let floFamiliesCache: any = undefined; // set to null on error, otherwise an object from JSON

const FLO_FAMILIES_URL = 'https://fonts.languagetechnology.org/families.json';

async function getFloFamilies(callbacks: CompilerCallbacks) {
  if(floFamiliesCache === undefined) {
    try {
      floFamiliesCache = await callbacks.net.fetchJSON(FLO_FAMILIES_URL);
      /* c8 ignore next 12 */
      if(!floFamiliesCache) {
        callbacks.reportMessage(PackageCompilerMessages.Warn_FloDataCouldNotBeRead({url: FLO_FAMILIES_URL}));
        floFamiliesCache = null;
      }
      else if(typeof floFamiliesCache != 'object') {
        callbacks.reportMessage(PackageCompilerMessages.Warn_FloDataIsInvalidFormat({url: FLO_FAMILIES_URL}));
        floFamiliesCache = null;
      }
    } catch(e) {
      callbacks.reportMessage(PackageCompilerMessages.Warn_FloDataCouldNotBeRead({url: FLO_FAMILIES_URL, e}))
      floFamiliesCache = null;
    }
  }
  return floFamiliesCache;
}

async function getFileStableRefFromFlo(callbacks: CompilerCallbacks, floSource: string): Promise<string> {
  const matches = FLO_SOURCE.exec(floSource);
  const floFamilies = await getFloFamilies(callbacks);
  /* c8 ignore next 4 */
  if(!floFamilies) {
    // Error already reported by getFloFamilies
    return null;
  }

  const family = floFamilies[matches.groups.family];
  if(!family) {
    callbacks.reportMessage(PackageCompilerMessages.Warn_FontNotFoundInFlo({filename: floSource, family: matches.groups.family}));
    return null;
  }

  if(!family.distributable) {
    callbacks.reportMessage(PackageCompilerMessages.Warn_FontFromFloIsNotFreelyDistributable(
      {filename: floSource, family: matches.groups.family}));
    // safe to continue
  }

  // TODO: consider .woff, .woff2 for web font inclusion
  const ttf = family.defaults?.ttf;
  if(!ttf) {
    callbacks.reportMessage(PackageCompilerMessages.Warn_FontInFloDoesNotHaveDefaultTtf({filename: floSource, family: matches.groups.family}));
    return null;
  }

  const file = family.files[ttf];
  /* c8 ignore next 4 */
  if(!file) {
    callbacks.reportMessage(PackageCompilerMessages.Warn_FontInFloHasBrokenDefaultTtf({filename: floSource, family: matches.groups.family}));
    return null;
  }

  if(!file.url) {
    callbacks.reportMessage(PackageCompilerMessages.Warn_FontInFloHasNoDownloadAvailable({filename: floSource, family: matches.groups.family}));
    return null;
  }

  // we don't use flourl at this time, becase we want a GitHub reference that
  // can be resolved to a stable URI
  const ghmatches: GitHubUrls.GitHubRegexMatchArray = GitHubUrls.GITHUB_URI.exec(file.url) ?? GitHubUrls.GITHUB_RAW_URI.exec(file.url);
  /* c8 ignore next 4 */
  if(!ghmatches) {
    callbacks.reportMessage(PackageCompilerMessages.Error_FontInFloDoesNotHaveARecognizedGitHubUri({filename: floSource, url: file.url}));
    return null;
  }

  return await resolveGitHubToStableRef(callbacks, ghmatches);
}

async function getFileStableRefFromGitHub(callbacks: CompilerCallbacks, source: string): Promise<string> {
  const matches: GitHubUrls.GitHubRegexMatchArray = GitHubUrls.GITHUB_URI.exec(source) ?? GitHubUrls.GITHUB_RAW_URI.exec(source);
  if(!matches) {
    callbacks.reportMessage(PackageCompilerMessages.Error_UriIsNotARecognizedGitHubUri({url: source}));
    return null;
  }
  return await resolveGitHubToStableRef(callbacks, matches);
}

async function resolveGitHubToStableRef(callbacks: CompilerCallbacks, matches: GitHubUrls.GitHubRegexMatchArray): Promise<string> {
  let commit: any = null;

  const url = `https://api.github.com/repos/${matches.groups.owner}/${matches.groups.repo}/commits/${matches.groups.branch}?path=${matches.groups.path}`;
  try {
    commit = await callbacks.net.fetchJSON(url);
    /* c8 ignore next 8 */
    if(!commit?.sha) {
      callbacks.reportMessage(PackageCompilerMessages.Error_CouldNotRetrieveStableUriFromGitHub({url}));
      return null;
    }
  } catch(e) {
    callbacks.reportMessage(PackageCompilerMessages.Error_CouldNotRetrieveStableUriFromGitHub({url, e}));
    throw e;
  }

  return `https://github.com/${matches.groups.owner}/${matches.groups.repo}/raw/${commit.sha}/${matches.groups.path}`;
}

const GITHUB_STABLE_SOURCE = GitHubUrls.GITHUB_STABLE_SOURCE;

/** @internal */
export const unitTestEndpoints = {
  GITHUB_STABLE_SOURCE,
  getFileDataFromGitHub,
  FLO_SOURCE,
  getFileStableRefFromFlo,
  getFileStableRefFromGitHub,
  getFileDataFromRemote,
  checkSourceFile,
};
