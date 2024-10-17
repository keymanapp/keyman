// TODO: refactor out of here
// import https from 'node:https';
import { CompilerCallbacks } from '@keymanapp/developer-utils';
import followRedirects from 'follow-redirects';
const { https } = followRedirects;

const FLO_SOURCE =    /^flo:(?<family>.+)$/;
const GITHUB_SOURCE = /^github:(?<name>[a-zA-Z0-9-].+)\/(?<repo>[\w\.-]+)\/raw\/(?<hash>[a-f0-9]{40})\/(?<path>.+)$/;

export interface KmpCompilerFileDataResult {
  data: Uint8Array;
  basename: string;
};

let floFamilies: any = null;

const FLO_FAMILIES_URL = 'https://fonts.languagetechnology.org/families.json';

async function getFloFamilies() {
  try {
    const data = await httpsGet(FLO_FAMILIES_URL);
    if(!data) {
      // TODO: error
      return null;
    }
    const text = new TextDecoder('utf-8').decode(data);
    return JSON.parse(text);
  } catch(e) {
    // TODO: error
    console.error(e);
    return null;
  }
}

async function getFileDataFromFlo(_callbacks: CompilerCallbacks, _kpsFilename: string, inputFilename: string, matches: RegExpExecArray): Promise<KmpCompilerFileDataResult> {
  if(!floFamilies) {
    floFamilies = await getFloFamilies();
    if(!floFamilies) {
      // TODO: error?
      return null;
    }
    if(typeof floFamilies != 'object') {
      // TODO: error
      console.error(`Data returned from fonts.languagetechnology.org is invalid for ${inputFilename}`);
      return null;
    }
  }

  const family = floFamilies[matches.groups.family];
  if(!family) {
    // TODO: error
    console.error(`Font family '${matches.groups.family}' was not found on fonts.languagetechnology.org for ${inputFilename}`);
    return null;
  }

  if(!family.distributable) {
    // TODO: warn
    console.warn(`Font family '${matches.groups.family}' is not marked as freely distributable on fonts.languagetechnology.org for ${inputFilename}`);
  }

  // TODO: consider .woff, .woff2 for web font inclusion
  const ttf = family.defaults?.ttf;
  if(!ttf) {
    // TODO: error
    console.error(`Font family '${matches.groups.family}' does not have a default .ttf font for ${inputFilename}`);
    return null;
  }

  const file = family.files[ttf];
  if(!file) {
    // TODO: error
    console.error(`Font family '${matches.groups.family}' has an invalid default .ttf font entry. Please report this to fonts.languagetechnology.org for ${inputFilename}`);
    return null;
  }

  if(!file.flourl && !file.url) {
    // TODO: error
    console.error(`Font family '${matches.groups.family}' does not have URLs to download .ttf font for ${inputFilename}.`);
    return null;
  }

  try {
    const data = await httpsGet(file.flourl ?? file.url);
    if(!data) {
      // TODO: error
      console.error(`some error for ${inputFilename}`);
      return null;
    }
    return { data, basename: ttf };
  } catch(e) {
    // TODO: Error
    console.error(e);
    return null;
  }
}

async function getFileDataFromGitHub(callbacks: CompilerCallbacks, _kpsFilename: string, inputFilename: string, matches: RegExpExecArray): Promise<KmpCompilerFileDataResult> {
  // /^github:(?<name>[a-zA-Z0-9-].+)\/(?<repo>[\w\.-]+)\/raw\/(?<hash>[a-f0-9]{40})\/(?<path>.+)$/
  const githubUrl = `https://github.com/${matches.groups.name}/${matches.groups.repo}/raw/${matches.groups.hash}/${matches.groups.path}`;
  try {
    const res = await httpsGet(githubUrl);
    if(!res) {
      console.error(`Expected https get not to return null for ${inputFilename}`);
      return null;
    }
    return { data: res, basename: callbacks.path.basename(matches.groups.path) };
  } catch(e) {
    // TODO: error
    console.error(e);
    return null;
  }
}

async function httpsGet(url: string, postData?: any): Promise<Uint8Array> {
  return new Promise((resolve, reject) => {
    const req = https.request(url, function(res) {
      if(res.statusCode < 200 || res.statusCode >= 300) {
        return reject(new Error(`Status ${res.statusCode}: ${res.statusMessage}`));
      }
      let body: Uint8Array[] = [];
      res.on('data', chunk => body.push(chunk));
      res.on('end', () => {
        resolve(Buffer.concat(body));
      })
    });
    req.on('error', err => reject(err));
    if(postData) {
      req.write(postData);
    }
    req.end();
  });
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
