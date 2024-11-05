import { execFileSync } from 'child_process';

// RFC3339 pattern, UTC
export const expectedGitDateFormat = /^\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d(\.\d\d\d)?Z$/;

/**
 * Returns the date and time of the last commit from git for the passed in path
 * @param path   Path for which to retrieve the last commit message
 * @returns string, in RFC3339, 'YYYY-MM-DDThh:nn:ss.SSSZ'
 */
export function getLastGitCommitDate(path: string): string {
  // TZ=UTC0 git log -1 --no-merges --date=format:%Y-%m-%dT%H:%M:%SZ --format=%ad
  let result = callGitLog([
    'log',                                // git log
    '-1',                                 // one commit only
    '--no-merges',                        // we're only interested in 'real' commits
    '--format=%at',                       // emit only the commit date as a UNIX timestamp
    '--',
    path
  ]);

  if(typeof result != 'string') {
    return null;
  }

  if(result == '') {
    // #12626: no history was found, but not an error. We may have only a merge
    // commit in history for that file, allow merges in the log
    result = callGitLog([
      'log',                                // git log
      '-1',                                 // one commit only
      '--format=%at',                       // emit only the commit date as a UNIX timestamp
      '--',
      path
    ]);

    if(typeof result != 'string') {
      return null;
    }
  }

  const sec = Number.parseInt(result);
  if(Number.isNaN(sec)) {
    // We received invalid data, perhaps a git error string so just ignore
    return null;
  }

  // We receive a timestamp in seconds but we need milliseconds
  return new Date(sec * 1000).toISOString();
}

function callGitLog(params: string[]) {
  try {
    const result = execFileSync('git', params, {
      encoding: 'utf-8',                    // force a string result rather than Buffer
      windowsHide: true,                    // on windows, we may need this to suppress a console window popup
      stdio: ['pipe', 'pipe', 'pipe']       // all output via pipe, so we don't get git errors on console
    });
    return result.trim();
  } catch (e) {
    // If git is not available, or the file is not in-repo, then it is probably
    // fine to just silently return null, as the only machines where this is
    // critical are the CI machines where we build and deploy .keyboard_info
    // files, and where git will always be available. It would be possible to
    // have this raise an error in CI environments, but the chance of error
    // seems low.
    return null;
  }
}
