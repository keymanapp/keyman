import { execFileSync } from 'child_process';

// RFC3339 pattern, UTC
export const expectedGitDateFormat = /^\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\dZ$/;

/**
 * Returns the date and time of the last commit from git for the passed in path
 * @param path   Path for which to retrieve the last commit message
 * @returns string, in RFC3339, 'YYYY-MM-DDThh:nn:ssZ'
 */
export function getLastGitCommitDate(path: string): string {
  // TZ=UTC0 git log -1 --no-merges --date=format:%Y-%m-%dT%H:%M:%SZ --format=%ad
  let result = null;

  try {
    result = execFileSync('git', [
      'log',                                // git log
      '-1',                                 // one commit only
      '--no-merges',                        // we're only interested in 'real' commits
      '--date=format:%Y-%m-%dT%H:%M:%SZ',   // format the date in our expected RFC3339 format
      '--format=%ad'                        // emit only the commit date
    ], {
      env: { ...process.env, TZ: 'TZ0' },   // use UTC timezone, not local
      encoding: 'utf-8',                    // force a string result rather than Buffer
      windowsHide: true,                    // on windows, we may need this to suppress a console window popup
      cwd: path,                            // path to run git from
      stdio: ['pipe', 'pipe', 'pipe']       // all output via pipe, so we don't get git errors on console
    });
  } catch (e) {
    // If git is not available, or the file is not in-repo, then it is probably
    // fine to just silently return null, as the only machines where this is
    // critical are the CI machines where we build and deploy .keyboard_info
    // files, and where git will always be available. It would be possible to
    // have this raise an error in CI environments, but the chance of error
    // seems low.
    return null;
  }

  result = result.trim();

  // We'll only return the result if it walks like a date, swims like a date,
  // and quacks like a date.
  if (!result.match(expectedGitDateFormat)) {
    return null;
  }

  return result;
}
