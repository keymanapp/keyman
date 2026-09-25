import { execFileSync } from 'node:child_process';
import { assert } from 'chai';
import 'mocha';
import { makePathToFixture } from './helpers/index.js';
import { expectedGitDateFormat, getLastGitCommitDate } from '../src/util/getLastGitCommitDate.js';

describe('getLastGitCommitDate', function () {
  it('should return a valid date for a folder in this repo', async function() {
    this.timeout(5000); // getLastGitCommitDate depends on git which can sometimes take longer
    const path = makePathToFixture('.');
    const date = getLastGitCommitDate(path);
    assert.match(date, expectedGitDateFormat);
  });

  it('should return null for a folder outside the repo', async function() {
    this.timeout(5000); // getLastGitCommitDate depends on git which can sometimes take longer
    const date = getLastGitCommitDate('/');
    assert.isNull(date);
  });

  function isShallowRepository() {
    try {
      const result = execFileSync('git', ['rev-parse', '--is-shallow-repository'], {
        encoding: 'utf-8',                    // force a string result rather than Buffer
        windowsHide: true,                    // on windows, we may need this to suppress a console window popup
        stdio: ['pipe', 'pipe', 'pipe']       // all output via pipe, so we don't get git errors on console
      });
      return result.trim() == 'true';
    } catch (e) {
      return false;
    }
  }

  it('should return a valid date for a specific file in the repo', async function() {
    this.timeout(5000); // getLastGitCommitDate depends on git which can sometimes take longer

    if(isShallowRepository()) {
      // If we have a shallow clone, then the date will be the most recent
      // commit date, and we'll just skip this test. For example, this happens
      // on GHA checkouts by default with actions/checkout depth=1
      this.skip();
    }

    const path = makePathToFixture('get-last-git-commit-date/README.md');
    const date = getLastGitCommitDate(path);
    // The expected date was manually extracted using the following command, with msec appended:
    //   TZ=UTC git log --date=iso-strict-local -- fixtures/get-last-git-commit-date/README.md
    // If the fixture modified, then this will also need to be updated.
    assert.equal(date, '2024-06-17T20:43:48.000Z');
  });
});
