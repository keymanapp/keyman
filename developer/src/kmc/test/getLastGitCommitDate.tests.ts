import { assert } from 'chai';
import 'mocha';
import { makePathToFixture } from './helpers/index.js';
import { expectedGitDateFormat, getLastGitCommitDate } from '../src/util/getLastGitCommitDate.js';

describe('getLastGitCommitDate', function () {
  it('should return a valid date for a folder in this repo', async function() {
    const path = makePathToFixture('.');
    const date = getLastGitCommitDate(path);
    assert.match(date, expectedGitDateFormat);
  });

  it('should return null for a folder outside the repo', async function() {
    const date = getLastGitCommitDate('/');
    assert.isNull(date);
  });

  it('should return a valid date for a specific file in the repo', async function() {
    const path = makePathToFixture('get-last-git-commit-date/README.md');
    const date = getLastGitCommitDate(path);
    // The expected date was manually extracted using the following command, with msec appended:
    //   TZ=UTC git log --date=iso-strict-local -- fixtures/get-last-git-commit-date/README.md
    // If the fixture modified, then this will also need to be updated.
    assert.equal(date, '2024-06-17T20:43:48.000Z');
  });
});
