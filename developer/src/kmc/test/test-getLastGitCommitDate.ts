import { assert } from 'chai';
import 'mocha';
import { makePathToFixture } from './helpers/index.js';
import { getLastGitCommitDate } from '../src/util/getLastGitCommitDate.js';

describe('getLastGitCommitDate', function () {
  it('should return a valid date for a folder in this repo', async function() {
    const path = makePathToFixture('.');
    const date = getLastGitCommitDate(path);
    assert.match(date, /^\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\dZ$/);
  });

  it('should return null for a folder outside the repo', async function() {
    const date = getLastGitCommitDate('/');
    assert.isNull(date);
  });
});
