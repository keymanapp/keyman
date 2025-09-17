import * as fs from 'node:fs';
import 'mocha';
import { assert } from 'chai';
import sinon from 'sinon';

import { unitTestEndpoints } from './pr-build-status.mjs';

// debug only
import { getOctokit } from '@actions/github';
import * as process from 'node:process';

const commits = {
  '390a5bc6dba73cd32bc8c7290ac73321f6504deb': [ 'failure', '3 checks failed ❌; 21 checks completed successfully ✅'],
  '825f7d2df0069fa56a8f2a534b42a49ee1f21ca1': [ 'success', '24 checks completed successfully ✅' ],
  '870d37f56b489ef5067b4352a06505369d9038e0': [ 'error', '6 checks in an error state ❌; 1 check failed ❌; 17 checks completed successfully ✅' ],
  '11a558e926370253db37026c95b7ff5a6aa1e8fc': [ 'failure', '2 checks failed ❌; 22 checks completed successfully ✅' ],
  '8fd5ccc9026b85d6a780d8bcf862380e7a89aefb': [ 'pending', 'Builds have not yet been triggered ⌛' ],
  '153683cfb007c6066c9dcf71d25afa4c66efa17f': [ 'pending', 'Builds have not yet been triggered ⌛' ],
};

// When adding new SHAs to test, run this to collect data from GitHub
const debug = false;
if(debug) {

  async function writeTestFile(sha) {
    fs.writeFileSync('fixtures/' + sha + '-statuses.json',
      JSON.stringify(await unitTestEndpoints.getCommitStatuses(octokit, 'keymanapp', 'keyman', sha), null, 2), 'utf-8');
    fs.writeFileSync('fixtures/' + sha + '-check-runs.json',
      JSON.stringify(await unitTestEndpoints.getCommitCheckRuns(octokit, 'keymanapp', 'keyman', sha), null, 2), 'utf-8');
  }

  const octokit = getOctokit(process.env['GITHUB_TOKEN']);
  for(const sha of Object.keys(commits)) await writeTestFile(sha);
}

describe('pr-build-status', function () {
  beforeEach(() => {
    // just silencing the logging from pr-build-status bot
    // in order to make the test results clearer
    sinon.stub(console, 'log');
  });

  afterEach(() => {
    console.log.restore();
  });

  for(const sha of Object.keys(commits)) {
    it(`should return '${commits[sha][1]}' for sha ${sha}`, function () {
      const json = JSON.parse(fs.readFileSync(`fixtures/${sha}-statuses.json`,'utf-8'));
      const status = unitTestEndpoints.calculateCheckResult(json);
      assert.isNotNull(status);
      assert.isArray(status);
      assert.deepStrictEqual(status.splice(0, 2), commits[sha]);
    });
  }
});
