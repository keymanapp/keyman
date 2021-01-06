import { info as logInfo } from '@actions/core';
import { GitHub } from '@actions/github';

import { sendCommentToPullRequestAndRelatedIssues, fixupHistory } from './fixupHistory';
import { incrementVersion } from './incrementVersion';
const yargs = require('yargs');
import { readFileSync } from 'fs';

const argv = yargs
  .command(['history'], 'Fixes up HISTORY.md with pull request data')
  .command(['version'], 'Increments the current ptach version in VERSION.md')
  .demandCommand(1, 2)
  .options({
    'base': {
      description: 'Base branch',
      alias: 'b',
      default: 'master'
    },

    'token': {
      description: 'GitHub oauth token',
      demandOption: true,
      alias: 't',
      type: 'string'
    }
  })
  .help()
  .alias('help', 'h')
  .argv;

const main = async (): Promise<void> => {

  const octokit: GitHub = new GitHub(argv.token);

  // Pretend we have a single change. If we chain commands (as in normal usage),
  // then we use the real history change count to determine if we continue.
  let changeCount = 1;

  const version = readFileSync('./VERSION.md', 'utf8').trim();

  //
  // Test
  //
  if(argv._.includes('test-current-pulls')) {
    logInfo(`# Doing a test run for ${version} against PR #881`);
    await sendCommentToPullRequestAndRelatedIssues(octokit, [881]);
    process.exit(0);
  }

  //
  // Add entries to HISTORY.md
  //

  if(argv._.includes('history')) {
    logInfo(`# Validating history for ${version}`);
    changeCount = await fixupHistory(octokit, argv.base);
    logInfo(`# ${changeCount} change(s) found for ${version}\n`);
  }

  //
  // Increment the version number if history has any entries
  //

  if(argv._.includes('version') && changeCount > 0) {
    logInfo(`# Incrementing version from ${version}`);
    const newVersion = incrementVersion();
    logInfo(`# New version is ${newVersion}\n`);
    process.exit(0); // Tell shell that we have updated files
  }

  process.exit(1); // Tell shell that we haven't updated files
};


main().then(
  ()=>logInfo('Finished')
)
.catch((error: Error): void => {
  console.error(`An unexpected error occurred: ${error.message}, ${error.stack ?? 'no stack trace'}.`);
  process.exit(2);
});
