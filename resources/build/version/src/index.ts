import { info as logInfo } from '@actions/core';
import { GitHub } from '@actions/github';

import { sendCommentToPullRequestAndRelatedIssues, fixupHistory } from './fixupHistory.js';
import { incrementVersion } from './incrementVersion.js';
import yargs from 'yargs';
import { hideBin } from 'yargs/helpers';
import { readFileSync } from 'fs';
import { reportHistory } from './reportHistory.js';

const argv = await yargs(hideBin(process.argv))
  .command(['history'], 'Fixes up HISTORY.md with pull request data')
  .command(['version'], 'Increments the current patch version in VERSION.md')
  .command(['report-history'], 'Print list of outstanding PRs waiting for the next build')
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
    },

    'force': {
      description: 'Force a version increment even if no changes found',
      type: 'boolean'
    },

    'from': {
      description: 'Version tag or commit for starting version to report from (must be on same branch)',
      type: 'string'
    },

    'to': {
      description: 'Version tag or commit for finishing version to report to (must be on same branch)',
      type: 'string'
    },

    'github-pr': {
      description: 'Query GitHub for Pull Request number and title instead of parsing from merge commit comments (not valid with --from, --to)',
      type: 'boolean'
    },

    'write-github-comment': {
      description: 'Write comment to GitHub PRs for all history entries; used only with "history" command',
      type: 'boolean',
      default: true
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
  // Report on upcoming changes
  //

  if(argv._.includes('report-history')) {
    let pulls = await reportHistory(octokit, argv.base, argv.force, argv['github-pr'], argv.from, argv.to);
    let versions: {[index:string]:any} = {};
    pulls.forEach((item) => {
      if(typeof item.version == 'string') {
        if(typeof (versions)[item.version] == 'undefined')  {
          versions[item.version] = {data: item.tag_data, pulls: []};
       }
       // We want to invert the order of the pulls as we go to
       // match what we get when we generate HISTORY.md automatically
       versions[item.version].pulls.unshift(item);
      }
    });
    for(const version of Object.keys(versions)) {
      logInfo(`\n## ${versions[version].data}\n`);
      for(const pull of versions[version].pulls) {
        logInfo(`* ${pull.title} (#${pull.number})`);
      }
    }
    process.exit(0);
  }

  //
  // Add entries to HISTORY.md
  //

  if(argv._.includes('history')) {
    logInfo(`# Validating history for ${version}`);
    changeCount = await fixupHistory(octokit, argv.base, argv.force, argv['write-github-comment'], argv['github-pr'], argv.from, argv.to);
    logInfo(`# ${changeCount} change(s) found for ${version}\n`);
  }

  //
  // Increment the version number if history has any entries
  //

  if(argv._.includes('version') && (changeCount > 0 || argv.force)) {
    logInfo(`# Incrementing version from ${version}`);
    const newVersion = incrementVersion();
    logInfo(`# New version is ${newVersion}\n`);
    process.exit(0); // Tell shell that we have updated files
  }

  process.exit(50); // Tell shell that we haven't updated files
};


main().then(
  ()=>logInfo('Finished')
)
.catch((error: Error): void => {
  console.error(`An unexpected error occurred: ${error.message}, ${error.stack ?? 'no stack trace'}.`);
  process.exit(1);
});
