import {
//    error as logError,
//    info as logInfo,
    warning as logWarning
} from '@actions/core';

import { GitHub } from '@actions/github';

import { findLastHistoryPR, getAssociatedPR} from './graphql/queries';
import { spawnChild } from './util/spawnAwait';
import { readFileSync, writeFileSync } from 'fs';
import { gt } from 'semver';

const getPullRequestInformation = async (
  octokit: GitHub, base: string
): Promise<string | undefined> => {
  const response = await octokit.graphql(
    findLastHistoryPR(base)
  );

  if (response === null) {
    return undefined;
  }

  //logInfo(JSON.stringify(response));

  const {
    search: {
      nodes: [
        {
          mergeCommit: {
            oid: commit_id
          }
        }
      ]
    }
  } = response;

  return commit_id;
};

interface PRInformation {
  title: string;
  number: number;
}

const getAssociatedPRInformation = async (
  octokit: GitHub,
  commit_id: string
): Promise<PRInformation | undefined> => {
  const response = await octokit.graphql(
    getAssociatedPR,
    { sha: commit_id }
  );

  if (response === null || response.repository.commit === null) {
    return undefined;
  }

  //console.log(JSON.stringify(response));

  const {
    repository: {
      commit: {
        parents: {
          nodes: [
            {
              associatedPullRequests: {
                nodes: [
                  {
                    title: title,
                    number: number
                  }
                ]
              }
            }
          ]
        }
      }
    }
  } = response;

  return { title, number };
};

// ------------------------------------------------------------------------------------
// splitPullsIntoHistory
// ------------------------------------------------------------------------------------

const splicePullsIntoHistory = async (pulls: PRInformation[]): Promise<number> => {
  //
  // Get current version and history from VERSION.md and TIER.md
  //

  const version = readFileSync('./VERSION.md', 'utf8').trim();
  const tier = readFileSync('./TIER.md', 'utf8').trim();
  //logInfo(`VERSION="${version}"`);

  //
  // Load and parse history file, looking for our version
  //

  const history: string[] = readFileSync('./HISTORY.md', 'utf8').split(/\r?\n/);
  //logInfo(JSON.stringify(history, null, 2));

  //
  // Split history into entries
  //

  const versionMatch = new RegExp(`^## ${version} ${tier}`, 'i');

  let historyChunks: { heading: string[]; newer: string[]; current: string[]; older: string[] } = {heading:[], newer:[], current:[], older:[]};
  let state = "heading";
  for(const line of history) {
    if(line.match(versionMatch)) {
      // We are in the correct section of history
      state = "current";
    } else {
      let match = line.match(/^\#\# ([0-9.]+)/);
      if(match) {
        state = gt(match[1], version) ? "newer" : "older";
      }
    }
    historyChunks[state].push(line);
  }

  //
  // Add heading if needed
  //

  let changed = false;

  const date = new Date().toISOString().substring(0,10);
  const heading = `## ${version} ${tier} ${date}`;

  if(historyChunks.current.length == 0) {
    changed = true;

    console.log(`-- Adding ${heading}`);
    historyChunks.current.push(heading);
    historyChunks.current.push('');
  }

  //
  // Add each PR that is not already in historyChunks.current to the list
  //

  for(const pull of pulls) {
    const pullNumberRe = new RegExp(`#${pull.number}\\b`);

    if(pull.title.match(/^auto/)) {
      // We always skip pull requests that are
      // automatically generated. At time this is written,
      // that only includes incrementing version. But in
      // future there may be others.
      continue;
    }

    let found = false;
    for(const line of historyChunks.current) {
      if(line.match(pullNumberRe)) {
        found = true;
        break;
      }
    }

    if(!found) {
      const entry = `* ${pull.title} (#${pull.number})`;
      console.log(`-- Adding ${entry}`);
      historyChunks.current.splice(2, 0, entry);
      changed = true;
    }
  }

  if(historyChunks.current.length && historyChunks.current[historyChunks.current.length-1] != '') {
    // Formatting: add blank line at end of section if not there yet.
    historyChunks.current.push('');
    changed = true;
  }

  if(historyChunks.current.length) {
    // We update the heading to refresh the date
    // But we won't assume changes otherwise
    historyChunks.current[0] = heading;
  }

  //
  // If we have changes, reconstruct and save HISTORY.md
  //

  //Debug:
  //console.log(JSON.stringify(historyChunks, null, 2));

  if(changed) {
    console.log('History has changed; writing updated history');
    let newHistory: string = (<string[]>[]).concat(historyChunks.heading, historyChunks.newer, historyChunks.current, historyChunks.older).join('\n');
    writeFileSync('HISTORY.md', newHistory, 'utf8');
  }

  return historyChunks.current.length - 3; // - 3 for header + blanks
}

/**
 * Adds any outstanding pull request titles to HISTORY.md for the current
 * version. Retrieves pull request details from GitHub.
 * @returns number of history entries for the current version,
 *          0 if no pulls associated with the current vesrion, or
 *          -1 on error.
 */

export const fixupHistory = async (
  octokit: GitHub, base: string
): Promise<number> => {

  //
  // Get the last auto history merge commit ref
  //

  const commit_id = await getPullRequestInformation(octokit, base);
  if (commit_id === undefined) {
    logWarning('Unable to fetch pull request information.');
    return -1;
  }

  //
  // Now, use git log to retrieve list of merge commit refs since then
  //

  const git_result = (await spawnChild('git', ['log', '--merges', /*'--first-parent',*/ '--format=%H', base, `${commit_id}..`])).trim();
  if(git_result.length == 0) {
    // We won't throw on this
    logWarning('No pull requests found since previous increment');
    return 0;
  }

  const new_commits = git_result.split(/\r?\n/g);
  //console.log(JSON.stringify(new_commits, null, 2));

  //
  // Retrieve the pull requests associated with each merge
  //

  let pulls: PRInformation[] = [];

  for(const commit of new_commits) {
    const pr = await getAssociatedPRInformation(octokit, commit);
    if(pr === undefined) {
      logWarning(`commit ref ${commit} has no associated pull request.`);
      continue;
    }
    pulls.push(pr);
  }

  //logInfo(JSON.stringify(pulls, null, 2));

  //
  // Splice these into HISTORY.md
  //

  const changeCount = await splicePullsIntoHistory(pulls);

  return changeCount;
};
