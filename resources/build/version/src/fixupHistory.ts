import {
    warning as logWarning
} from '@actions/core';

import { GitHub } from '@actions/github';
import { readFileSync, writeFileSync } from 'fs';
import { gt } from 'semver';
import { reportHistory } from './reportHistory.js';
import { spawnChild } from './util/spawnAwait.js';

interface PRInformation {
  title: string;
  number: number;
}

// ------------------------------------------------------------------------------------
// splitPullsIntoHistory
// ------------------------------------------------------------------------------------

const splicePullsIntoHistory = async (pulls: PRInformation[], base?: string): Promise<{count: number, pulls: number[]}> => {

  let currentPulls: number[] = [];

  //
  // Get current version and history from VERSION.md and TIER.md. This may not
  // yet be committed, so read the details from the worktree.
  //

  let version = readFileSync('./VERSION.md', 'utf8').trim();
  let tier = readFileSync('./TIER.md', 'utf8').trim();

  if(base) {
    // If we are merging history from another branch, we need to use the data
    // from that branch. In this case, we must assume that the version and tier
    // data are definitely already committed.
    const currentBase = (await spawnChild('git', ['branch', '--show-current'])).trim();
    if(currentBase != base) {
      version = (await spawnChild('git', ['show', base+':VERSION.md'])).trim();
      tier = (await spawnChild('git', ['show', base+':TIER.md'])).trim();
    }
  }
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
  type ChunkType = keyof typeof historyChunks;
  let state: ChunkType = "heading";
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
    // Look for the PR in any other chunk -- can be found with merges of master into PR or chained PRs
    for(const chunk of ['newer','current','older'] as ChunkType[]) {
      for(const line of historyChunks[chunk]) {
        if(line.match(pullNumberRe)) {
          found = true;
          break;
        }
      }
    }

    if(!found) {
      const entry = (pull.number == 0) ? `* ${pull.title}` : `* ${pull.title} (#${pull.number})`;
      console.log(`-- Adding ${entry}`);
      historyChunks.current.splice(2, 0, entry);
      currentPulls.push(pull.number);
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

  return {count: historyChunks.current.length - 3, pulls: currentPulls}; // - 3 for header + blanks
}

/**
 * Creates a comment on each pull request found to point users to
 * a relevant build.
*/
export const sendCommentToPullRequestAndRelatedIssues = async (
  octokit: GitHub,
  pulls: number[]
): Promise<any> => {

  const tier = readFileSync('./TIER.md', 'utf8').trim();
  const version = readFileSync('./VERSION.md', 'utf8').trim();
  const versionTier = version + (tier == 'stable' ? '' : '-'+tier);

  const messagePull = `Changes in this pull request will be available for download in [Keyman version ${versionTier}](https://keyman.com/downloads/releases/${tier}/${version})`;

  // Potentially creating many comments, we need to respect GitHub rate limits
  // https://docs.github.com/en/rest/guides/best-practices-for-integrators#dealing-with-abuse-rate-limits
  let result: Promise<any> = Promise.resolve();
  pulls.forEach(pull => {

    result = result
      // At least 1 second delay between requests; we'll do 10 seconds
      .then(() => new Promise(resolve => setTimeout(resolve, 10000)))
      // Always serially rather than in parallel
      .then(() => {
        console.log(`Creating comment on Pull Request #${pull}`);
        return octokit.issues.createComment({
          owner: 'keymanapp',
          repo: 'keyman',
          issue_number: pull,
          body: messagePull,
        })
      });
  });

  return result;
}

/**
 * Adds any outstanding pull request titles to HISTORY.md for the current
 * version. Retrieves pull request details from GitHub.
 * @returns number of history entries for the current version,
 *          0 if no pulls associated with the current version, or
 *          -1 on error.
 */

export const fixupHistory = async (
  octokit: GitHub, base: string, force: boolean, writeGithubComment: boolean, useGitHubPRInfo: boolean, from?: string, to?: string
): Promise<number> => {

  //
  // Get pull request details
  //

  let pulls: PRInformation[] = [];

  try {
    pulls = await reportHistory(octokit, base, force, useGitHubPRInfo, from, to);
  } catch(e) {
    logWarning(String(e));
    return -1;
  }

  if(pulls == null) {
    return -1;
  }

  //
  // Splice these into HISTORY.md
  //

  const historyResult = await splicePullsIntoHistory(pulls, base);

  //
  // Write a comment to GitHub for each of the pulls
  //

  if(writeGithubComment && historyResult.pulls.length > 0) {
    await sendCommentToPullRequestAndRelatedIssues(octokit, historyResult.pulls);
  }

  return historyResult.count;
};
