import { warning as logWarning } from '@actions/core';

import { GitHub } from '@actions/github';

import { findLastHistoryPR, getAssociatedPR} from './graphql/queries';
import { spawnChild } from './util/spawnAwait';

const getPullRequestInformation = async (
  octokit: GitHub, base: string
): Promise<string | undefined> => {
  const response = await octokit.graphql(
    findLastHistoryPR(base)
  );

  if (response === null) {
    return undefined;
  }

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

/**
 * Retrieves any  outstanding pull request titles to HISTORY.md for the
 * current version. Retrieves pull request details from GitHub.
 * @returns array of pull requests
 */

export const reportHistory = async (
  octokit: GitHub, base: string, force: boolean
): Promise<PRInformation[]> => {

  //
  // Get the last auto history merge commit ref
  //

  const commit_id = await getPullRequestInformation(octokit, base);
  if (commit_id === undefined) {
    throw 'Unable to fetch pull request information.';
  }

  //
  // Now, use git log to retrieve list of merge commit refs since then
  //

  const git_result = (await spawnChild('git', ['log', '--merges', /*'--first-parent',*/ '--format=%H', base, `${commit_id}..`])).trim();
  if(git_result.length == 0 && !force) {
    // We won't throw on this
    logWarning('No pull requests found since previous increment');
    return [];
  }

  const new_commits = git_result.split(/\r?\n/g);

  //
  // Retrieve the pull requests associated with each merge
  //

  let pulls: PRInformation[] = [];

  if(git_result.length == 0) {
    pulls.push({
      title: 'No changes made',
      number: 0
    });
  }
  else {
    for(const commit of new_commits) {
      const pr = await getAssociatedPRInformation(octokit, commit);
      if(pr === undefined) {
        logWarning(`commit ref ${commit} has no associated pull request.`);
        continue;
      }
      pulls.push(pr);
    }
  }

  return pulls;
};
