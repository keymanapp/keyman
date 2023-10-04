import { warning as logWarning, info as logInfo } from '@actions/core';

import { GitHub } from '@actions/github';

import { findLastHistoryPR, getAssociatedPR} from './graphql/queries.js';
import { spawnChild } from './util/spawnAwait.js';

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
  }: any = response;

  return commit_id;
};

interface PRInformation {
  title: string;
  number: number;
  version?: string;
  tag_data?: string;
}

const getAssociatedPRInformation = async (
  octokit: GitHub,
  commit_id: string
): Promise<PRInformation | undefined> => {
  const response = await octokit.graphql(
    getAssociatedPR,
    { sha: commit_id }
  );

  if (response === null || (<any>response).repository.commit === null) {
    return undefined;
  }

  const {
    repository: {
      commit: {
        parents: {
          nodes: [
            {
              associatedPullRequests: {
                nodes: nodes
              }
            }
          ]
        }
      }
    }
  }: any = response;

  const node = nodes.find((node:any) => node.state == 'MERGED');
  return node ? { title: node.title, number: node.number } : undefined;
};

/**
 * Retrieves any  outstanding pull request titles to HISTORY.md for the
 * current version. Retrieves pull request details from GitHub.
 * @returns array of pull requests
 */

export const reportHistory = async (
  octokit: GitHub, base: string, force: boolean, useGitHubPRInfo: boolean,
  fromVersion?: string, toVersion?: string
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

  let args=['log', '--merges', '--first-parent', '--format=%H'];
  if(fromVersion != undefined && toVersion != undefined) {
    args.push(fromVersion + '..' + toVersion);
  } else {
    args.push(`origin/${base}`, `${commit_id}..origin/${base}`);
  }
  const git_result = (await spawnChild('git', args)).trim();
  if(git_result.length == 0 && !force) {
    // We won't throw on this
    logWarning('No pull requests found since previous increment');
    return [];
  }

  logInfo('Found commits for '+JSON.stringify(args)+': \n'+git_result);

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
  } else {
    let git_tag = 'Next Version', git_tag_data = 'Next Version';
    const re = /#(\d+)/;
    const emojiRE = /[\u{1f300}-\u{1f5ff}\u{1f900}-\u{1f9ff}\u{1f600}-\u{1f64f}\u{1f680}-\u{1f6ff}\u{2600}-\u{26ff}\u{2700}-\u{27bf}\u{1f1e6}-\u{1f1ff}\u{1f191}-\u{1f251}\u{1f004}\u{1f0cf}\u{1f170}-\u{1f171}\u{1f17e}-\u{1f17f}\u{1f18e}\u{3030}\u{2b50}\u{2b55}\u{2934}-\u{2935}\u{2b05}-\u{2b07}\u{2b1b}-\u{2b1c}\u{3297}\u{3299}\u{303d}\u{00a9}\u{00ae}\u{2122}\u{23f3}\u{24c2}\u{23e9}-\u{23ef}\u{25b6}\u{23f8}-\u{23fa}]/gu;
    for(const commit of new_commits) {
      if(!useGitHubPRInfo) {
        const git_pr_title = (await spawnChild('git', ['log', '--format=%b', '-n', '1', commit])).replace(emojiRE, ' ').trim();
        if(git_pr_title.match(/^auto\:/)) continue;
        const git_pr_data = (await spawnChild('git', ['log', '--format=%s', '-n', '1', commit])).trim();
        const this_git_tag = (await spawnChild('git', ['tag', '--points-at', commit])).trim();
        const e = re.exec(git_pr_data);
        if(e) {
          if(this_git_tag != '') {
            const this_git_date = (await spawnChild('git', ['log', '--format=%cs','-n', '1', this_git_tag])).trim();
            // Transform the tag into our regular HISTORY.md format
            // Note that we switched from release-x.y.z to release@x.y.z around 17.0.30-alpha
            const tag_format = /^release[-@](\d+\.\d+\.\d+)(-(.+))?$/.exec(this_git_tag);
            if(tag_format) {
              git_tag_data = tag_format[1] + ' ' + (tag_format[3] == null ? 'stable' : tag_format[3]) + ' ' + this_git_date;
            }
            git_tag = this_git_tag;
          }

          const pr: PRInformation = {
            title: git_pr_title,
            number: parseInt(e[1], 10),
            version: git_tag,
            tag_data: git_tag_data
          };
          if(pulls.find(p => p.number == pr.number) == undefined) {
            pulls.push(pr);
          }
        }
      } else {
        const pr = await getAssociatedPRInformation(octokit, commit);
        if(pr === undefined) {
          logWarning(`commit ref ${commit} has no associated pull request.`);
          continue;
        }
        if(pulls.find(p => p.number == pr.number) == undefined) {
          pr.tag_data = git_tag_data;
          pr.version = git_tag;
          pr.title = pr.title.replace(emojiRE, ' ').trim();
          pulls.push(pr);
        }
      }
    }
  }
  return pulls;
};
