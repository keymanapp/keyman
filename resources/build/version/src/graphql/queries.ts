export function findLastHistoryPR(base: string) {
  return `
  query FindLastPR {
    search(query: "is:pr is:merged sort:created-desc repo:keymanapp/keyman author:keyman-server label:auto \\"auto: increment\\" base:${base}", last: 1, type: ISSUE) {
      nodes {
        ... on PullRequest {
          number
          mergeCommit {
            oid
          }
        }
      }
    }
  }
`;
};

export const getAssociatedPR = `
  query associatedPRs($sha: String!) {
    repository(name: "keyman", owner: "keymanapp") {
      commit: object(expression: $sha) {
        ... on Commit {
          parents(last: 1) {
            nodes {
              associatedPullRequests(first: 1) {
                nodes {
                  title
                  number
                }
              }
            }
          }
        }
      }
    }
  }
`;
