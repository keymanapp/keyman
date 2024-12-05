/**
 * Matches only a GitHub permanent raw URI with a commit hash, without any other
 * components; note hash is called branch to match other URI formats
 */
export const GITHUB_STABLE_SOURCE = /^https:\/\/github\.com\/(?<owner>[a-zA-Z0-9-]+)\/(?<repo>[\w\.-]+)\/raw\/(?<branch>[a-f0-9]{40})\/(?<path>.+)$/;

/**
 * Matches any GitHub git resource raw 'user content' URI which can be
 * translated to a permanent URI with a commit hash
 */
export const GITHUB_RAW_URI = /^https:\/\/raw\.githubusercontent\.com\/(?<owner>[a-zA-Z0-9-]+)\/(?<repo>[\w\.-]+)\/(?:refs\/(?:heads|tags)\/)?(?<branch>[^/]+)\/(?<path>.+)$/;

/**
 * Matches any GitHub git resource raw URI which can be translated to a
 * permanent URI with a commit hash
 */
export const GITHUB_URI = /^https:\/\/github\.com\/(?<owner>[a-zA-Z0-9-]+)\/(?<repo>[\w\.-]+)\/(?:raw|blob|tree)\/(?:refs\/(?:heads|tags)\/)?(?<branch>[^/]+)\/(?<path>.+)$/;

/**
 * Matches any GitHub git resource raw URI which can be translated to a
 * permanent URI with a commit hash, with the http[s] protocol optional, for
 * matching user-supplied URLs. groups are: `owner`, `repo`, `branch`, and
 * `path`.
 */
export const GITHUB_URI_OPTIONAL_PROTOCOL = /^(?:http(?:s)?:\/\/)?github\.com\/(?<owner>[a-zA-Z0-9-]+)\/(?<repo>[\w\.-]+)(?:\/(?:(?:raw|blob|tree)\/(?:refs\/(?:heads|tags)\/)?(?<branch>[^/]+)\/(?<path>.*))?)?$/;


export interface GitHubRegexMatchArray extends RegExpMatchArray {
  groups?: {
    owner?: string;
    repo?: string;
    branch?: string;
    path?: string;
  }
}