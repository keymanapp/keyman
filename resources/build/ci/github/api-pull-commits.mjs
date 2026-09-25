/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by mcdurdin on 2025-08-28
 *
 * Paginate large commit lists for PRs so we get all build commands
 */
import { Octokit } from "octokit";

if(process.argv.length < 4) {
  console.log(`Usage: node ${process.argv[1]} <GITHUB_TOKEN> <PULL_NUMBER>`);
  process.exit(1);
}

const GITHUB_TOKEN = process.argv[2];
const pull_number = process.argv[3];

const octokit = new Octokit({
  auth: GITHUB_TOKEN
});

const data = await octokit.paginate("GET /repos/{owner}/{repo}/pulls/{pull_number}/commits", {
  owner: "keymanapp",
  repo: "keyman",
  pull_number,
  per_page: 100,
  headers: {
    "X-GitHub-Api-Version": "2022-11-28",
  },
});

process.stdout.write(JSON.stringify(data));
