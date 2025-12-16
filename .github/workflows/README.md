# api-verification.yml
TODO

# auto-merge-keyman-server-pr.yml
TODO

# build-test-publish-docker.yml
TODO

# cleanup-ghcr.yml
TODO

# close-linked-issues-for-merged-prs.yml
TODO

# core-arm64-windows-test.yml: Test Keyman Core on Windows/ARM64

This action allows us to test Keyman Core for Windows on ARM64 architecture with
a GitHub agent, as we do not have any TeamCity Windows build agents with ARM64
arch.

It will normally be triggered as part of a test build from the
resources/teamcity/triggers/trigger-test-builds.sh script.

You can manually trigger a core-arm64-windows-test action on GitHub on your fork
of keymanapp/keyman. This is normally only needed to test changes to the action
itself:

- push the changes to your fork of keymanapp/keyman: `git push myfork HEAD:master`
- make sure you have GHA enabled in the settings of your fork
- seet `GITHUB_TOKEN=<your_gh_token>`
- trigger a build with:

  ```bash
   curl --write-out '\n' --request POST \
    --header "Accept: application/vnd.github+json" \
    --header "Authorization: token $GITHUB_TOKEN" \
    --data "{ \"event_type\": \"core-arm64-windows-test: master\", \
    \"client_payload\": { \
      \"buildSha\": \"$(git rev-parse refs/heads/master)\", \
      \"branch\": \"master\", \
      \"user\": \"${USER}\", \
      \"isTestBuild\": \"true\", \
      \"force\": \"true\"}" \
    https://api.github.com/repos/<yourgithubname>/keyman/dispatches
  ```

  To trigger a build for PR #1234 in a branch `my-pr-branch` this would look similar
  to this:

  ```bash
   curl --write-out '\n' --request POST \
    --header "Accept: application/vnd.github+json" \
    --header "Authorization: token $GITHUB_TOKEN" \
    --data "{ \"event_type\": \"core-arm64-windows-test: PR #1234\", \
    \"client_payload\": { \
      \"buildSha\": \"$(git rev-parse refs/heads/my-pr-branch)\", \
      \"branch\": \"my-pr-branch\", \
      \"user\": \"${USER}\", \
      \"isTestBuild\": \"true\", \
      \"force\": \"true\"}" \
    https://api.github.com/repos/<yourgithubname>/keyman/dispatches
  ```

....

# crowdin.yml
TODO

# deb-packaging.yml: Ubuntu Packaging GitHub Action

You can manually trigger a deb-packaging action on GitHub on your fork
of keymanapp/keyman. This is normally only needed to test changes to the action
itself:

- push the changes to your fork of keymanapp/keyman: `git push myfork HEAD:master`
- make sure you have GHA enabled in the settings of your fork
- seet `GITHUB_TOKEN=<your_gh_token>`
- trigger a build with:

  ```bash
   curl --write-out '\n' --request POST \
    --header "Accept: application/vnd.github+json" \
    --header "Authorization: token $GITHUB_TOKEN" \
    --data "{ \"event_type\": \"deb-pr-packaging: master\", \
    \"client_payload\": { \
      \"buildSha\": \"$(git rev-parse refs/heads/master)\", \
      \"branch\": \"master\", \
      \"baseBranch\": \"master\", \
      \"baseRef\": \"$(git rev-parse refs/heads/master^)\", \
      \"user\": \"${USER}\", \
      \"isTestBuild\": \"true\", \
      \"force\": \"true\"}" \
    https://api.github.com/repos/<yourgithubname>/keyman/dispatches
  ```

  To trigger a build for PR #1234 in a branch `pr-1234` this would look similar
  to this:

  ```bash
   curl --write-out '\n' --request POST \
    --header "Accept: application/vnd.github+json" \
    --header "Authorization: token $GITHUB_TOKEN" \
    --data "{ \"event_type\": \"deb-pr-packaging: PR #1234\", \
    \"client_payload\": { \
      \"buildSha\": \"$(git rev-parse refs/heads/pr-1234)\", \
      \"branch\": \"pr-1234\", \
      \"baseBranch\": \"master\", \
      \"baseRef\": \"$(git rev-parse refs/heads/master)\", \
      \"user\": \"${USER}\", \
      \"isTestBuild\": \"true\", \
      \"force\": \"true\"}" \
    https://api.github.com/repos/<yourgithubname>/keyman/dispatches
  ```

# labeler.yml
TODO

# npm-publish.yml
TODO

# pr-build-status.yml
TODO
