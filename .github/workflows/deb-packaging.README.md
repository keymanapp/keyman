# Debian Packaging GitHub Action

You can manually trigger a deb-packaging action on GitHub, e.g. to test changes:

- push the changes to your fork of keymanapp/keyman: git push myfork HEAD:master
- make sure you have GHA enabled in the settings of your fork and created an
  access token on GitHub
- trigger a build with:

  ```bash
   curl --write-out '\n' --request POST \
    --header "Accept: application/vnd.github+json" \
    --header "Authorization: token $GITHUB_TOKEN" \
    --data "{ \"event_type\": \"deb-pr-packaging: master\", \
    \"client_payload\": { \
      \"buildSha\": \"$(git rev-parse refs/heads/master)\", \
      \"headSha\": \"$(git rev-parse refs/heads/master)\", \
      \"branch\": \"master\", \
      \"isTestBuild\": \"true\"}" \
    https://api.github.com/repos/<yourgithubname>/keyman/dispatches

  ```
