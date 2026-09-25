# Release builds

A release is triggered nightly whenever a pull request has been merged to the
master (alpha) or beta branches; it runs from a TeamCity build configuration
that runs `trigger-release-builds.sh` and then uploads the resulting
`HISTORY.md` to <https://downloads.keyman.com/history/HISTORY.md>.

For stable releases, these are always manually triggered in TeamCity.

To manually make a release (for example if a build falls over for a transient error or
there was a build configuration problem), create a new PR that targets the branch,
merge it, and then run the **Trigger Release Builds** build configuration for that
branch. If you have no changes, see that as an opportunity to improve documentation,
including this file...

# Test builds

Test builds are triggered for any commit to a PR, via `trigger-test-builds.sh`.