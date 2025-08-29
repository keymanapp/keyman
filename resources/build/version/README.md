# Version increment and automatic history maintenance

This node module automatically refreshes HISTORY.md with titles from the latest
pull requests merged since the last automatic version increment, and if any
history has been found, increments the version, and creates a PR for the
increment.

It is wrappered by ../../teamcity/triggers/trigger-release-builds.sh for normal CI
usage.