#!/bin/bash
# runs GitHub workflow locally for building, testing and publishing Docker images
# requires act (https://nektosact.com/)
# requires a GitHub token with repo permissions (e.g. GITHUB_TOKEN from a personal access token)
# requires Docker with buildx support

tempfile=/tmp/event.json

trap "rm -f \"${tempfile}\"" EXIT

cat > ${tempfile} <<EOF
{
  "repository_dispatch": {
    "event_name": "repository_dispatch",
    "event_type": "build-test-publish-docker",
    "action": "build-test-publish-docker",
    "ref_name": "master"
  }
}
EOF

act repository_dispatch \
  -e /tmp/event.json \
  --actor <GitHub username> \
  -s GITHUB_TOKEN=<GitHub token> \
  --container-options "--user runner:$(getent group docker | cut -d: -f3)"

rm -f ${tempfile}
