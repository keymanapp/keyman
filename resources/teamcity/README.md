# Teamcity Build Configuration Scripts

This directory contains TeamCity (TC) build scripts. Each script corresponds
to a build configuration on TC.

## Running scripts locally

It's possible to run some of the build locally in a Docker container.
Of course this won't work for the parts, like uploading files, that
require keys or other secrets.

To run locally start a Docker container:

```bash
docker run -v $(pwd):/Develop -it ubuntu:24.04 /bin/bash
```

(for the Linux integration tests, you'll have to pass `--privileged` as well.)

Inside of the container, run

```bash
apt update && apt install -y sudo
export DOCKER_RUNNING=1
echo "ubuntu ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
su ubuntu
```

and then run the build, e.g.

```bash
cd /Develop
resources/teamcity/keyman-linux-test.sh configure,build,test
```
