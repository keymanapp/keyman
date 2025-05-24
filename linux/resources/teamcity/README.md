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
apt update && apt install sudo
export DOCKER_RUNNING=1
```

and then run the build, e.g.

```bash
linux/resources/teamcity/keyman-linux-test.sh configure,build,test
```

NOTE: by default this will run the build as `root` in the container,
so you will end up with files owned by `root` in your tree.
Either create a user with the same user id as your local user in the
container, or use one of the images created by `resources/docker-images`
(though you won't be able to test if all the dependencies get installed
if using the docker-images containers).
