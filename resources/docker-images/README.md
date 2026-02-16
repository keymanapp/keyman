# Container

Docker containers that can be used to build Keyman on the respective
platforms. They contain everything that a CI build agent needs to
build for the platform.

## Prerequisites

You'll need Docker Buildx installed to successfully be able to build the
container images. This is most easily achieved by installing the [official
Docker version](https://docs.docker.com/engine/install/ubuntu/).

Currently it is not possible to use Podman instead of Docker due to a number
of bugs and incompatibilities in the Podman implementation.

## Building the images

To build the docker images:

```shell
resources/docker-images/build.sh
```

By default this will create 64-bit images for building
Keyman Core, Keyman for Android, Keyman for Linux, Keyman for Web
and (cross-platform parts of) Keyman Developer. These images are based
on Ubuntu 24.04 with Node 20 and Emscripten 3.1.58 (for the exact versions,
see [`minimum-versions.inc.sh`](../build/minimum-versions.inc.sh)).
The images are named e.g. `keymanapp/keyman-core-ci:default`.

The versions can be changed, e.g.

```shell
resources/docker-images/build.sh --distro-version jammy --node 20
```

This will create an image named e.g.
`keymanapp/keyman-core-ci:ubuntu-jammy-java21-node20.16.0-emsdk3.1.58`.

Once the image is built, it may be used to build parts of Keyman.

## Building locally

It is possible to build locally with these images:

```shell
# build 'Keyman Core' in docker
resources/docker-images/run.sh :core -- core/build.sh --debug build
```

Note: For Core and Linux we put the generated binaries in a
container specific directory because they are platform dependent.

If you build both with Docker and directly with the build scripts, it is
advisable to run a `git clean -dxf` before switching between the two. The
reason is that the Docker images use a different user, so that paths
will be different.

**Warning:** On Windows, don't mix building in Windows/git-bash and WSL/Docker
without a full clean (`git clean -fdx`) of the repository. Mixed building will
fail for many reasons, including, among others:
* many CLI tools have different names, and references are cached in configure
  steps
* cached paths may be stored with backslashes which work only on Windows
* Keyman Core has varying targets based on the build platform
* npm writes build-platform-specific settings/modules in node_modules
* meson creates build-platform-specific build files

## Running tests locally

To run the tests locally, use the `run.sh` script:

```shell
# Run common/web tests
resources/docker-images/run.sh :web -- common/web/build.sh --debug test
```

## Using a Docker container registry

It is possible to keep the Docker images in a container registry.

The parameters for `run.sh` and `build.sh` are
* `--registry`

  the container registry used to keep the images
* `--username`

  a user name for the container registry
* `--password`

  the user's password

`--username` and `--password` are not required, if the registry can be
accessed without username and password.

The default for the registry is `ghcr.io`.

Building the images for a registry and pushing them to it, looks like:

```shell
resources/docker-images/build.sh --registry 'myregistry:5678' --username 'user' --password 'password' build,publish:
```

Running tests locally with the `run.sh` looks like:

```shell
resources/docker-images/run.sh :web --registry 'myregistry:5678' --username 'user' --password 'password' -- web/build.sh test
```

## Running GitHub Actions locally

There exist [GitHub actions for building the Docker images](../../.github/workflows/build-test-publish-docker.yml).
For testing and developing the workflow can be started locally.
As a prerequisite [act](https://nektosact.com/) must be installed.

This is an exmaple for running the workflow with act:

```bash
#!/bin/bash
# runs GitHub workflow locally for building, testing and publishing Docker images
# requires act (https://nektosact.com/)
# requires a GitHub token with repo permissions (e.g. GITHUB_TOKEN from a personal access token)
# requires Docker with buildx support

tempfile=/tmp/event.json

cat > ${tempfile} <<EOF
{
  "client_payload": {
  },
  "action": "build-test-publish-docker",
  "act": true
}
EOF

act repository_dispatch \
  -e /tmp/event.json \
  --actor='<username>' \
  -s GITHUB_TOKEN='<GitHub token>' \
  --container-options "--user runner:$(getent group docker | cut -d: -f3)" \
   -W '.github/workflows/build-test-publish-docker.yml'

rm -f ${tempfile}
```

## Remote Debugging in Docker Container

It is possible to remote debug in the Docker container by passing `--remote-debug`
to the `run.sh` script.

For example to debug the integration tests in Docker:

* Run Docker container with:

  ```bash
  resources/docker-images/run.sh --remote-debug :linux -- linux/ibus-keyman/tests/scripts/run-tests.sh \
    --no-x11 --remote-debug -- --directory /home/build/.local/share/keyman/test_kmx k_0100___basic_input_unicodei
  ```

* then attach debugger in vscode
