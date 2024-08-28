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
Keyman Core, Keyman for Android, Keyman for Linux and
Keyman for Web. These images are based on the Ubuntu 24.04
with Node 20 and Emscripten 3.1.58 (for the exact versions,
see [`minimum-versions.inc.sh`](../build/minimum-versions.inc.sh))
and are named e.g. `keyman-core-ci:default`.

The versions can be changed, e.g.

```shell
resources/docker-images/build.sh --ubuntu-version jammy --node 20
```

This will create an image named e.g. `keyman-core-ci:jammy-node20`.

Once the image is built, it may be used to build parts of Keyman.

## Building locally

It is possible to build locally with these images:

```shell
# build 'Keyman Core' in docker
resources/docker-images/run.sh core -- core/build.sh --debug build
```

Note: For Core and Linux we put the generated binaries in a
container specific directory because they are platform dependent.

## Running tests locally

To run the tests locally, use the `run.sh` script:

```shell
# Run common/web tests
resources/docker-images/run.sh web -- common/web/build.sh --debug test
```
