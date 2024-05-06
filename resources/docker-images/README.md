# Container

Docker containers that can be used to build Keyman on the respective
platforms. They contain everything that a CI build agent needs to
build for the platform.

## Prerequisites

You'll need Docker Buildx installed to successfully be able to build the
container images. This is easiest achieved by installing the [official
Docker version](https://docs.docker.com/engine/install/ubuntu/).

Currently it is not possible to use Podman instead of Docker due to a number
of bugs and incompatibilities in the Podman implementation.

## Building the images

To build the docker images:

```shell
resources/docker-images/build.sh
```

By default this will create 64-bit images for building
Keyman for Android, Keyman for Linux and Keyman for Web. These images
are based on the Ubuntu 24.04 with Node 20 and Emscripten
3.1.44 (for the exact versions, see [`minimum-versions.inc.sh`](../build/minimum-versions.inc.sh))
and are named e.g. `keyman-core-ci:default`.

The versions can be changed, e.g.

```shell
resources/docker-images/build.sh --ubuntu-version jammy --node 20
```

This will create an image named e.g. `keyman-core-ci:jammy-node20`.

Once the image is built, it may be used to build parts of Keyman.

## Building locally

It is possible to build locally with these images:

- Keyman Core

  ```shell
  # build 'Keyman Core' in docker
  # keep build artifacts separate
  mkdir -p $(git rev-parse --show-toplevel)/core/build/docker-core
  docker run -it --rm -v $(git rev-parse --show-toplevel):/home/build/build \
    -v $(git rev-parse --show-toplevel)/core/build/docker-core:/home/build/build/core/build \
    keymanapp/keyman-core-ci:default \
    core/build.sh --debug build
  ```

  Note: Since the generated binaries are platform dependent we put them in a container
  specific directory.

- Keyman for Linux

  ```shell
  # build 'Keyman for Linux' installation in docker
  # keep build artifacts separate
  mkdir -p $(git rev-parse --show-toplevel)/linux/build/docker-linux
  docker run -it --rm -v $(git rev-parse --show-toplevel):/home/build/build \
    -v $(git rev-parse --show-toplevel)/linux/build/docker-linux:/home/build/build/linux/build \
    -e DESTDIR=/tmp \
    keymanapp/keyman-linux-ci:default \
    linux/build.sh --debug build install
  ```

  Note: Since the generated binaries are platform dependent we put them in a container
  specific directory.

- Keyman Web

  ```shell
  # build 'Keyman Web' in docker
  docker run --privileged -it --rm \
    -v $(git rev-parse --show-toplevel):/home/build/build \
    keymanapp/keyman-web-ci:default \
    web/build.sh --debug
  ```

- Keyman for Android

  ```shell
  # build 'Keyman for Android' in docker
  docker run -it --rm -v $(git rev-parse --show-toplevel):/home/build/build \
    keymanapp/keyman-android-ci:default \
    android/build.sh --debug
  ```

## Running tests locally

- Keyman Core

  ```shell
  # build 'Keyman Core' in docker
  # keep build artifacts separate
  mkdir -p $(git rev-parse --show-toplevel)/core/build/docker-core
  docker run -it --rm -v $(git rev-parse --show-toplevel):/home/build/build \
    -v $(git rev-parse --show-toplevel)/core/build/docker-core:/home/build/build/core/build \
    keymanapp/keyman-core-ci:default \
    core/build.sh --debug test
  ```

  Note: Since the generated binaries are platform dependent we put them in a container
  specific directory.

- Keyman for Linux

  ```shell
  # build 'Keyman for Linux' installation in docker
  # keep build artifacts separate
  mkdir -p $(git rev-parse --show-toplevel)/linux/build/docker-linux
  docker run --privileged -it --rm -v $(git rev-parse --show-toplevel):/home/build/build \
    -v $(git rev-parse --show-toplevel)/linux/build/docker-linux:/home/build/build/linux/build \
    -e DESTDIR=/tmp \
    keymanapp/keyman-linux-ci:default \
    linux/build.sh --debug test
  ```

  Note: this requires the `--privileged` parameter in order for all tests to pass!

  Note: Since the generated binaries are platform dependent we put them in a container
  specific directory.

- Keyman Web

  ```shell
  # build 'Keyman Web' in docker
  docker run --privileged -it --rm \
    -v $(git rev-parse --show-toplevel):/home/build/build \
    keymanapp/keyman-web-ci:default \
    web/build.sh --debug test
  ```

  Note: this requires the `--privileged` parameter in order for all tests to pass!

- Keyman for Android

  ```shell
  # build 'Keyman for Android' in docker
  docker run -it --rm -v $(git rev-parse --show-toplevel):/home/build/build \
    keymanapp/keyman-android-ci:default \
    android/build.sh --debug test
  ```
