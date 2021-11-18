# Setup your Keyman build environment on Windows

TODO

* [interim notes](../../windows/src/README.md)

# Notes for Contributors

When cloning this repo for local development on a Windows machine, take care not to place it overly deep in your file system.
Some of the paths for compilation can push character lengths around 160 characters long, while certain operations on Windows systems may be limited to paths of 260 characters or less.
For example, [`git clean` on Windows with msys](https://stackoverflow.com/questions/22575662/filename-too-long-in-git-for-windows/22575737#22575737) is limited due to dependence on older Windows APIs.
