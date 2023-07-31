# Keyboard baseline fixtures

This folder contains a set of keyboards that are used by multiple projects to
verify behaviour, both for compilation, and for runtime tests.

The following projects are known to use these keyboards:
* core -- .kmn files only, compiled with kmc during tests
* developer/kmcmplib -- .kmn files, compiled for tests, and .kmx as reference
* linux -- .kmn files for test steps only, .kmx files

The .kmx files were built with kmcomp, not kmc, in order to ensure that
developer/kmcmplib gets a valid baseline reference.

Once kmc is stable, it is possible that we will be able to use the kmc npm
module to build .kmx for all projects that need them.

## Build parameters

Keyboards were built with debug information and no compiler version embedded,
with kmcomp 16.0.138:

```bat
for %d in (*.kmn) do kmcomp -no-compiler-version -d %d
```
