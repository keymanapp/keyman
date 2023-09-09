# Regression Tests

This folder contains tools to run automated regression tests on Keyman Engine for Web
and Keyman Developer. The intent is to validate changes to the compiler and the engine
against the set of available keyboards in the Keyman keyboards repository from
https://github.com/keymanapp/keyboards.

Currently, the regression tests run only on Windows because we have a dependency on
the `kmanalyze` program which is not yet available outside the keyman source repo. This
is the only Windows dependency (the `kmc` compiler is also used).

The test suite runs in Node.js and launches Chrome for the tests. In the future, the
tests may be able to be run in native Node.js.

## Configuration

The test suite expects the keyboards repository to be located as a sibling folder of
the keyman repository, for example:

    C:\Projects\keyman
    - keyboards (keyboards.git)
      - release
        - k
          - khmer_angkor
      - ...
    - apps (keyman.git)
      - windows
        - src
          - ...
      - web
        - src
          - ...
        - testing
          - regression-tests

Keyman Engine for Web, Keyman Developer (kmc, kmanalyze) must be built
prior to running the tests. (If not testing source versions, only kmanalyze is
required). See the build documentation for each project for details; summary below:

To build KeymanWeb (bash):

    cd web/source
    ./build.sh

To build just the Keyman Developer tools required from the Windows source (cmd):

    cd developer\src
    nmake

### Known Failures

As there are some known failures, these are listed in `src/known-failures.js`.
As the bugs that impact these keyboards are addressed, they should be progressively
removed from the file. Keyboards listed in this file will still be tested and
report errors, but they will not fail the test suite overall.

## Running Tests

An interactive or manual test web page is available, as well as the automated
tests that run with node + Karma + Mocha + Chai.

### Manual Tests

`node interactive.js` will start a web server listening on http://localhost:1337/.
Navigating to this address will allow you to load and test a given keyboard from the
keyboards repository. This assumes that the keyboards are already available and that
the .tests have already been built.

This is designed more for validating the testing framework and changes to the testing
tools than for intensive testing.

### Automated Tests

`node test.js` runs the entire test suite. There are a number of command line
parameters available:

    $ node test.js -h
    Usage: test [options]

    Options:
      -V, --version                       output the version number
      -c, --compiler-versions [versions]  Specify compiler version(s) to test. Can specify "stable", "source" or a specific version number. (default: ["stable","source"])
      -e, --engine-versions [versions]    Specify KeymanWeb engine version(s) to test. Can specify "stable", "source" or a specific version number. (default: ["stable","source"])
      -k, --keyboards [keyboards]         Builds and tests specific keyboard source files. If -k is not specified, then test all keyboards in the keyboards repository. (default: [])
      -f, --fail-fast                     Don't attempt to continue tests after the first keyboard test fails
      --deep                              Compare all version combinations against base version, instead of just one; only valid when comparing 1 version of each against base
      --skip-analysis                     Don't create .tests files, assume they are already present
      -l, --log-all-failures              Log all test failures to console, not just the first failure for each keyboard
      -h, --help                          output usage information

When the entire test suite is run, the regression test compares the latest version of
the compiler from the source repository, together with the latest version of Keyman
Engine for Web from the source repository, against the latest stable releases of
each, downloaded from https://downloads.keyman.com/

These two versions are referenced as `source` and `stable` respectively. You can
also specify exact stable, beta or alpha versions for download by specifying a
version number in the `-c` and `-e` parameters. You can test as many versions as
you like at a time, but beware: the number of permutations tested increases as
the square of tested versions.

The continuous integration configuration currently uses `node test.js -l`.

By default, the tests will run through all keyboards, even after a failure, except
for catastrophic errors (such as timeouts, missing test files).

#### Quick run of tests

`node test-builder.js` can be used to re-run a specific test without downloading
tools or recompiling the keyboard. Full command line:

    $ node test-builder.js -h
    Usage: test-builder [options]

    Options:
      -V, --version                     output the version number
      -c, --compiler-version [version]  Specify compiler version that is being tested, which is saved in the results file.
      -e, --engine-version [version]    Specify KeymanWeb engine version that is being tested, which is saved in the results file.
      -k, --keyboards [keyboards]       Test specific keyboard(s) in the repo, e.g. k/khmer_angkor. If -k is not specified,
                                        then test all keyboards in the keyboards repository. (default: [])
      -h, --help                        output usage information

The test builder generates a `tests-generated.js` file for running Karma and then launches Karma with it.

## Background

`kmanalyze` generates a .tests file for a given keyboard, from analysis of a
corresponding .kmx file (a future update will use the intermediate in-memory
.kmx file used by the compiler to ensure we access only the web rules). It
does not currently support deadkeys, recursive groups, or web-specific rules
(lines prefixed with `$keymanweb:`). Nor does it support keyboard or system
options. It will generate a single test for each store referenced in an `any`
statement, rather than a test for each character in the store.

.tests files and .results files are stored in a `tests/` folder for each
keyboard in the repository.

The test suite runs tests against the physical keyboard ruleset for keyboards.
`src/test-runner.js` contains the code for running the individual tests. These
could be extended in future to support touch tests as well as other functionality,
e.g. by parameterising the `inputTests` property of the .tests file.
