## Keyman Acceptance Tests

This folder contains acceptance tests for Keyman products. These test procedures should be run, where possible, before moving a product from alpha to beta, or from beta to stable.

A selection of these tests may also be added to pull requests as required regression tests before merging into the master branches.

Each test has three main sections, with step-by-step instructions:

`Setup` – prepares the environment for the test.

`Action` – describes the steps required to perform the test.

`Cleanup` – restores the environment to its original state.

## History
There was for a time period a test team operated external to Keyman development team they used a test suite program to manage and run these acceptance tests.
The test team added to the tests as new features and bug corrections where made to help give good coverage of the products.
These where extracted on the 12th May 2026. Then converted back into the markdown format that matches the Keyman "User Testing" syntax on Github. The have been placed in /docs/tests/acceptance in the keyman github repo.
