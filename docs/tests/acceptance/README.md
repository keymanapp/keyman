## Keyman Acceptance Tests

This folder contains acceptance tests for Keyman products. These test procedures should be run, where possible, before moving a product from alpha to beta, or from beta to stable.

A selection of these tests may also be added to pull requests as required regression tests before merging into the master branches.

Each test has three main sections, with step-by-step instructions:

`Setup` – prepares the environment for the test.

`Action` – describes the steps required to perform the test.

`Cleanup` – restores the environment to its original state.

## History
For a period of time, a test team operated separately to the Keyman development team. They used a test suite application to manage and run these acceptance tests.

The test team added to the test suite as new features were introduced and bugs were fixed, helping to provide good coverage of the products.

The tests were extracted on the 12th of May 2026 and converted back into Markdown format to match the Keyman User Testing syntax used on GitHub. They have been placed in /docs/tests/acceptance in the `keyman` GitHub repository.
