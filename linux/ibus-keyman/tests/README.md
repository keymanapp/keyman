# ibus-keyman integration tests

The ibus-keyman integration tests run the tests defined in `core/tests/unit/kmx`.
The tests run in a separate Xephyr session. Any dconf settings that have to be set during the tests
are stored in `~/.config/glib-2.0/settings/keyfile`.

## Running tests

The tests get run as part of building `ibus-keyman`, more specifically when running `make check`.

### Run all tests

All tests can be run with the test script:

```bash
scripts/run-tests.sh
```

### Run specific tests

To run a single test you pass the testname (as found in
`core/build/arch/*/tests/unit/kmx`). Multiple tests should be separated by space.

```bash
scripts/run-tests.sh -- k_000___null_keyboard k_005___nul_with_initial_context
```
