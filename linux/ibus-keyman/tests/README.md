# ibus-keyman integration tests

The ibus-keyman integration tests run the tests defined in `common/test/keyboards/baseline`.
The tests run in a separate Xephyr session. Any dconf settings that have to be set during the tests
are stored in `~/.config/glib-2.0/settings/keyfile`.

## Running tests

The tests get run as part of building `ibus-keyman`, more specifically when running `build.sh test`.

### Run all tests

All tests can be run with the test script:

```bash
scripts/run-tests.sh
```

### Run specific tests

To run a single test you pass the testname (as found in
`common/test/keyboards/baseline`). Multiple tests should be separated by space.

```bash
scripts/run-tests.sh -- k_000___null_keyboard k_005___nul_with_initial_context
```

## Debugging tests

This is easiest done by running the tests in a VM and then remote debugging
into the VM. The VM should be the same Linux version as the host, and the
Keyman repo should be shared with the VM through a shared folder.

Then you can start the `run-tests.sh` script on the VM with the
`--remote-debug` option. This will start the background services and then
start a GDB session waiting for the debugger to connect.

For example:

```bash
linux/ibus-keyman/tests/scripts/run-tests.sh --remote-debug \
  --no-surrounding-text --no-wayland -- k_001___basic_input_unicodei
```

On the host, you can then attach to the VM. See the
`Attach to gdbserver (ibus-keyman integration tests)` configuration
in `docs/settings/linux/launch.json` for a sample configuration in
vscode. You'll have to adjust the IP address to match the VM which the
`run-tests.sh` script will output.
