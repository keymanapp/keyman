# keyman-system-service tests

This directory defines a keyman-test-service. This is mostly identical to
keyman-system-service. The differences are:

- it runs on the session bus instead of system bus
- KeyboardDevice (which in production interacts with evdev) is mocked
  so that the tests don't require root permissions.

The keyman-test-service gets called/used by the ibus-keyman integration
tests.

This test service goes hand in hand with `linux/ibus-keyman/tests/dbus-test-server`:
during test setup the dbus-test-server gets started (and registers its
class), but also adds our `services` subdir (`linux/keyman-system-service/tests/services`)
to the list of directories to be searched for services.

When during the test `ibus-engine-keyman` calls a method on
`keyman-test-service` it will be started by dbus.
