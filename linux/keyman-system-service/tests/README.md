# keyman-system-service tests

This directory defines a keyman-test-service. This is mostly identical to
keyman-system-service. The differences are:

- it runs on the session bus instead of system bus
- KeyboardDevice (which in production interacts with evdev) is mocked
  so that the tests don't require root permissions.

The keyman-test-service gets called/used by the ibus-keyman integration
tests.
