# @keymanapp/developer-utils

This is a helper library for @keymanapp/kmc and other @keymanapp modules,
containing helper functions such as:

* `spawnChild()`
* Sentry integration
* MIT license text validation

It is not intended for separate use.

**Note:** developer/server depends on this module; adding extra dependencies may
impact the bundling for that module too (#10872).