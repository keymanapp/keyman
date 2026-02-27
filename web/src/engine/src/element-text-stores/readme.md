## engine/element-text-stores

This submodule provides a subset of the main engine's Web-oriented code that's used to 'wrap' webpage
elements as part of KMW attachment and interface the element with the `keyboard` submodule.


Please keep any code in this folder / namespace as free as possible from dependencies on other parts of KMW.  Some of our unit tests wish to run against these types without requiring KMW to be active.

Note that with a little work, we _could_ completely spin this into its own separate module - this could be useful for development and testing purposes, especially now that we've dropped the old `TouchAlias` type that was a bit entangled with main engine code.