## engine/element-wrappers

This submodule provides a subset of the main engine's Web-oriented code that's used to 'wrap' webpage
elements as part of KMW attachment and interface the element with the `keyboard-processor` submodule.

At this time, said code is included as part of KMW's main source.  It is provided here as a
separate module for use in unit tests, though it is 100% engine code.  It is not currently practical
to 100% modularize it in KMW's current state due to cross-references.