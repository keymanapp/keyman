## engine/dom-utils

This submodule provides a subset of the main engine's Web-oriented code that's used to facilitate DOM-based
layout interactions; it is used in the following scenarios:

1. It is a published as part of our public API on Keyman Engine for Web.
2. It is used for various layout operations within the OSK.
3. It is used by our various desktop UI modules that interface with Keyman Engine for Web.
4. It is used by the element-attachment manager to help determine tab order among attached elements.

Since it is used by multiple different top-level modules, it is best to isolate these functions as their
own standalone module in order to avoid unwanted entanglement among its consumers.