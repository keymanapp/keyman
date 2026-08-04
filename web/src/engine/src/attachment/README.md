## engine/attachment

This submodule defines the behaviors of KeymanWeb's page and element attachment model - that is,
the logic KeymanWeb uses to integrate with its host page and determine which elements are eligible
for use as Keyman-editable context.

Note that this component does _not_ define any related handlers used to hook into the elements,
determine changes in focus, or any other related logic.  This is focused solely on determining
page components are supported for use as input context providers.