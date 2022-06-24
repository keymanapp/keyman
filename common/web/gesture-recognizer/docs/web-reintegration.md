# Web Re-integration Guide

As this is based on refactored KeymanWeb 15.0 code, there will eventually come a time to integrate this
module into KeymanWeb and replace the old version.  As there has been some refactoring, this document
will list notable changes to facilitate reintegration once this module is ready.

## From feat/web/gesture-recognizer-main

The OSK helper functions for `MouseEventEngine` and for `TouchEventEngine` have been disabled, as they
are references to components external to this module.

## From feat/web/gesture-recognizer-base

Starts work on a unified mouse+touch object - `GestureRecognizer` - and starts work on the unified
config object.

Rather than specifying callback methods, we also shift to use of `EventEmitter` and an event-based model.
At this stage, the event parameters should be relatively easy to interpret and forward to the original methods.

