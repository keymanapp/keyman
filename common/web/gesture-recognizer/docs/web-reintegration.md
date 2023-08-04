# Web Re-integration Guide

As this is based on refactored Keyman Engine for Web 15.0 code, there will eventually come a time to
integrate this module back into it and replace the old version.  As there has been some refactoring,
this document will list notable changes to facilitate reintegration once this module is ready.

## From feat/web/gesture-recognizer-main

The OSK helper functions for `MouseEventEngine` and for `TouchEventEngine` have been disabled, as they
are references to components external to this module.

## From feat/web/gesture-recognizer-base

Starts work on a unified mouse+touch object - `GestureRecognizer` - and starts work on the unified
config object.

Rather than specifying callback methods, we also shift to use of `EventEmitter` and an event-based model.
At this stage, the event parameters should be relatively easy to interpret and forward to the original methods.

## From feat/web/gesture-recognizer-bounds

This adds abstraction + generalization for gesture detection boundary logic.  To translate the new config
parameters to KeymanWeb's VisualKeyboard...

`inputStartBounds`: would be left undefined.  Default behavior is a perfect match here.

`maxRoamingBounds`: would be backed by an invisible VisualKeyboard-managed element.  This element would be
and resized with the other VisualKeyboard elements.  Represents the currently 1/3-a-row buffer above the
keyboard that supports continued interaction before cancelling it.

Alternatively, this _could_ receive a custom wrapper implementation of the interface that simply appends
extra space to each side of the main target-element's `getBoundingClientRect` value.  (One issue with this
approach is that the padding value may differ per device orientation.)
- This could be supported via paddedZoneSource with negative padding toward the top.

`safeBounds`: would be left undefined.  The default behavior is modeled directly after VisualKeyboard's
viewport-aware bounds detection & logic.

`safeBoundsPadding` models the slightly-inset boundaries that are used to disable `safeBounds` near an edge
should the initial touch have started near that edge.

## From feat/web/recognizer-multipoint-tracking

This makes things a _bit_ more complicated than with gesture-recognizer-base.  Now, the `GestureRecognizer`
object's lone event will yield an `TrackedInput` instance corresponding to the ongoing input.  This object
contains a `TrackedPoint`, which tracks all data corresponding to its triggering `mousedown` or `touchstart`.
Further events for that "tracked point" are based on `TrackedPoint.path`.  This approach provides the
perspective of each touch point individually for further processing... effectively splitting multi-touchpoint
events into multiple _individual_ events while keeping the metadata organized over each touchpoint's lifetime.

## From change/web/internal-gesture-src-nomenclature

In case someone has to trace this history on things:

- `TrackedInput` is now `ComplexGestureSource`.
- `TrackedPoint` is now `SimpleGestureSource`.
- `TrackedPath`  is now `GesturePath`.

## From refactor/web/gesture-source-paradigm + its followup

- `ComplexGestureSource` is now fully decommissioned; it made assumptions that were overly complicating
gesture state-machine / model-matching code.

- Accordingly, `SimpleGestureSource` is now simply just `GestureSource`.  Yay on that end!
