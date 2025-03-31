# Gesture Recognizer design - index

GestureRecognizer is an engine designed to interpret mouse event paths and touch event paths into sequences of externally-specified [gesture components](./glossary.md).  This is accomplished via definition of gesture-component "models", which the engine is then able to interpret when "processing" the input.  Each gesture-component model may be defined from its own perspective independently of any other, though effort may be required to identify and carefully define components that are shared by two or more gesture types.

There are a few different aspects to GestureRecognizer, each with its own internal doc.

- [Gesture modeling](./gesture-modeling.md) refers to the specification of gestures to be interpreted from the user's input.
- [Gesture processing](./gesture-processing.md) refers to the process of interpreting the user's input as gestures.
- The [Glossary](./glossary.md) provides definitions for some of the terms seen among the internal documentation pages.

Keyman configures two separate instances of this gesture engine - one for main keyboard body and another for the predictive-text banner.  In both cases, gestures (from the user perspective) are broken own into components, each corresponding to different movement patterns that comprise a each gesture type.  Once components are recognized, they are added into a sequence that represents the causal relationship among the different components of the gesture and ensures that it may continue if appropriate for that type of gesture.

----

For the main keyboard implementation of Keyman Engine for Web, the full set of Keyman gesture components is defined in `web/src/engine/osk/src/input/gestures/specsForLayout.ts`.

### Longpresses

Components:
1. "Hold and wait"
    - The subkey menu is displayed once this state is reached.
2. Subkey selection

### Flicks
1. Flick start - the user has input enough motion to consider this the start of a flick (and not the start of a longpress)
2. Flick mid - the user has input enough motion in a specific direction to "lock" the flick (in order to ensure a smoother animation)
3. Flick reset - the user has moved the touchpoint close enough to the start location to "unlock" the flick and reset the flick's base coordinate.
4. Flick end - the user has input enough motion to confirm the flick _and_ released the touchpoint.

### Multitap
1. First tap - the user has tapped a key.
2. Awaiting the next tap
3. Awaiting release of the current tap
4. Exit case:  a key was held too long - no further taps on the key should be considered part of the gesture

### Modipress
1. The user holds down on a modipress-friendly key
2. Engine alt-state:  a different set of initial gesture components is activated and used for newly-starting concurrent gestures
3. Release of the modipressed key.