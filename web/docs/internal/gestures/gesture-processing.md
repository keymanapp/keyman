# GestureRecognizer - gesture processing

GestureRecognizer is designed to support customizable sets of gesture-component and gesture-component sequences, rather than forcing a non-customizable and limited number of gestures.  While customizability makes it difficult to determine the specifications needed for a traditional finite state machine (FSM), it is possible to straightforwardly define FSM states & transitions for each gesture and gesture component in isolation.

For definitions to a few commonly-used terms below, see the [Glossary](./glossary.md) document.

There are four separate layers used when processing gesture input to ensure that each gesture and gesture component may be modeled as they would work in isolation.  Sequential components are connected via FSM-style state transitions that the engine can resolve to ensure the best gesture-models for the user's input are selected.

The processing layers for gesture modeling are as follows:
1. [`PathMatcher`](#pathmatcher) - Handles modeling and matching of a single touchpoint's path over time.
2. [`GestureMatcher`](#gesturematcher) - Handles modeling and matching for a single gesture-component's isolated model.
3. [`MatcherSelector`](#matcherselector) - Coordinates groups of gesture-component models for each active input and ensures only one gesture model for each input reaches completion, keeping each individual gesture-component model isolated but ensuring only the user's intended one remains.
4. [`GestureSequence`](#gesturesequence) - Handles coordination of the higher-level state machine connecting the components of a "gesture sequence" - of multiple related gesture-components over time - as a complete, connected gesture.

Each layer but the last and outermost - `GestureSequence` - leverages `Promise`s that resolve once the layer's role in detecting input of a gesture-component is completed.

## `PathMatcher`

The `PathMatcher` class is responsible for matching the all aspects of a single touchpoint to its corresponding part of a potential gesture-component.  In particular, it matches both the [`PathModel`](./gesture-modeling.md#pathmodel) and [`ContactModel`](./gesture-modeling.md#contactmodel) layers of a gesture-component model for a single input point.

## `GestureMatcher`

The `GestureMatcher` class is responsible for the matching against the higher-level aspects of a single gesture-component model as defined on the [`GestureModel`](./gesture-modeling.md#gesturemodel) layer of gesture-component specification.  This includes handling the multi-touchpoint aspects of gesture-component matching (when one gesture-component permits or requires multiple `ContactModel`s).  `GestureMatcher` and `PathMatcher` together take a view in which its models operate in full isolation from all others.

It also handles cases where there may be _no_ touchpoints at the start of the model - this can be the case for gestures such as multitap, where a new component may start after another component finalizes with the release of a lone, active tap location.

When a `GestureMatcher` finalizes and its `Promise` resolves, the gesture component is considered to have matched and be valid component of the ongoing gesture sequence.  It will also indicate the manner in which the gesture component should resolve as well as indicating if further gesture components are permitted for the ongoing gesture sequence.

In the case that a `GestureModel` cannot be matched for an input due to an invalid initial state, `GestureMatcher` is responsible for handling this case and cancelling attempts to match that input.  `reject`ion is invalid for such cases in order to prevent a `reject`->replace loop from occurring, as gesture models support the ability to 'replace' themselves for the matching attempt with another instance of the same model in order to reset gesture properties, and the initial state would not change between matching attempts.

## `MatcherSelector`

Both `PathMatcher` and `GestureMatcher` above permit gesture-component modeling to be specified and act as if the modeled gesture is the only one under consideration.  This abstraction is permitted thanks to the efforts of `MatcherSelector`, which models the _potential_ gesture-components that _could_ result for each active input, filtering out any that are no longer valid possibilities.  When a new touchpoint starts being input, its gesture-component models are spun up here, as are the models permitted to continue an active gestures sequence.

There is no single, direct FSM used to determine which gesture-component, if any, is matched.  Instead, `MatcherSelector` enforces an "implicit" one built by considering all permitted transitions to new states (allowed [`GestureModels`](./gesture-modeling.md#gesturemodel)s) in isolation from each other.  Once any component moves from the "potential" state to an "actually matched" state by satisfying the required 'input' properties to reach the state, `MatcherSelector` "selects" that component as the "winner" for its corresponding input(s) and transitions to its state.  Specific aspects of the input are used to select _which_ version of the state (each with a different permitted transition to other states) is selected, and the corresponding outbound "edges" are then noted to prepare for processing any follow-up gesture-components.  `MatcherSelector` also handles cancellation of any no-longer-valid competitors and signals completion of the winner as a new component of its gesture sequence... or as the base component of a new sequence if applicable.

`MatcherSelector` will enforce, respect, and utilize "resolution priority" ordering among potential gesture-component processing.  In the case more than one model is matched, the correct (highest-priority) one will be matched as the "winner",  partly by leveraging the standard properties of `Promise`s; only the first resolution of any `Promise` counts.

When gestures permit, start, or enforce alternative modeling states that cause the set of permitted gesture types to change, this is enforced by pushing a new `MatcherSelector` onto a stack to model the alternative gesture-modeling state.  When said state ends, the `MatcherSelector` will be popped off of the stack.  For Keyman keyboards, this is leveraged by modipress gestures in order to prevent nested modipresses.

## GestureSequence

Whenever the first gesture-component for a `GestureSequence` is matched, and thus a valid gesture for input has been confirmed, a `GestureSequence` for that input is created.  This class will raise the `'stage'` event to signal when new valid gesture-component segments of the sequence have been recognized - including the first - and ensure that all such segments for the same input are properly associated with each other and properly ordered in chronological order.

Internally, `GestureSequence` works with `MatcherSelector` and handles its outputs, doing the actual work of interpreting newly-resolved models, their states, and related data.  `GestureSequence` is responsible for receiving the outbound transitions and passing them back to `MatcherSelector` in order to start interpretation for any potential follow-up gesture-components.