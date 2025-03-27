# GestureRecognizer - gesture modeling
GestureRecognizer is designed to support customizable sets of gesture-components and gesture-component sequences, rather than forcing a non-customizable and limited number of gestures.  In order to simplify the requirements for gesture customization, there are three layers used when modeling gesture components.  Other aspects of GestureRecognizer have been designed to allow manage the parallel evaluation of multiple gesture components and ensure that only one per input triggers completion.

To define terms used here, consider the path taken by one finger during a potential touch-based gesture input.  Suppose a user holds a button or key down for a period of time to activate a menu, then selecting an item from that menu.  Note the phrasing there - there are two separate 'stages', or 'components', to that input:
1. The user holds the button down for a period of time, without moving the input location.
    - After a set amount of time passes, the UI's state shifts, displaying a new menu corresponding to the held location and button.
2. Once the menu appears, the user then moves their finger to select an item from the menu.
    - Once an item is selected and the finger is lifted, the program should respond and act accordingly to the selected menu item.

These two items correspond to **gesture components**, corresponding to _segments_ of the overall touch input path.  These components are related and happen in sequence, hence use of the term **gesture sequence**.

**Touchpoint** refers to the location of the mouse (during mouse-based input) or of the finger (during touch) being observed.

The three layers comprising gesture models are as follows:
1. [`PathModel`](#pathmodel) is responsible for modeling the path of the touchpoint handled by the `ContactModel` owning it.
2. [`ContactModel`](#contactmodel) owns a `PathModel` and is additionally responsible for validating initial state, changes in associated item, and the portion of previously-existing path to consider if continuing an active gesture sequence.
3. [`GestureModel`](#gesturemodel) owns at least one `ContactModel` and defines the gesture-component's properties and relationships to other gesture-components.  Also defines special properties for gesture-components with an initial state that lacks active touchpoints.

Topping these off is the [`GestureModelDefs`](#gesturemodeldefs) type.  This object should be defined once, with a full set of custom gesture-component definitions, as part of configuring a `GestureRecognizer` instance.

## `GestureModelDefs`

`GestureModelDefs` is used to specify a full set of all `GestureModel` definitions to be used for an instance of `GestureRecognizer`.  These may also be listed in `sets`, each identified with a unique `string` and comprised of `GestureModel` ids, for cases where the set of gestures available by default should change in certain conditions.

`GestureModelDefs` is used internally to define and implicitly specify a finite-state-model (FSM) for interpreting input as gestures.  The currently-active gesture set - `default` by default - specifies the legal states that may be reached from an implicit "start" state.  Once one is successfully matched, the properties of its `GestureModel` specify any further legal states that may succeed it.

There are essentially three implicit states not defined directly:
- "start" - when no gesture is yet matched
- "finalize" - when the last gesture-component possible has been matched.  After processing here is completed, state then transitions to "finish".
- "finish" - when all possible gesture-matching for the input has been completed - whether via cancellation, end of input, or reaching the end of supported gesture-components for the sequence of gesture-components already interpreted.

## `GestureModel`

Each `GestureModel` encapsulates the specification of an individual gesture-component and its relationships to other gesture-components.

From the FSM perspective, each `GestureModel` specifies the behaviors of a set of tightly-related FSM states, the requirements of any inputs / inbound edges that will satisfy them, and the legal outbound transitions to other states that each permits.  It does this in isolation, with minimal need to consider the specification of any other `GestureModel` "states" - the [`MatcherSelector`](./gesture-processing.md#gesturematcher) tier of gesture processing exists to support this paradigm.

The only aspect that affects one `GestureModel`'s relationship to others is that of its `resolutionPriority` value.  Processing of `GestureModel`s with higher values for this property will always occur the same for those of lower values, allowing them to "win" in the case that more than one `GestureModel` is satisfied by the input at the time that the latest sample input is received.

The `contacts` (`ContactModel`s) and `sustainTimer` properties encapsulate specification for the states' 'inputs'.  `sustainTimer` exists to handle the case where there should be no active 'inputs' for the initial state and should be only used here in that case.  `contacts` defines the specification and expectations for what matching mouse or touch input paths should look like.  These specifications also indicate how each matching input type should be interpreted in relation to permitted outbound transitions.

`resolutionAction` and `rejectionActions` encapsulate specification for the states' permitted outbound transitions.  Proper 'resolution' is the single required type of outbound transition that must be defined, defining one state for the tightly-related set.  When `rejectionActions` has defined alternatives (on the basis of `'path'`, `'item'`, and/or `'timer'`), each variant should be considered its own variant of the tightly-related set of states for the `GestureModel`.  Each such state then has its own permitted outbound transition (or possibly set thereof) to other `GestureModel`s (and thereby, states).  Note that "resolution" records the state as a permanent component of the observed gesture sequence, while any form of rejection does not do so, instead "replacing" the current model with that of the outbound edge as an option for further processing.

Finally, `sustainWhenNested` defines a special property for its corresponding state.  In the case that gesture processing is "shifted" to have a different initial state by the modeled gesture sequence, when set to `true` this property will defer any outbound transitions until all other gestures started during this alt-state reach completion.  When set to `false`, such 'nested' gestures would instead be cancelled.

## `ContactModel`

A `ContactModel` and its properties specify the expectations of the model for its touchpoint's path and its relationship to the corresponding path taken at the time the previous state (if it existed) finished processing.  The `pathModel` property is delegated responsibility for the path itself, while `pathResolutionAction` determines how to interpret successful resolution of the path.

From the FSM perspective, before the `pathModel` is even considered, if there is still an active touchpoint associated with the active gesture sequence, `allowsInitialState` is used to verify that the current state of the touchpath is allowed as input.  For example, certain gesture types may only be supported when certain 'items' were currently selected by the corresponding state's predecessor.  Once satisfied, `baseCoordReplacer` and `pathInheritance` are used to create a customized view of the path and selectively limit the range of path-history preserved as needed to facilitate the use of its `pathModel` for evaluating further updates input for the touchpoint's path.

Once the initial sample from the touchpoint's path is validated, `timer` is used to set a limited time window in place in relation to the touchpoint and how to interpret the elapsed amount of time once resolution is triggered.  If `timer.expectedResult == true`, once the set amount of time has elapsed, that itself will trigger resolution.  If `false`, path resolution will only trigger resolution of the model if the set amount of time has _not_ elapsed.

When either the timer (if specified) reaches its end or the `pathModel` signals resolution of the path, `validateItem` may be specified to perform a final check to ensure that the selected `item` is valid for the gesture being modeled.

## `PathModel`

`PathModel` is used within gesture modeling to specify whether the path-based aspects of the user's input for the modeled touchpoint satisfy a gesture-component's requirements.  The type is an object containing a single method - `evaluate` - which receives four parameters related to values accessible on the 'path' level and may return one of three values:
- `'reject'`:  the path is not legal for this model's gesture component; it should no longer be considered
- `'resolve'`: the path satisfies and completes the path aspect of this model's gesture component; we're ready to transition and either accept or reject it (based on other, higher-level model properties)
- `undefined`: the path is legal but does not _yet_ satisfy the path aspect of this model's gesture component.
