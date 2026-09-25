# GestureRecognizer Glossary

- [**Finite State Machine (FSM)**](https://en.wikipedia.org/wiki/Finite-state_machine): A formal mathematical model of computation that abstracts a process into distinct states that may transition among each other.  [Wikipedia page](https://en.wikipedia.org/wiki/Finite-state_machine)

- **Gesture**: what the user perceives as a single gesture.  This may consist of multiple **gesture components**.  Often, though not always, this notion corresponds directly to a full **gesture sequence**.

- **Gesture component**: one segment of the touchpath and related metadata sufficient to trigger transition of state for gesture interpretation.  Each represents a **state cluster** (see below) for the implicit gesture-modeling **FSM**.

- **Gesture model**: the specification needed to interpret a segment of the touchpath as a gesture component.  Each directly defines a **state cluster** on the implicit **FSM** based on whether the input "resolves" the model or is "rejected" due to specific causes.  Each such cause may permit a different transition and thus corresponds to a different implicit state on the implicit gesture-modeling FSM.

- **Gesture processing**: the process of interpreting one or more touchpaths, at run-time, as a gesture sequence.

- **Gesture sequence**: A full sequence of causally-related and successfully matched **gesture component**s.  Represents the path of gesture-components taken by the input through the gesture-modeling **FSM** implicitly specified by the engine's `GestureModelDefs` value.

- **State cluster**: A set of implicit, highly-related states in the gesture-modeling FSM that arise from a gesture model specification.

Properties:
  1. All states share the same state ID, which is unique to the state cluster.
  2. After processing, there is no need to identify which specific state was reached - only that one of the variants was reached.
  3. The input paths necessary to reach each state are mutually exclusive and deterministic.
  4. All inbound paths to each state in the cluster have an identical specification that is fully independent from the prior state (or lack thereof).
  5. The permitted outbound transitions may vary for each state (as determined by input), but the non-FSM role in gesture modeling for each is identical.

- **Touchpath**: the path taken by a touchpoint over time

- **Touchpoint**: the location of the cursor (during mouse-based input) or of the finger (during touch) being observed.

## Examples

Consider the path taken by one finger during a potential touch-based gesture input.  Suppose a user holds a button or key down for a period of time to activate a menu, then selecting an item from that menu.  Note the phrasing there - there are two separate 'stages', or 'components', to that input:
1. The user holds the button down for a period of time, without moving the input location.
    - After a set amount of time passes, the UI's state shifts, displaying a new menu corresponding to the held location and button.
2. Once the menu appears, the user then moves their finger to select an item from the menu.
    - Once an item is selected and the finger is lifted, the program should respond and act accordingly to the selected menu item.

These two items correspond to **gesture components**, corresponding to _segments_ of the overall touch input path.  These components are related and happen in sequence as a **gesture sequence**, with the former a prerequisite for the latter.