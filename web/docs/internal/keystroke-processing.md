# Keystroke Processing

In addition to handling keystroke events produced from hardware keyboards, Keyman Engine for Web manages an on-screen keyboard that can also be used to generate such events.  This document will enumerate the various components of data management and control flow used to handle and process keystrokes.  For a more thorough examination of the keystroke lifecycle, see the [keystroke-lifecycle](./keystroke-lifecycle.md) page.

## Input:  KeyEvent - the pre-processed keystroke

Defined at [web/src/engine/keyboard/src/keyEvent.ts](https://github.com/keymanapp/keyman/blob/master/web/src/engine/keyboard/src/keyEvent.ts), the `KeyEvent` is used to represent incoming _and_ potential keystrokes.  This type is what the JS-keyboard processor references when evaluating keyboard rules during its keystroke processing.  For versions of the engine that support AltGr aliasing, such aliasing will be applied during generation of `KeyEvent` objects.

Note that for predictive-text's fat-finger correction functionality, the engine will also generate versions of this type for nearby but _unpressed_ keys into the keystroke processing engine as well in order to facilitate predictions that follow context manipulations that could have resulted from specialized rules or reorders on neighboring keys.  `Mock`-cloned copies of the active context-state will be leveraged to prevent unwanted manipulation of the true context source.

### `isSynthetic`
`isSynthetic` should be set to `true` if generated through interaction with an on-screen-keyboard or for fat-finger simulation.  It should only be set to `false` if sending the basic key-event data through to the destination, without rule processing, leads to default handling picking up the slack.  Browsers provide default handling of keystrokes not directly defined within keyboards, but this is not available for keystrokes against the engine's OSK without internal support.

For other applications of `false`, consider a hardware K_ENTER keystroke - this could be interpreted as a "submit form" / "search" command depending on the context.

When `true`, the keystroke processor will need to simulate the results of "standard" default key-event handling, as nothing else can handle it.  At present, we don't attempt to force / synthesize an event that could trigger the "true" default.

### `on('keyevent')`

On-screen keyboard modules and hardware-keyboard event interception modules will produce instances of `KeyEvent` as noted above.  Once this is completed, both module types will _signal_ the keystroke event by raising an event with ID `keyevent` and provide the `KeyEvent` instance as its first parameter.  The main module of the engine will then forward that `KeyEvent` object to the input processing pipeline via `InputProcessor.processKeyEvent` (see below).

## Output:  RuleBehavior

Once keystroke processing is completed, the outward-facing components of the engine expect to receive a `RuleBehavior` object describing all primary and side effects of the keystroke.  Defined at [web/src/engine/js-processor/src/ruleBehavior.ts](https://github.com/keymanapp/keyman/blob/master/web/src/engine/js-processor/src/ruleBehavior.ts), all of its fields aside from `transcription` are specific to existing JS-keyboard side effects.  (Certain Keyman language features can make permanent side-effect changes to state that should only be persisted for the "true" keystroke - not for any predictive-text 'alternative' keys.)

See also:  [context-state-management.md](context-state-management.md#js-keyboard-keystroke-processing)

## Processing Pipeline

### Input Processor

The "first stop" for incoming keystrokes is the `InputProcessor`, found at [web/src/engine/main/src/headless/inputProcessor.ts](https://github.com/keymanapp/keyman/blob/master/web/src/engine/main/src/headless/.inputProcessor.ts), through its `processKeyEvent` method.  This class manages higher-level functionality triggered by keystroke events while deferring actual interpretation of the incoming keystroke further down the line.  Of particular note is that it also handles control-flows that require restoration of previously-occuring contexts.

This class is the connection point for generating prediction requests and receiving corresponding suggestions.  In order to facilitate higher-quality predictive-text when enabled, the `InputProcessor` will _also_ generate and trigger processing for nearby keys.  This process allows transforms, reorders, and KMN keyboard rules to take effect and be used as alternative context roots for predictions.  These are generally run against `Mock`-based clones of the true context source and are additionally prevented from triggering long-term side-effects, such as changes to KMN-keyboard variable stores, by only calling `RuleBehavior.finalize` for the true input keystroke's result object.

Keys generated by OSK multitap need special handling here as well; they should always be applied to the context state as it existed at the time of the initial tap.  To facilitate this, the `InputProcessor` will directly rewind the active context-source to match the corresponding context state before requesting that the `KeyEvent` be processed.

Due to the role `InputProcessor` plays, it is intended that _all_ keystroke processing variants leverage this class in order to maintain integration with such higher-level functionality.

#### The "context cache"

As both predictive-text and multitaps require the ability to reference old context states, the `InputProcessor` maintains a cache of recently-occurring context-state transitions - of [`Transcription`](./context-state-management.md#the-transcription---representing-context-state-transitions)s.  This cache is kept reasonably small to prevent memory bloat, so functionality referencing the cache should verify the results of any related lookup operation.

Thanks to the `Transcription.preInput`, it is possible to rewind the current context to its state at the time the `Transcription` was generated - a feature quite useful for applying predictive-text suggestions and for handling multitap keystrokes.  Predictive-text reversions may even re-use cached predictions associated with the `Transcription`, avoiding the need to recompute them.

#### LanguageProcessor

The `LanguageProcessor` class, whose instances are owned within the engine by `InputProcessor`, are the anchor point for connecting rest of Keyman Engine for Web with the predictive-text web-worker and its functionalities.

### KeyboardProcessor
- `epic/web-core` note:  renamed to `JSKeyboardProcessor`.

Found within [web/src/engine/js-processor/src/keyboardProcessor.ts](https://github.com/keymanapp/keyman/blob/master/web/src/engine/js-processor/src/keyboardProcessor.ts), the `KeyboardProcessor` class is the top-level class responsible for processing keystrokes targeting JS-based keyboards.  It is also responsible for providing default output for OSK-based keystrokes not explicitly handled by the keyboard that should receive browser-default handling (when JS-based keyboards are active).
- `epic/web-core` note:  renamed to `JSKeyboardProcessor`, moved to `jsKeyboardProcessor.ts` within the same folder.

This is likely the top-level class that `epic/web-core` would likely need to replace wholesale with a Keyman Core wrapper - hopefully with a mutual abstraction providing a similar interface for both.  (`KeyboardProcessor` was originally named after Core's equivalent class, as it pretty much fills the same role.)  `InputProcessor` could then leverage that mutual abstraction for its functions without needing a specialized version per keystroke interpreting engine.

#### The `processKeystroke` method

https://github.com/keymanapp/keyman/blob/b4df4ab80862bc90da42bcdbd333df0a14da01ca/common/web/keyboard-processor/src/text/keyboardProcessor.ts#L217

The method linked above is the primary entrypoint for rule processing of individual, preprocessed `KeyEvent`s.  There's a little infrastructure between it and the actual keyboard-script for JS keyboards, but it is the sole entry point in `KeyboardProcessor` responsible for keyboard rule evaluation.
- `epic/web-core` note:  renamed to `JSKeyboardProcessor`.

#### JS-keyboard interfacing

Certain Keyman language features can make permanent side-effect changes to state.  In order to prevent these from taking place for every keystroke, the method that interfaces with JS keyboards - `KeyboardInterface.process` - saves the context state (as a `Mock`) and current variable store values, then prepares a fresh `RuleBehavior` instance, before passing control off to the keyboard's backing script.  (Note that `KeyboardInterface` itself primarily consists of keyboard-script API called by JS-keyboard script.)
- `epic/web-core` note:  renamed to `JSKeyboardInterface`.

A few of the keyboard-script API methods will mark `RuleBehavior` properties directly when called, but the bulk of its data will be set once the keyboard-script returns control to Keyman Engine for Web.  At this time, variable store values will also be reverted to prevent possible cross-contamination effects when predictive text is active - they're reapplied later if `RuleBehavior.finalize` is leveraged on the resulting instance.  Components documented in [context-state-management.md](./context-state-management.md) are then leveraged to determine the total change to context caused by the keystroke.

Note that for JS-keyboards, in 18.0 and before the true keystroke is processed against the _true_ context state, not a cloned copy, and thus its changes are applied immediately.

#### Keystroke-default emulation

Note that browser-default keystroke processing is defined within the `DefaultRules` class found at [web/src/engine/keyboard/src/defaultRules.ts](https://github.com/keymanapp/keyman/blob/master/web/src/engine/keyboard/src/defaultRules.ts).  It currently produces a (possibly `null`) string output rather than a `RuleBehavior`, but the latter can be easily constructed based on the returned string and the existing context state.
- For `epic/web-core`, it may be wise to spin the `DefaultRules` component off as its own processor, serving as a backup for _all_ keystroke processing variants.

----

In case referenced classes/files have moved:  this doc was last updated in 18.0-beta, based upon commit d1e45a0df49f81597887577a8c83af36d4a85283