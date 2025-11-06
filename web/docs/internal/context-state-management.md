# Context State Management

## The `TextStore` Abstraction

The `TextStore` abstraction and its associated types and classes exist
to facilitate handling different types of context sources within Keyman
Engine for Web through a common interface.  In essence, any implementing
type is valid within the engine as a "target" for "output" from any
existing keyboard supported by the engine.  Through 18.0, only JS-based
keyboards were supported due to lack of implementation of alternate
keystroke-processing engines.

The base implmentation may be found at
[web/src/engine/keyboard/textStore.ts](https://github.com/keymanapp/keyman/blob/master/web/src/engine/keyboard/src/textStore.ts).
The methods defined therein support properties and methods for fetching,
setting, and manipulating deadkey markers, text-selection, and text
within whatever context source it represents.  JS-keyboard keystroke
processing directly uses these methods during operation.

Since the context sources are the same for all keyboard processors, the
all use the same base implementation, although not all funcitonality is
needed by all keyboard processors (e.g. deadkey tracking functionality
is only needed by the JS-keyboard processor). This implementation
provides common support for deadkey tracking, matching, and handling -
the same strategy is used regardless of context-source type.  A few
additional methods are provided to assist with comparison and contrast
between two different context states and restoration of a prior context
state.

- note:  previously called `OutputTarget`

### Deadkey management

Specifics for the implementation of JS-keyboard deadkeys can be found
here: [web/src/engine/keyboard/src/deadkeys.ts](https://github.com/keymanapp/keyman/blob/master/web/src/engine/keyboard/src/deadkeys.ts)

`ordinal`:  if two deadkeys are in the same "virtual" position, this
field resolves which came first. `matched`: used during keyboard-rule
processing.

Note that we do NOT actually insert the deadkeys into the raw
text-context!  Web's pattern here is different than that of Keyman Core.

## Manipulating context-states

Initially used to support predictive-text, the `Transform` type aims to
encapsulate the minimal information necessary to transition from one
context-state to another.  It is both selection-agnostic and
deadkey-agnostic.  `Transform`s are used both by predictive-text and by
the webview-embedded build of the engine in order to succinctly
communicate the data needed to update context upon receiving keystrokes.

- `Transform`'s specification may be found at
  [common/web/types/src/lexical-model-types.ts](https://github.com/keymanapp/keyman/blob/master/common/web/types/src/lexical-model-types.ts).

`Transform`s consist of three values:

- `deleteLeft` - the number of codepoints prior to the caret/selection
  that should be deleted
- `insert` - the text to insert at the caret and/or replace
  currently-selected text
- `deleteRight` - the number of codepoints _after_ the caret/selection
  that should be deleted
  - Note that `deleteRight` does not currently see actual use due to iOS
    platform limitations.

This type may then be used as an argument to `TextStore.apply()`
(defined on `TextStore`) to update any context source accordingly.

It is possible to determine the `Transform` needed to transition from
one `TextStore` to another using `TextStore.buildTransformFrom`
(defined on `TextStore`).

### The `SyntheticTextStore` - representing context-state

The comparison and contrast methods mentioned above for `TextStore`
are of particular use for predictive text, which usually operates with a
headless implementation of the type, termed a `SyntheticTextStore`.  This class may be
found in
[web/src/engine/keyboard/src/syntheticTextStore.ts](https://github.com/keymanapp/keyman/blob/master/web/src/engine/keyboard/src/syntheticTextStore.ts).
It is a fully-featured implementation of the `TextStore` interface.

It is possible to make a `SyntheticTextStore`-based clone of any
`TextStore`-derived type - a feature leveraged significantly within
the inner workings of Keyman Engine for Web.  As JS keyboards can have
side effects beyond text-manipulation, predictive text generally
operates by first _cloning_ the "true" context source.  `SyntheticTextStore`s are
also used when saving context states within the engine for later
reference and/or reuse - a feature also utilized significantly for
multitap support.

`SyntheticTextStore`s can also easily be constructed from scratch for a simple string.
Optionally, caret position or selection data may be specified at
construction time as well. `epic/web-core`: in theory, this should make
them easy to utilize for integration with Keyman Core.

- note: previously called `Mock`

### The `Transcription` - representing context-state transitions

The `Transcription` class (defined in
[web/src/engine/js-processor/src/transcription.ts](https://github.com/keymanapp/keyman/blob/master/web/src/engine/js-processor/src/transcription.ts))
is the type within the engine that most closely matches a sense of
transition in context state.   These are generally constructed by
comparing two `TextStore` instances to each other via
`TextStore.buildTranscriptionFrom`, with the base instance
corresponding to the "new" state and the first parameter matching the
original state before transition.

`Transcription`s are granted unique identifiers and are used within the
engine for preservation of recent context states. These identifiers are
currently generated within the class's constructor and are internally
set.  When predictive-text generates new suggestions or a multitap needs
to revert to a prior context, both will use the `Transcription`'s unique
identifier in order to find the corresponding context state and leverage
it as needed for their operations.

Important fields:

- `keystroke` - the keystroke that triggered the context change
  corresponding to this `Transcription`
- `transform` - the direct effects of the keystroke

    - This uses a specialized variant that also notes if the transition
      destroyed previously-existing selected text.

- `preInput` - the state of the context immediately before the keystroke
  was processed

Note that the transition metadata does not include deadkeys generated by
its triggering `keystroke`.  The decision was made long ago to forgo
directly recording deadkey changes when recording `Transcriptions`, as
any operation that restores an old context also seeks to apply
deadkey-destroying operations immediately afterward.  Should we ever
need to do so, PR #1611 contains code that was originally designed for
actively detecting and recording deadkey transition data in
`Transcription`s.

A cache of recent context-state transitions is stored at
`keyman.core.contextCache`, with `keyman.core` being an instance of
`InputProcessor`
([web/src/engine/main/src/headless/inputProcessor.ts](https://github.com/keymanapp/keyman/blob/master/web/src/engine/main/src/headless/inputProcessor.ts)),
the component responsible for linking keystroke processing with
predictive-text support and restoration of context-state for
multitap-generated keystrokes.

### JS-keyboard keystroke processing

For JS-keyboard keystroke processing, a `SyntheticTextStore` clone of the context is
generated before any actual keyboard rule checks are applied.  This
provides a clear "before" state (eventually saved at
`Transcription.preInput`) useful for determining the scope of the
keystroke's changes once processing is completed via
`buildTranscriptionFrom`.

Once keystroke processing is completed by a JS-keyboard, the
JS-processor constructs a `ProcessorAction` object describing all
primary and side effects of the keystroke.  Defined at
[web/src/engine/keyboard/src/keyboards/processorAction.ts](https://github.com/keymanapp/keyman/blob/master/web/src/engine/keyboard/src/keyboards/processorAction.ts),
all of its fields aside from `transcription` are specific to JS-keyboard
side effects, some of which do need special handling and support outside
of the keystroke processor.  None of these side effects apply for
common-case keystrokes and so have default handling in place within the
engine for cases where they are not needed.

---

In case referenced classes/files have moved:  this doc was last updated
in 19.0-alpha, based upon PR #14001.
