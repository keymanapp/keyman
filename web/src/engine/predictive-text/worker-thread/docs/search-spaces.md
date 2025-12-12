# The SearchSpace types

The `SearchSpace` interface exists to represent portions of the dynamically-generated graph used for correction-searching within the predictive-text engine.  As new input is received, new extensions to previous `SearchSpace`s may be created to extend the graph's reach, appending newly-received input to the context token to be corrected.  Loosely speaking, different instances of `SearchSpace` correspond to different potential tokenizations of the input and/or to different requirements for constructing and applying generated suggestions.

There are two implementations of this interface:
- `SearchPath`, which extends a `SearchSpace` by a single set of recent inputs affecting the range of represented text in the same manner.
- `SearchCluster`, which exists to _group_ `SearchPath` instances that represent the same range of represented text and input  set.

Both are capable of generating potential corrections, utilizing their links to prior `SearchSpace` entries in a manner that shares common corrected prefixes with other `SearchSpace` branches.

## The Underlying Problem

### Defining the Problem
It is easily possible for a user to fat-finger, accidentally typing a standard letter instead of the spacebar or similar when the latter is intended.  For languages using standard whitespace-based wordbreaking, this implies that the word boundaries seen in the context should not be considered absolute; we should model cases where the word-boundaries land elsewhere due to fat-finger effects.  Additionally, we have standing plans to support dictionary-based wordbreaking for languages that do not utilize whitespaces between words - this adds an extra case in which word-boundaries cannot be considered absolute.

Keyman keyboard rules further complicate matters.  They do not need to consider side-effects for predictive-text, and it's easily possible for a rule to output text changes that affect (or even _effect_) multiple text tokens within the context.

Take Greek, which has special word-final behavior for sigma:
- within a word, `σ` is the lowercase form.
- at the end of a word, it should appear as `ς` instead.

Keyman keyboards exist with the following behavior:

```keyman
"σ" + [K_SPACE] > "ς "
```

Note that this results in a context-manipulation (a `Transform`) spanning two tokens:
- it alters the end of the word currently at the end of context
- it also adds a whitespace token

There also exist keyboards like `khmer_angkor` that may perform character reordering, performing significant left-deletions and insertions in a single keystroke.  Furthermore, there's little saying that a keyboard can't be written that deletes a full grapheme cluster, rather than an individual key - a process that would add multiple left-deletions without any insertions.

We need structures and design to cover _all_ such cases - and to do so reliably and consistently.

### Requirements

Our model for correction-search requires the following properties:

1.  It must be able to model cases where keystrokes do not consistently correspond to the same text tokens.

Consider typing the word `banner` in English.  The `n` key is close to the spacebar, so its fat-finger distribution may contain the spacebar's output as an option.  Thus, there is the opportunity for correction to(or from) `ban` + ` ` + `er` instead, replacing the second `n` with a space.  The same keystrokes thus may correspond to one or three text tokens.

Consider the implications of this:
- Perhaps `banner` is the intended word
- Alternatively, `ban` is a legitimate word, and `er` is a perfectly valid English prefix.  What if the user is typing a word starting with `er`... do we have good suggestions based on that?
    - Suppose this gives us the following suggestions:  `banner`, `ban errors`, `ban erosion`.
    - How much of the context should be altered when applying these suggestions?
<!--   - To complicate matters further, note that the letters `c`, `v`, and `n` are also close to `b`.
    - Suppose this leads to `van errors`, `NaN errors`, etc..., but also `cannery`, `Vannessa`, etc. -->

2.  Each individual `SearchSpace` should only model correction of inputs that result in tokens of the same codepoint length as each other.
    - Consider the effects of a long left-deletion input.
        - If a `SearchSpace` held variable-length text, a large left-deletion could delete the whole block of text for some cases, but not for others; this gets messy to handle!
    - We want to ensure that `deleteLeft` effects can be modeled clearly and consistently, without ambiguity.

### Analysis

1.  Each keystroke's probability may be considered independent from other keystrokes.
    - To be clear, this is a simplifying assumption; attempting to do otherwise would both be speculative and complicate things further.

2.  It is not possible to guarantee that one keystroke will only extend a previous `SearchSpace` in one way.
    - If the incoming keystroke produces `Transform`s that have different `insert` length without varying the left-deletion count, this _must_ result in multiple `SearchSpace`s, as the total codepoint length will vary accordingly.
    - Also of note:  if left-deleting, it is possible for a left-deletion to erase the token adjacent to the text insertion point.

3.  When constructing and applying `Suggestion`s, it helps greatly to determine which `SearchSpace` led to it.
    - This allows us to determine _which_ keystrokes are being replaced, as well as _what_ parts of the Context will be affected.
    - If a correction is only possible if the context were to have a different initial word-boundary than currently exists, this implies a need to correct the Context accordingly as part of applying a suggestion.

4.  Following from the points above, it is possible for two or more `SearchSpace` segments to model input sequences with the same properties, as follows:
    - The input length in codepoints is identical.
    - The same keystrokes, and portions thereof, are utilized.

For example, consider a case with two keystrokes, each of which has versions emitting insert strings of one and two characters.  Taking two chars from one and one char from the other will result in a `SearchSpace` that models a total of two keystrokes that fully covers the two keys.

For such cases, any future keystrokes can extend both input sequences in the same manner.  While the actual correction-text may differ, the net effect it has on the properties of a token necessary for correction and construction of suggestions is identical.  The `SearchCluster` variant of `SearchSpace` exists for such cases, modeling the convergence of multiple `SearchPath`s and extending all of them together at once.