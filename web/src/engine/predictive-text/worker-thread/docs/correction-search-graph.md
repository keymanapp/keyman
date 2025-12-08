<!-- make it as a new file! -->
# The Correction-Search dynamic graph search-space

The Keyman predictive-text correction-search process is designed to consider all
of the most-likely possible input corrections when suggesting words from the
active lexical-model.  To do so, it dynamically builds portions of the search
graph as needed to generate corrections to the most recent token in the context.
This token lies immediately before the caret.

There is one major, notable simplifying assumption in the current
text-correction design:  we assume that each keystroke's `Transform` is 100%
independent from the `Transform` selected for every other keystroke.  This
assumption is, of course, invalid: the output of keystroke A may selectively
establish the context needed for a Keyman keyboard rule matched by one or more
keys in keystroke B.  Efforts to address this limitation are considered
out-of-scope at this time and will be addressed later in a future epic -
epic/true-correction - documented as issue #14709.

## Graph and Node Properties

### The overall graph

When viewed at low level, the graph generally takes on the form of a search
tree.  There is a single, common root node, with no input received or processed.
Each possible corrective-edit and/or keystroke replacement edit acts as an edge;
this edge then leads to a new node which represents a correction prefix - the
full text produced by the selected keystrokes and edits traversed on the graph.

In formal graph language, we should first note the following properties:
- The graph is directed:  all keystrokes and edits happen in a specific ordering
- The graph is acyclic:  there is no way to revisit a node more than once.
    - Even _if_ text deletions occurred that restored the text itself to match
      an earlier state, the new node would represent the additional keystrokes
      and thus not match the corresponding earlier node.
- The graph may not be, strictly speaking, a tree.
    - It is possible for the net result of two or more paths through the graph to
      produce the same text output from the same set of keystrokes.
      - The different paths may incur different costs.
    - To be a tree requires that each node may only be connected to the root via
      a single path.

### The individual nodes

The root node of the correction-search dynamic graph represents the empty token ``.
This token has no text content and represents no keystrokes.

Other nodes are reached by treating possible outputs from incoming keystrokes as
edges on the graph.  Noting the source `Transform` and its keystroke of origin, we
can generate a valid child node to represent the corresponding correction-search
prefix.

#### Critical node properties

1.  As future incoming `Transform`s may include `.deleteLeft` components, it is
important to note the represented codepoint length of the prefix.
-  It does not make sense to represent a node of negative length.  Should this
   result, we should throw away the token and start editing its predecessor
   instead.
-  A node of zero length may be considered to "throw away" and replace the
   corresponding context token with a new empty token.

2.  As the whole point of correction-search is to generate valid corrections for
the text, it is important to remember the range of input represented by any
generated correction.

- When the represented range matches that of the range represented by the
current active context text, corrections may be applied safely without
side-effects.

- Should the represented range differ from the range represented by the current
active context text, corrections to other tokens may be required; we do not wish
to either forget or to duplicate portions of the user's input.
    - E.g:  if a whitespace typo occurs mid-word, the user's expected correction
      might need to correct the current token, the whitespace's token, and the
      prior piece of the word into a single token.
    - The range for correction is determined by comparing the active context
      token ranges with that of the correction's source, which may have never
      added the wordbreak.

## Correction-Search and Dynamic Programming

With a few tweaks and restrictions, we can use dynamic programming techniques to
facilitate our correction-search processes.  First, note the assumption stated
earlier:

> [...] we assume that each keystroke's `Transform` is 100% independent from the
`Transform` selected for every other keystroke.

Therefore, we can find the cost of selecting a correction by using a
[divide-and-conquer
strategy](https://en.wikipedia.org/wiki/Divide-and-conquer_algorithm) for the
correction-search path:
1.  Find the cost of the path to which the incoming edit or keystroke
    `Transform` will apply.
2.  Modify by the cost of correcting the current keystroke with the specified
    `Transform` or applying a keystroke-level edit.

### Optimal Substructure

Let us examine the effects of adding different types of keystroke edits to the
correction-search path.

When adding an `insert` edit, we do not add data for an additional keystroke.
Instead, we look more deeply into the lexicon based on the current path's
prefix, extending the 'match' dimension of the search path with a cost penalty.
Thus, cost will only _increase_ for `insert` edit operations.

When adding a `delete` edit, we do add data for an additional keystroke, but opt
not to include its `Transform` whatsoever.  This extends the 'input' dimension
of the search path, paying a cost penality to do so.  Thus, cost will only
_increase_ for `delete` edit operations.

When adding a `match` or `substitute` edit without specified left-deletions, we
add data for an additional keystroke, including a corresponding `Transform`
while also looking more deeply into the lexicon in a manner that matches the
input.  For this case, no cost penalty is incurred for the 'match' component
when for `match` edits, though one _is_ applied for `substitute` edits.  A small
cost penalty corresponding to the selected `Transform`'s probability is applied
either way.  **So long** as the applied 'input' `Transform` has no specified
left-deletions, the total cost will remain flat or increase - it will not
decrease.  Both the 'input' and 'match' dimensions of the search path are
extended by `match` and `substitute` edit operations.

When adding a `match` or `substitute` edit **with** specified left-deletions, it
is possible for a naive implementation to perform a reduction in total cost.
Deleting portions of the 'input' (and corresponding sections of the 'match'
dimension) will reduce the path to a simpler state, generally of lower cost.
This is the sole case that may currently invalidate the dynamic programming
requirement of "optimal substructure".  (See #14366.)  With further time
investment, we should be able to develop and implement a strategy to restore
this condition even for such cases.

### Overlapping Subproblems

<!-- So, we talk dynamic programming, optimal substructure, etc BEFORE getting to modules. -->
<!-- The fact that we can design for these and apply them is WHY we can DO dynamic programming; it motivates the modules! -->

## < header goes here >

