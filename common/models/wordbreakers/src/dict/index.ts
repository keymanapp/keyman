import { positionAfter } from "../default/index.js";

const DEFAULT_PARAMS = {

}

/**
 * Splits a string into its constituent codepoints.
 *
 * BMP chars will be placed within single-char strings; non-BMP chars will
 * be represented by a single string with their surrogate-chars paired.
 * @param text
 * @returns
 */
export function splitOnCodepoints(text: string): string[] {
  const splayed: string[] = [];
  let tailIndex = 0;
  let end = text.length;
  while(tailIndex != end) {
    const startIndex = tailIndex;
    tailIndex = positionAfter(text, tailIndex);
    splayed.push(text.substring(startIndex, tailIndex));
  }

  return splayed;
}

/**
 * Provides dictionary-based wordbreaking assuming a LexiconTraversal can be specified for
 * the dictionary.
 * @param fullText
 * @param dictRoot
 * @returns
 */
export default function dict(fullText: string, dictRoot?: LexiconTraversal): Span[] {
  const params = DEFAULT_PARAMS;

  // If we have a space or a ZWNJ (U+200C), we'll assume a 100%-confirmed wordbreak
  // at that location.  We only need to "guess" at anything since.
  const splitIndex = Math.max(fullText.lastIndexOf(' '), fullText.lastIndexOf('\u200C'));
  const text = splitIndex > -1 ? fullText.substring(splitIndex+1): fullText;

  // 1.  Splay the string into individual codepoints.
  const codepointArr = splitOnCodepoints(text);

  /*
    Important questions:
    - What is the cheapest way to have a word-break boundary after this character?
      - This is a 100% valid question; the complications arise in moving from an "earlier"
        answer to a "later" answer.

    - What words are possible to reach given recent possible boundaries?
      - idea: keep a running 'tally' of all Traversals that have valid paths at the
        current processing stage, noting what their starting points were.
        - ... instead of 'tally'... 'priority queue'?
          - cheapest (start point cost) + (current traversal min cost) makes a good
            A* heuristic.
            - valid heuristic - traversal min-cost will never overestimate.
            - would likely avoid a need to search expensive branches this way.
            - ** NOTE: traversals do not CURRENTLY track their min-cost. **
              - should be possible to add as a feature, though.
              - current correction considers fat-finger prob * correction cost.
              - lexical probability only factors in 100%-after corrections, as a
                final step, at present, hence why it's not currently available.
        - if no longer valid, drop it from the 'tally' / the 'running'.
        - after each codepoint, we always add a newly-started traversal.
          - worst-case, it comes with a set penalty cost added to the previous
            codepoint's cost.
        - if a current traversal has entries, we have a direct candidate for best
          cost at this location.
          - if using the priority queue strat, we may need to process just enough entries
            to where the next node is equal or higher cost to the selected entry.
            - unless the cost crossed to reach it IS that cost.
          - If multiple co-located entries, use the best cost of them all.  They all
            search-key to each other, anyway, so whatever is best is still "valid enough"
            to trigger a boundary.

    So...

    O(N), N = length of string:  loop over each codepoint
      O(N [worst-case]): loop over each still-valid traversal
        - at most, matches the index of the current codepoint
        - in practice, will be NOTABLY fewer after the first few codepoints.
        O(A), A = size of alphabet: loop over traversal branches
          - ** though, in theory... couldn't we just direct look-up the exact branch? **
            - Trie format's traversal absolutely has the needed backing data.
            Would reduce O(A) => O(1), which is significant.
          - on valid transition, add new, initial-rooted traversal based at the new spot, referring to
            prev spot as ancestor.


    So far, those're two aspects of Traversals (noted with **'s) that could be enhanced to help optimize
    dict-breaking.
    - One to provide a heuristic & allow pruning paths via A*
      - also... to allow needing to use a Trie model from within the wordbreaker for word-prob lookups.
    - One to dramatically trim down the number of needed loop-iterations / branches for the search update indexing.
      - See:  O(A) => O(1).
  */

  // for(let branch of dictRoot.children()) {

  // }

  // dictRoot.entries[0]

  // 2.  Initial state:  one traversal @ root pointing to 'no ancestor'.
  // 2b. Could prep to build a 'memo' of calcs reusable by later runs?
  //     - may be best to note "still-valid start locations at this node"
  // 3.  Run the boundary search - see the approx looping structure noted above.
  // 4.  Best answer at the end wins!
  // 5.  May be worth persisting a cache of recent memos, do a quick diff on the
  //     most recent as a step 0 in future runs, to reuse data?
  //     - but... how to clear that cache on model change?
  //       - duh!  validate the passed-in root traversal.  If unequal, is diff model.  ezpz.

  return [];
}