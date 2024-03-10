import { positionAfter } from "../default/index.js";

// Based on the MIN_KEYSTROKE_PROBABILITY penalty used by the lm-worker.
const CHAR_SKIP_PENALTY = -Math.log2(.0001);

// const DEFAULT_PARAMS = {

// }

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

export type DictBreakerPath = {
  /**
   * The index of the character immediately before the most recently-available word boundary.
   * Is set to -1 if no such boundary exists.
   */
  boundaryIndex: number;

  // Could add a 'reference' if we create objects for each char in the context - such as for
  // caching & reusing boundary info with future inputs.

  /**
   * An active traversal representing potential words that may become completed, starting
   * immediately after the boundary indicated by `boundaryIndex`.
   */
  traversal: LexiconTraversal;

  /**
   * cost:  measured in -log(p) of each decision.
   */
  cost: number;

  /**
   * The path object used to reach the previous boundary.
   */
  parent?: DictBreakerPath;
}

/**
 * Provides dictionary-based wordbreaking assuming a LexiconTraversal can be specified for
 * the dictionary.
 * @param fullText
 * @param dictRoot
 * @returns
 */
export default function dict(fullText: string, dictRoot?: LexiconTraversal): Span[] {
  // const params = DEFAULT_PARAMS;

  // NOTE: not currently written to wordbreak the full context!

  // If we have a space or a ZWNJ (U+200C), we'll assume a 100%-confirmed wordbreak
  // at that location.  We only need to "guess" at anything since.
  const splitIndex = Math.max(fullText.lastIndexOf(' '), fullText.lastIndexOf('\u200C'));
  const text = splitIndex > -1 ? fullText.substring(splitIndex+1): fullText;

  // 1.  Splay the string into individual codepoints.
  const codepointArr = splitOnCodepoints(text);

  // 2.  Initialize tracking vars and prep the loop.
  let bestBoundingPath: DictBreakerPath = {
    boundaryIndex: -1,
    traversal: dictRoot,
    cost: 0
  };

  // Optimization TODO:  convert to priority queue.
  let activePaths: DictBreakerPath[] = [bestBoundingPath];

  // 3. Run the master loop.
  // 3a. For each codepoint in the string...
  for(let i=0; i < codepointArr.length; i++) {
    const codepoint = codepointArr[i];
    let paths: DictBreakerPath[] = [];

    // 3b. compute all viable paths to continue words & start new ones.
    for(const path of activePaths) {
      let traversal = path.traversal.child(codepoint);
      if(!traversal) {
        continue;
      }

      const pathCtd: DictBreakerPath = {
        boundaryIndex: path.boundaryIndex,
        traversal: traversal,
        cost: (path.parent?.cost ?? 0) - Math.log2(traversal.maxP),
        parent: path.parent
      }

      paths.push(pathCtd);
    }

    // 3c. Find the minimal-cost new path with a word boundary, if any exist.
    // If the traversal has entries, it's a legal path-end; else it isn't.
    const boundingPaths = paths.filter((path) => !!path.traversal.entries.length);
    // If none exist, this is the fallback.
    const penaltyPath: DictBreakerPath = {
      boundaryIndex: i,
      traversal: dictRoot,
      cost: bestBoundingPath.cost + CHAR_SKIP_PENALTY,
      parent: bestBoundingPath
    };

    boundingPaths.push(penaltyPath);
    // Sort in cost-ascending order.
    // As we're using negative log likelihood, smaller is better.
    // (The closer to log_2(1) = 0, the better.)
    boundingPaths.sort((a, b) => a.cost - b.cost);

    // We build a new path starting from this specific path; we're modeling a word-end.
    // If it's the "penalty path", we already built it.
    bestBoundingPath = boundingPaths[0];
    if(bestBoundingPath != penaltyPath) {
      bestBoundingPath = {
        boundaryIndex: i,
        traversal: dictRoot,
        cost: bestBoundingPath.cost,
        parent: bestBoundingPath
      }
    }

    paths.push(bestBoundingPath);

    // 3d. We now shift to the next loop iteration; we use the descendant `paths` set.
    activePaths = paths;
  }

  // 4. When all iterations are done, determine the lowest-cost path that remains,
  // without regard to if it supports a word-boundary.
  activePaths.sort((a, b) => a.cost - b.cost);
  const winningPath = activePaths[0];

  // 5. Build the spans.
  const spans: Span[] = [];

  let rewindPath = winningPath;
  while(rewindPath) {
    const start = rewindPath.boundaryIndex+1;
    const end = codepointArr.length; // consistent because of the effects from the splice below
    const text = codepointArr.splice(start, end - start).join('');

    spans.unshift({
      start: start,  // currently in code points; we'll correct it on the next pass.
      end: end, // same.
      length: text.length, // Span spec:  in code units
      text: text
    });
    rewindPath = rewindPath.parent;
  }

  // 6. Span pass 2 - index finalization.
  // - Remember, split-index is our offset!
  // - We currently have codepoint `start` and `end`, but need code-unit values.
  let totalLength = splitIndex;
  for(let i = 0; i < spans.length; i++) {
    const baseSpan = spans[i];
    const start = totalLength;
    totalLength += baseSpan.length;

    const trueSpan: Span = {
      ...baseSpan,
      start: start,
      end: totalLength
    };

    spans[i] = trueSpan;
  }

  // ... and done!
  return spans;

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