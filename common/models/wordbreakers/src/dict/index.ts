import { LazySpan } from "../default/index.js";

// Based on the MIN_KEYSTROKE_PROBABILITY penalty used by the lm-worker.
const CHAR_SKIP_PENALTY = -Math.log2(.0001);

// const DEFAULT_PARAMS = {

// }

export function splitOnWhitespace(text: string): Span[] {
  const sections: Span[] = [];

  let start = 0;

  // Surrogate pairs will never overlap \u0020, so we don't need to be
  // surrogate-pair aware here.
  for(let index = 0; index < text.length + 1; index++) {
    let char = (index == text.length ? ' ' : text.charAt(index));
    if(char.match(/\s/) || char == '\u200c') {
      if(start !== undefined) {
        sections.push(new LazySpan(text, start, index));
        start = undefined; // we do not emit whitespace tokens here.
      }
    } else if(start === undefined) {
      start = index;
    }
  }

  return sections;
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
   * Indicates if this path's most recent traversal enforces a boundary without matching
   * a word in the lexicon.
   */
  wasUnmatchedChar?: boolean;

  /**
   * The path object used to reach the previous boundary.
   */
  parent?: DictBreakerPath;
}

/**
 * Provides dictionary-based wordbreaking assuming a LexiconTraversal can be specified for
 * the dictionary.
 * @param fullText The full context to be tokenized.
 * @param dictRoot A LexiconTraversal interface from the active LexicalModel,
 * allowing efficient dictionary lookups of encountered words.
 * @returns
 */
export default function dict(fullText: string, dictRoot: LexiconTraversal): Span[] {
  if(!dictRoot) {
    throw new Error("Cannot use dictionary-based wordbreaker without `LexiconTraversal` dictionary access");
  }

  // Whenever we have a space or a ZWNJ (U+200C), we'll assume a 100%-confirmed wordbreak
  // at that location.  We only need to "guess" at anything between 'em.
  const sections = splitOnWhitespace(fullText);
  let allSpans: Span[] = [];

  for(const section of sections) {
    // Technically, this may give us a 'partial' wordbreak at the section's end, which
    // may be slightly significant for earlier sections.  Probably not worth worrying
    // about, though.
    allSpans = allSpans.concat(_dict_break(section, dictRoot));
  }

  return allSpans;
}

/**
 * Given a section of text without whitespaces and ZWNJs, uses the active lexical-model's
 * entries to detect optimal word-breaking locations.
 * @param span A span representing the section and its position within the context.
 * @param dictRoot A LexiconTraversal interface from the active LexicalModel,
 * allowing efficient dictionary lookups of encountered words.
 * @returns An array of `Span`s representing each tokenized word, indexed according to their
 * location in the section's original context.
 */
function _dict_break(span: Span, dictRoot: LexiconTraversal): Span[] {
  if(span.length == 0) {
    return [];
  }

  /*
    The biggest, most important question:  "What is the cheapest way to have a
    word-break boundary after this character?"
    - Most complications arise in the process of moving from an "earlier" answer
      to a "later" answer.
   */

  const text = span.text;
  const splitIndex = span.start;

  // 1.  Splay the string into individual codepoints.
  const codepointArr = [...text];

  // 2.  Initialize tracking vars and prep the loop.
  let bestBoundingPath: DictBreakerPath = {
    boundaryIndex: -1,
    traversal: dictRoot,
    cost: 0
  };




  /* Optimization TODO:  convert to priority queue?
    - cheapest (start point cost) + (current traversal min cost) makes a good A*
      heuristic.
      - valid heuristic - traversal min-cost will never overestimate.
      - min-cost:  higher probability = lower cost
        - basically... good ol' negative log-likelihood, or at least something
          proportional
      - would likely avoid a need to search expensive branches this way.
        - current correction considers fat-finger prob * correction cost.
        - lexical probability only factors in 100%-after corrections, as a final
          step, at present, hence why it's not currently available.
   */

  /**
    Keeps a running 'tally' of all Traversals that have valid paths at the
    current processing stage, noting what their starting points were.
    - The driving question:  "What prefixes and words are possible to reach
      given recent possible boundaries?"
   */
  let activePaths: DictBreakerPath[] = [bestBoundingPath];

  // 3. Run the master loop.
  // 3a. For each codepoint in the string...
  for(let i=0; i < codepointArr.length; i++) {
    const codepoint = codepointArr[i];
    // We rebuild the `activePaths` on each outer-loop iteration.
    // Essentially, we're "double-buffering".
    let paths: DictBreakerPath[] = [];

    // 3b. compute all viable paths to continue words & start new ones.
    for(const path of activePaths) {
      let traversal = path.traversal.child(codepoint);
      // If a path is no longer valid, drop it from the 'tally' / the 'running'.
      if(!traversal) {
        continue;
      }

      const pathCtd: DictBreakerPath = {
        boundaryIndex: path.boundaryIndex,
        traversal: traversal,
        cost: (path.parent?.cost ?? 0) - Math.log2(traversal.p),
        parent: path.parent
      }

      paths.push(pathCtd);
    }

    // 3c. Find the minimal-cost new path with a word boundary, if any exist.
    // If the traversal has entries, it's a legal path-end; else it isn't.
    const boundingPaths = paths.filter((path) => !!path.traversal.entries.length);

    // We build a new path starting from this specific path; we're modeling a word-end.
    // If none exist, this is the fallback... and we penalize with a notably higher cost
    // for doing so.
    const penaltyParent: DictBreakerPath = {
      boundaryIndex: i-1,  // successor will cover one codepoint
      traversal: dictRoot.child(codepoint), // no `entries`, but... it's fine.
      // bestBoundingPath is currently a root-level traversal.  Its parent corresponds
      // to the previous token.
      cost: bestBoundingPath.cost + CHAR_SKIP_PENALTY,
      parent: bestBoundingPath.parent,
      wasUnmatchedChar: true
    };

    boundingPaths.push(penaltyParent);
    // Sort in cost-ascending order.
    // As we're using negative log likelihood, smaller is better.
    // (The closer to log_2(1) = 0, the better.)
    boundingPaths.sort((a, b) => a.cost - b.cost);

    const bestBound = boundingPaths[0];
    const successorPath: DictBreakerPath = {
      boundaryIndex: i,
      traversal: dictRoot,
      cost: bestBound.cost,
      parent: bestBound
    }

    bestBoundingPath = successorPath;
    paths.push(successorPath);

    // 3d. We now shift to the next loop iteration; we use the descendant `paths` set.
    activePaths = paths;
  }

  // 4. When all iterations are done, determine the lowest-cost path that
  //    remains, without regard to if it supports a word-boundary.
  //
  // If we happen to end on a potential word-boundary, opt for that one.  If two
  // match aside from boundaryIndex, take the lesser.  It comes first, BTW, so
  // stable-sorts auto-resolve this.
  activePaths.sort((a, b) => (a.cost - b.cost));
  const winningPath = activePaths[0];

  // 5. Build the spans.
  const spans: (Span & { codepointLength: number, unmatched: boolean })[] = [];
  const pathAsArray: DictBreakerPath[] = [];

  let rewindPath = winningPath;
  while(rewindPath) {
    const start = rewindPath.boundaryIndex+1;
    const end = codepointArr.length; // consistent because of the effects from the splice below
    const text = codepointArr.splice(start, end - start).join('');

    pathAsArray.unshift(rewindPath);
    spans.unshift({
      start: start,  // currently in code points; we'll correct it on the next pass.
      end: end, // same.
      length: text.length, // Span spec:  in code units
      text: text,
      codepointLength: end - start,
      unmatched: !!rewindPath.wasUnmatchedChar
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

    const trueSpan: typeof spans[0] = {
      ...baseSpan,
      start: start,
      end: totalLength
    };

    spans[i] = trueSpan;
  }

  // If all we had was whitespace, hence no spans, return.
  if(spans.length == 0) {
    return spans;
  }

  // 7. Span pass 3:  identify continuous penalty spans.  Why split into separate spans
  // when we can merge all the bits we can't recognize as a big lump instead?
  // - Looks nicer in the unit tests, if nothing else.
  // - Has _far_ better potential for 'learning' down the line.

  let spanBucket: Span[] = [];
  const finalSpans: Span[] = [];

  function mergeBucket(spanBucket: Span[]) {
    if(spanBucket.length > 0) {
      const startSpan = spanBucket[0];
      const endSpan = spanBucket[spanBucket.length - 1];
      //

      finalSpans.push({
        start: startSpan.start,
        end: endSpan.end,
        length: endSpan.end - startSpan.start,
        text: spanBucket.map((entry) => entry.text).join('')
      });
    }
  }

  for(const span of spans) {
    if(span.codepointLength == 1 && span.unmatched) {
      spanBucket.push(span);
    } else {
      mergeBucket(spanBucket);
      spanBucket = [];
      finalSpans.push(span);
    }
  }

  mergeBucket(spanBucket);

  // ... and done!
  return finalSpans;

  /*
    So...

    O(N), N = length of string:  loop over each codepoint
      O(N [worst-case]): loop over each still-valid traversal
        - at most, matches the index of the current codepoint
        - in practice, will be NOTABLY fewer after the first few codepoints.
        O(1): check if the Traversal can continue with the new incoming char.
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

  // return [];
}