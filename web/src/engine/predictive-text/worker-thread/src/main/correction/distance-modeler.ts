import { SENTINEL_CODE_UNIT } from '@keymanapp/models-templates';
import { PriorityQueue } from '@keymanapp/web-utils';

import { LexicalModelTypes } from '@keymanapp/common-types';

import { ClassicalDistanceCalculation } from './classical-calculation.js';
import { ExecutionTimer, STANDARD_TIME_BETWEEN_DEFERS } from './execution-timer.js';
import { QUEUE_NODE_COMPARATOR, SearchQuotientSpur } from './search-quotient-spur.js';
import { PathResult } from './search-quotient-node.js';
import { subsetByChar, subsetByInterval, mergeSubset, TransformSubset } from '../transform-subsets.js';

import Distribution = LexicalModelTypes.Distribution;
import LexiconTraversal = LexicalModelTypes.LexiconTraversal;
import ProbabilityMass = LexicalModelTypes.ProbabilityMass;
import Transform = LexicalModelTypes.Transform;
import TransformUtils from '#./transformUtils.js';

type RealizedInput = ProbabilityMass<Transform>[];  // NOT Distribution - they're masses from separate distributions.

// p = 1 / (e^4) = 0.01831563888.  This still exceeds many neighboring keys!
// p = 1 / (e^5) = 0.00673794699.  Strikes a good balance.
// Should easily give priority to neighboring keys before edit-distance kicks in (when keys are a bit ambiguous)
export const EDIT_DISTANCE_COST_SCALE = 5;
export const MIN_KEYSTROKE_PROBABILITY = 0.0001;

export type TraversableToken<TUnit> = {
  key: TUnit,
  traversal: LexiconTraversal
}

enum TimedTaskTypes {
  CACHED_RESULT = 0,
  PREDICTING = 1,
  CORRECTING = 2
}

/**
 * This type models a partially-processed subset of Transforms to be processed
 * as a batch due to sharing similar properties.
 */
export interface PartialSearchEdge {
  /**
   * A set of `Transform`s all sharing matching, batchable properties for the full portion
   * that has been processed, including the portions of the `.insert` property preceding
   * `subsetSubindex` but not those after.
   */
  transformSubset: TransformSubset<number>;

  /**
   * Indicates the depth in the insert string of the most recently added input
   * that should next be incorporated into the search path.
   */
  subsetSubindex: number;

  /**
   * Indicates whether the subset is for a substitution/match edge pattern (true) or
   * a deletion pattern (false) of the dynamic search-graph construction.
   */
  doSubsetMatching: boolean;
}

// Represents a processed node for the correction-search's search-space's
// tree-like graph.  May represent internal and 'leaf' nodes on said graph, as
// well as the overall root of the search.  Also used to represent edges on the
// graph TO said nodes - there's a bit of overloading here.  Either way, it
// stores the cost of the optimum path used to reach the node.
//
// For cases where incoming transforms have multiple inserted characters, this
// class can step through the characters, one at a time, in an
// efficiently-batched manner. Also of note:  this class will use the "sentinel"
// character in the calculation input sequence for any path where the input does
// not match the corresponding character from the lexicon, even for deletions -
// batching _mismatches_ efficiently.
//
// The stored path cost may be an overestimate when the edit distance is greater
// than the current search threshold.  The first version of the node to be
// dequeued from SearchSpace's priority queue hierarchy 'wins' and is taken as
// the absolute minimum; subsequent versions are ignored as suboptimal.
//
// Provides functions usable to enumerate across the node's outward edges to new
// nodes for continued search. Most of the actual calculations occur as part of
// this process.
//
// For nodes with raw edit-distance cost within the current threshold for
// correction searches, we do have admissibility. If not enough nodes are
// available within that threshold, however, admissibility may be lost, leaving
// our search as a heuristic.

/**
 * Represents a step in a correction-search path used to match potential input sequences
 * against entries in the active LexicalModel's lexicon and the functionality used to take
 * more steps until valid search endpoints are reached.
 */
export class SearchNode {
  /**
   * The search-term keying method used by the active LexicalModel
   * @param str
   * @returns
   */
  readonly toKey: (wordform: string) => string = str => str;

  /**
   * Calculations used to determine the edit-distance required for the path represented by
   * this SearchNode instance.
   */
  calculation: ClassicalDistanceCalculation<string>;

  /**
   * The Traversals (2d lexicon iterator) representing each prior step into the
   * lexicon for the prefix currently represented by this SearchNode instance's
   * represented search path.
   */
  matchedTraversals: LexiconTraversal[];

  /**
   * The actual Transform input sequence being considered as a potential correction.
   */
  priorInput: RealizedInput;

  /**
   * When defined, indicates that this instance models a set of transforms that have not
   * yet been fully input into the search path.
   */
  private partialEdge?: PartialSearchEdge;

  /**
   * Internal lazy-cache for .inputSamplingCost; it's a bit expensive to re-compute.
   */
  private _inputCost?: number;

  /**
   * A unique identifier corresponding to the earliest SearchPath containing
   * the correction-search graph edge represented by this instance.
   *
   * Corresponding search results will be tagged with this, which can be used
   * to identify the result's original source tokenization.
   */
  readonly spaceId: number;

  /**
   * Notes if the most recent edit processed in the node's represented search path
   * was not a deletion.  Root nodes are considered to meet this condition.
   */
  readonly nonDeletion: boolean;

  constructor(rootTraversal: LexiconTraversal, spaceId: number, toKey?: (arg0: string) => string);
  constructor(node: SearchNode, spaceId?: number, isSubstitution?: boolean);
  constructor(param1: LexiconTraversal | SearchNode, spaceId?: number, param2?: boolean | ((arg0: string) => string)) {
    if(param1 instanceof SearchNode) {
      const priorNode = param1;

      Object.assign(this, priorNode);
      if(this.partialEdge) {
        this.partialEdge = Object.assign({}, this.partialEdge);
      }
      this.priorInput = priorNode.priorInput.slice(0);
      this.matchedTraversals = priorNode.matchedTraversals.slice();

      this.nonDeletion = param2 as boolean;

      // Do NOT copy over _inputCost; this is a helper-constructor for methods
      // building new nodes... which will have a different cost.
      delete this._inputCost;

      // This is unique at each level, though it will reuse a previous ID if no new
      // one is provided (say, for 'insert' edits).
      this.spaceId = spaceId ?? priorNode.spaceId;
    } else {
      this.calculation = new ClassicalDistanceCalculation();
      this.matchedTraversals = [param1];
      this.priorInput = [];
      const toKey = param2 as ((arg0: string) => string);
      this.toKey = toKey || (x => x);
      this.spaceId = spaceId;
      this.nonDeletion = true;
    }
  }

  /**
   * Returns the number of raw text edits (insertions, deletions, replacements) coerced
   * by the correction-search in order to match the input with the lexical path represented
   * by the current node.
   */
  get editCount(): number {
    return this.calculation.getHeuristicFinalCost();
  }

  /**
   * Indicates this search node has only processed _part_ of a recent input set; no new
   * inputs should be received or processed while this returns `true`.
   */
  get hasPartialInput(): boolean {
    return !!this.partialEdge;
  }

  /**
   * The Traversal (2d lexicon iterator) representing the lexicon's contents for
   * the prefix currently represented by this SearchNode instance's represented
   * search path.
   */
  get currentTraversal(): LexiconTraversal {
    return this.matchedTraversals[this.matchedTraversals.length - 1];
  }

  /**
   * Returns the effective "cost" for choosing the inputs leading to the current node.
   * The less likely the keystroke sequence, the higher the cost.
   */
  get inputSamplingCost(): number {
    if(this._inputCost !== undefined) {
      return this._inputCost;
    } else {
      let MIN_P = MIN_KEYSTROKE_PROBABILITY;
      // Should technically re-normalize the sampling distribution.
      // -ln(p) is smaller for larger probabilities, as ln(p) is always <= 0.  Approaches infinity as p => 0.

      // TODO:  probably more efficient to instead use actual 'probability space'... but that'll involve extra changes.
      this._inputCost = this.priorInput.map(mass => mass.p > MIN_P ? mass.p : MIN_P).reduce((previous, current) => previous - Math.log(current), 0);
      // For a partially-processed set, we do include the set's full modelled probability mass.
      if(this.partialEdge) {
        const mass = this.partialEdge.transformSubset.cumulativeMass;
        this._inputCost -= Math.log(mass > MIN_P ? mass : MIN_P);
      }
      return this._inputCost;
    }
  }

  // The part used to prioritize our search.
  /**
   * Returns the effective "cost" of the search-path leading to the current search node.
   * The correction search evaluates Nodes in cost-ascending order based on this property's
   * return value.
   */
  get currentCost(): number {
    // - We reintrepret 'known cost' as a psuedo-probability.
    //   - Noting that 1/e = 0.367879441, an edit-distance cost of 1 may be intepreted as -ln(1/e) - a log-space 'likelihood'.
    //     - Not exactly normalized, though.
    // That's a really, really high likelihood, thoough.
    //
    // At any rate, we can linearly scale the known-cost to have about whatever probability we want.
    // If we can state it as p = 1 / (c * e), note then that ln(c * e) = ln(c) + 1.  So, scale * (ln(c) + 1).
    // If we can state it as e^x, note that ln(e^x) = x * ln(e) = x - just scale by 'x'!

    // p = 1 / (e^4) = 0.01831563888.  This still exceeds many neighboring keys!
    // p = 1 / (e^5) = 0.00673794699.  Strikes a good balance.
    // Should easily give priority to neighboring keys before edit-distance kicks in (when keys are a bit ambiguous)
    return EDIT_DISTANCE_COST_SCALE * this.editCount + this.inputSamplingCost;
  }

  /**
   * Adds outbound paths from the current Node that model the insertion of a
   * character not seen in the input, as if the user accidentally skipped typing
   * it.  No new input will be expected, but the search will continue one
   * character deeper in the backing lexicon.
   * @returns An array of SearchNodes corresponding to lexical entries that are
   * prefixed with the lexicon entry represented by the current Node's
   * matchSequence text.
   */
  buildInsertionEdges(): SearchNode[] {
    if(this.hasPartialInput) {
      throw new Error("Invalid state:  will not take new input while still processing Transform subset");
    }

    // Do not create insertion nodes after an empty transform; only before.
    // "Before" and "after" are identical for empty transforms - why duplicate?
    //
    // Also, do not create insertion nodes directly after deletions; that's
    // essentially a substitution.  We'll have an equivalent edge already built
    // with higher-accuracy modeling.
    //
    // Following previous insertions is fine, as is following a proper
    // match/substitution.
    if(this.priorInput.length > 0) {
      const priorInput = this.priorInput[this.priorInput.length - 1].sample;
      if(TransformUtils.isEmpty(priorInput) || !this.nonDeletion) {
        return [];
      }
    }

    let edges: SearchNode[] = [];

    for(let lexicalChild of this.currentTraversal.children()) {
      let childCalc = this.calculation.addMatchChar(lexicalChild.char);

      let searchChild = new SearchNode(this, this.spaceId, true);
      searchChild.calculation = childCalc;
      searchChild.priorInput = this.priorInput;
      searchChild.matchedTraversals.push(lexicalChild.traversal());

      edges.push(searchChild);
    }

    return edges;
  }

  /**
   * This method is used while stepping through intermediate deletion 'edges'
   * for transforms with multi-character insert strings.
   * @returns
   */
  private processDeletionSubset(): SearchNode {
    // We're on easy street:  all transforms are already essentially merged into
    // a single mass here.
    let calculation = this.calculation.addInputChar(SENTINEL_CODE_UNIT);

    // If on a 'delete' style path, we just build out the paths into a single
    // merged path.
    const node = new SearchNode(this);
    node.calculation = calculation;
    // no update to the match string or traversal to be found here...
    node.partialEdge.subsetSubindex++;
    return node;
  }

  /**
   * Finalizes the results of search nodes that represent the last step for
   * processing multi-character insert transforms.
   * @returns
   */
  private tryFinalize() {
    const subset = this.partialEdge?.transformSubset;
    if(!subset || subset.key > this.partialEdge.subsetSubindex) {
      // Not yet ready for finalization.  Just exit.
      return this;
    }

    // Finalization time!  We can safely transition the result node out
    // of 'subset' mode.
    delete this.partialEdge;

    // Whatever entries are in the subset, they actually resolve down to the
    // same net edit, as specified here.  It's more efficient to build the
    // transform insert string on the subset, rather than mass-editing a group
    // at each step and then consolidating them at the end.
    this.priorInput.push({
      sample: { insert: subset.insert, deleteLeft: subset.entries[0]?.sample.deleteLeft ?? 0 },
      p: subset.cumulativeMass
    });
    return this;
  }

  /**
   * For nodes modeling partially-processed inputs (partway through a
   * multi-character insert Transform), this method will build the next step of
   * the search path, iterating one character deeper within the
   * partially-processed input.
   * @returns
   */
  processSubsetEdge(): SearchNode[] {
    const partialEdge = this.partialEdge;
    if(!partialEdge) {
      throw new Error("Invalid state:  not currently processing a Transform subset");
    }

    const startSubset = partialEdge.transformSubset;
    const subIndex = partialEdge.subsetSubindex;

    // For raw backspaces - if no insert string, we can already finalize!
    if(partialEdge.subsetSubindex >= partialEdge.transformSubset.key) {
      return [this.tryFinalize()];
    }

    if(!partialEdge.doSubsetMatching) {
      return [this.processDeletionSubset().tryFinalize()];
    }

    // After this, it's all substitution / matching.
    const traversal = this.currentTraversal;
    let nodesToReturn: SearchNode[] = [];
    let keySet: Set<string> = new Set();

    const subsetMap = subsetByChar(startSubset, subIndex, this.toKey);
    for(const [char, subset] of subsetMap.entries()) {
      // build new node for the next char.
      let calculation = this.calculation;
      let childTraversal: LexiconTraversal;
      if(char) {
        childTraversal = traversal.child(char);
        // These cases - where there's no match in the lexicon - are bundled
        // after this for-loop.
        //
        // ... except for when there ARE no children at all.  But, for those
        // cases... a substitution would be invalid - we can't substitute for
        // each char in the insert string, so abort.  It's better matched by an
        // 'insert' edge or a by different input from a sibling subset.
        if(!childTraversal) {
          continue;
        }

        calculation = calculation.addInputChar(char);
        calculation = calculation.addMatchChar(char);
      } // else we COULD bundle these as a single subset... but it's likely not that important.
      // after all, the cost ISN'T shifting, so we'll process 'em almost immediately and move on.

      keySet.add(char);

      const node = new SearchNode(this, this.spaceId, this.nonDeletion);
      node.calculation = calculation;
      node.partialEdge.subsetSubindex++;
      // Append the newly-processed char to the subset's `insert` string.
      node.partialEdge.transformSubset = {...subset, key: startSubset.key, insert: subset.insert + char};
      // '' inserts do not traverse deeper; do not add an empty traversal entry!
      if(childTraversal) {
        node.matchedTraversals.push(childTraversal);
      }
      nodesToReturn.push(node);
    };

    // These come at a notably higher cost (due to required edit) and are less likely to be processed.
    // It's best to batch them so we only need a single in-memory node to represent them, rather than
    // one node per transform / subset, which can scale very rapidly.
    let calculation = this.calculation.addInputChar(SENTINEL_CODE_UNIT);

    for(const child of traversal.children()) {
      const childTraversal = child.traversal();
      let childCalc =  calculation.addMatchChar(child.char);
      let childSubset: TransformSubset<number>;

      if(keySet.has(child.char)) {
        // OK, so we built a set that successfully matched this char.  Other paths that
        // emit a char after keying don't match it - we want to combine that path here.
        // If there are multiple 'insert' chars, missing on one shouldn't force a 'miss'
        // on the other subpaths that may follow this with valid char matches.
        const nonMatchSubsets = [...subsetMap.values()].filter(c => {
          // Ignore entries that 'key out' - there's no char left to act as a substitute.
          // Also ignore the entry that actually did match - it's a match, not substitute.
          return c.key != child.char && c.key != '';
        });
        childSubset = mergeSubset(nonMatchSubsets, startSubset.key);
      } else {
        // We didn't build any that match?  Guess nothing matched, then.
        // Clone so that our .insert tweak below does not have unintended effects.
        childSubset = {...startSubset};
      }

      // May happen in unit tests or when corrections are disabled.
      if(!childSubset.entries.length) {
        continue;
      }

      // Append a match-failure marker for the non-matching char set onto the subset's
      // `insert` string.
      childSubset.insert += SENTINEL_CODE_UNIT;

      const node = new SearchNode(this, this.spaceId, this.nonDeletion);
      node.calculation = childCalc;
      node.matchedTraversals.push(childTraversal);
      node.partialEdge.subsetSubindex++;
      node.partialEdge.transformSubset = childSubset;
      nodesToReturn.push(node);
    }

    return nodesToReturn.map(n => n.tryFinalize());
  }

  /**
   * Called by `buildDeletionEdges` and `buildSubstitutionEdges` to construct
   * intermediate TransformSubset-based nodes that extend the search path one
   * step into the incoming input transforms in an efficiently-batched manner.
   *
   * When an incoming character cannot match the next character for the node's
   * represented lexicon prefix - be it due to not adding one (deletions) or
   * due to not being the same character, all mismatching cases are merged into
   * one, reducing the rate of expansion for the search graph.
   * @param dist
   * @param isSubstitution
   * @param edgeId
   * @returns
   */
  private setupSubsetProcessing(dist: Distribution<Transform>, isSubstitution: boolean, edgeId: number) {
    if(this.hasPartialInput) {
      throw new Error("Invalid state:  will not take new input while still processing Transform subset");
    }

    const edges: SearchNode[] = [];
    const subsets = subsetByInterval(dist);

    for(let dl = 0; dl < subsets.length; dl++) {
      const dlSubset = subsets[dl];
      if(!dlSubset) {
        continue;
      }

      const calc = this.calculation;
      const newMatchLength = Math.max(0, calc.matchSequence.length - dl);
      const edgeCalc = calc.getSubset(calc.inputSequence.length - dl, newMatchLength);

      for(let ins = 0; ins < dlSubset.length; ins++) {
        const insSubset = dlSubset[ins];
        if(!insSubset) {
          continue;
        }

        const node = new SearchNode(this, edgeId, isSubstitution);
        node.calculation = edgeCalc;
        node.partialEdge = {
          doSubsetMatching: isSubstitution,
          subsetSubindex: 0,
          transformSubset: isSubstitution ? insSubset : {
            ...insSubset,
            entries: [ { sample: { insert: SENTINEL_CODE_UNIT.repeat(ins), deleteLeft: dl }, p: insSubset.cumulativeMass}]
          }
        }
        // Get the traversal at the new end location.  (Root is always at index 0.)
        node.matchedTraversals = this.matchedTraversals.slice(0, newMatchLength+1);

        edges.push(node);
      }
    }

    return edges;
  }

  /**
   * Adds paths that consider the next Transform from the input without matching it to
   * extended paths from the lexicon.  This aims to model (and ignore) when the user
   * accidentally double-taps an input key by not extending the match string alongside
   * the input.
   *
   * @returns An array of SearchNodes corresponding to search paths that skip the next
   * input keystroke.
   */
  buildDeletionEdges(dist: Distribution<Transform>, edgeId: number): SearchNode[] {
    return this.setupSubsetProcessing(dist, false, edgeId);
  }

  /**
   * Adds paths that seek to:
   * 1.  Match the next input transform in the sequence with matching prefixes
   * 2.  Substitute one (or more) characters from the next input transform
   * from the lexicon to better match viable lexicon prefixes
   *
   * @returns An array of SearchNodes corresponding to search paths that match or
   * replace the next currently-unprocessed input.
   */
  buildSubstitutionEdges(dist: Distribution<Transform>, edgeId: number): SearchNode[] {
    // Note:  due to the batching approach used via TransformSubsets,
    // substitutions are _not_ adequately represented by one 'insertion' + one
    // 'deletion' step. Explicit substitution / match-oriented processing is
    // required.
    return this.setupSubsetProcessing(dist, true, edgeId);
  }

  /**
   * A key uniquely identifying identical match sequences.  Reaching the same net result
   * by different paths should result in identical `resultKey` values.
   */
  get resultKey(): string {
    // Filter out any duplicated match sequences.  The same match sequence may be reached via
    // different input sequences, after all.
    return this.calculation.matchSequence.join('');
  }

  /**
   * If the known edit-distance cost is equal to the input length and non-zero, this means
   * that literally every input has been full-on replaced.  Thus, this is likely not a good
   * 'root' to use for predictions.
   */
  get isFullReplacement(): boolean {
    // Logic exception:  0 cost, 0 length != a "replacement".
    return (!!this.editCount) && this.editCount == this.priorInput.length;
  }
}

export class SearchResult {
  private resultNode: SearchNode;

  constructor(node: SearchNode) {
    this.resultNode = node;
  }

  get inputSequence(): ProbabilityMass<Transform>[] {
    return this.resultNode.priorInput;
  }

  get matchSequence(): TraversableToken<string>[] {
    return this.resultNode.calculation.matchSequence.map((char, i) => ({key: char, traversal: this.resultNode.matchedTraversals[i+1]}));
  };

  get matchString(): string {
    return this.resultNode.resultKey;
  }

  /**
   * Gets the number of Damerau-Levenshtein edits needed to reach the node's
   * matchString from the output induced by the input sequence used to reach it.
   *
   * (This is scaled by `SearchSpace.EDIT_DISTANCE_COST_SCALE` when included in
   * `totalCost`.)
   */
  get knownCost(): number {
    return this.resultNode.editCount;
  }

  /**
   * Gets the "input sampling cost" of the edge, which should be considered as the
   * negative log-likelihood of the input path taken to reach the node.
   */
  get inputSamplingCost(): number {
    return this.resultNode.inputSamplingCost;
  }

  /**
   * Gets the "total cost" of the edge, which should be considered as the
   * negative log-likelihood of the input path taken to reach the node
   * multiplied by the 'probability' induced by needed Damerau-Levenshtein edits
   * to the resulting output.
   */
  get totalCost(): number {
    return this.resultNode.currentCost;
  }

  get finalTraversal(): LexiconTraversal {
    return this.resultNode.currentTraversal;
  }

  get spaceId(): number {
    return this.resultNode.spaceId;
  }
}

// Current best guesstimate of how compositor will retrieve ideal corrections.
export async function *getBestMatches(searchSpace: SearchQuotientSpur, timer: ExecutionTimer): AsyncGenerator<SearchResult> {
  let currentReturns: {[resultKey: string]: SearchNode} = {};

  // Stage 1 - if we already have extracted results, build a queue just for them and iterate over it first.
  const returnedValues = Object.values(searchSpace.returnedValues);
  if(returnedValues.length > 0) {
    let preprocessedQueue = new PriorityQueue<SearchNode>(QUEUE_NODE_COMPARATOR, returnedValues);

    while(preprocessedQueue.count > 0) {
      const entryFromCache = timer.time(() => {
        let entry = preprocessedQueue.dequeue();

        // Is the entry a reasonable result?
        if(entry.isFullReplacement) {
          // If the entry's 'match' fully replaces the input string, we consider it
          // unreasonable and ignore it.
          return null;
        }

        currentReturns[entry.resultKey] = entry;
        // Do not track yielded time.
        return new SearchResult(entry);
      }, TimedTaskTypes.CACHED_RESULT);

      if(entryFromCache) {
        // Time yielded here is generally spent on turning corrections into predictions.
        // It's timing a different sort of task, so... different task set ID.
        const timeSpan = timer.start(TimedTaskTypes.PREDICTING);
        yield entryFromCache;
        timeSpan.end();

        if(timer.timeSinceLastDefer > STANDARD_TIME_BETWEEN_DEFERS) {
          await timer.defer();
        }
      }
    }
  }

  // Stage 2:  the fun part; actually searching!
  do {
    const entry = timer.time(() => {
      let newResult: PathResult = searchSpace.handleNextNode();

      if(newResult.type == 'none') {
        return null;
      } else if(newResult.type == 'complete') {
        const node = newResult.finalNode;

        // Is the entry a reasonable result?
        if(node.isFullReplacement) {
          // If the entry's 'match' fully replaces the input string, we consider it
          // unreasonable and ignore it.  Also, if we've reached this point...
          // we can(?) assume that everything thereafter is as well.
          return null;
        }

        // As we can't guarantee a monotonically-increasing cost during the search -
        // due to effects from keystrokes with deleteLeft > 0 - it's technically
        // possible to find a lower-cost path later in such cases.
        //
        // If it occurs, we should re-emit it - it'll show up earlier in the
        // suggestions that way, as it should.
        if((currentReturns[node.resultKey]?.currentCost ?? Number.MAX_VALUE) > node.currentCost) {
          currentReturns[node.resultKey] = node;
          // Do not track yielded time.
          return new SearchResult(node);
        }
      }

      return null;
    }, TimedTaskTypes.CORRECTING);

    if(entry) {
      const timeSpan = timer.start(TimedTaskTypes.PREDICTING);
      yield entry;
      timeSpan.end();
    }

    if(timer.timeSinceLastDefer > STANDARD_TIME_BETWEEN_DEFERS) {
      await timer.defer();
    }
  } while(!timer.elapsed && searchSpace.currentCost < Number.POSITIVE_INFINITY);

  return null;
}
