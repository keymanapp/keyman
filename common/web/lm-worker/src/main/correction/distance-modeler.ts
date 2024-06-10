import { Comparator, isHighSurrogate, SENTINEL_CODE_UNIT, PriorityQueue } from '@keymanapp/models-templates';
import { SearchBatcher } from './search-batcher.js';

import { ClassicalDistanceCalculation, EditToken } from './classical-calculation.js';

type RealizedInput = ProbabilityMass<Transform>[];  // NOT Distribution - they're masses from separate distributions.

export type TraversableToken<TUnit> = {
  key: TUnit,
  traversal: LexiconTraversal
}

export const QUEUE_NODE_COMPARATOR: Comparator<SearchNode> = function(arg1, arg2) {
  return arg1.currentCost - arg2.currentCost;
}

// Represents a processed node for the correction-search's search-space's tree-like graph.  May represent
// internal and 'leaf' nodes on said graph, as well as the overall root of the search.  Also used to represent
// edges on the graph TO said nodes - there's a bit of overloading here.  Either way, it stores the cost of the
// optimum path used to reach the ndoe.
//
// The stored path cost may be an overestimate when the edit distance is greater than the current search threshold.  The
// first version of the node to be dequeued from SearchSpace's priority queue hierarchy 'wins' and is taken as the absolute
// minimum; subsequent versions are ignored as suboptimal.
//
// Provides functions usable to enumerate across the node's outward edges to new nodes for continued search.
// Most of the actual calculations occur as part of this process.
//
// For nodes with raw edit-distance cost within the current threshold for correction searches, we do have admissibility.
// If not enough nodes are available within that threshold, however, admissibility may be lost, leaving our search as a
// heuristic.
//
export class SearchNode {
  calculation: ClassicalDistanceCalculation<string, EditToken<string>, TraversableToken<string>>;

  currentTraversal: LexiconTraversal;
  toKey: (wordform: USVString) => USVString = str => str;
  priorInput: RealizedInput;

  // Internal lazy-cache for .inputSamplingCost, as it's a bit expensive to re-compute.
  private _inputCost?: number;

  constructor(rootTraversal: LexiconTraversal, toKey?: (arg0: USVString) => USVString);
  constructor(node: SearchNode);
  constructor(rootTraversal: LexiconTraversal | SearchNode, toKey?: (arg0: USVString) => USVString) {
    toKey = toKey || (x => x);

    if(rootTraversal instanceof SearchNode) {
      let priorNode = rootTraversal;
      this.calculation = priorNode.calculation;
      this.currentTraversal = priorNode.currentTraversal;
      this.priorInput = priorNode.priorInput;
      this.toKey = priorNode.toKey;
      // Do NOT copy over _inputCost; this is a helper-constructor for methods
      // building new nodes... which will have a different cost.
    } else {
      this.calculation = new ClassicalDistanceCalculation();
      this.currentTraversal = rootTraversal;
      this.priorInput = [];
      this.toKey = toKey;
    }
  }

  get knownCost(): number {
    return this.calculation.getHeuristicFinalCost();
  }

  get inputSamplingCost(): number {
    if(this._inputCost !== undefined) {
      return this._inputCost;
    } else {
      let MIN_P = SearchSpace.MIN_KEYSTROKE_PROBABILITY;
      // Should technically re-normalize the sampling distribution.
      // -ln(p) is smaller for larger probabilities, as ln(p) is always <= 0.  Approaches infinity as p => 0.

      // TODO:  probably more efficient to instead use actual 'probability space'... but that'll involve extra changes.
      this._inputCost = this.priorInput.map(mass => mass.p > MIN_P ? mass.p : MIN_P).reduce((previous, current) => previous - Math.log(current), 0);
      return this._inputCost;
    }
  }

  // The part used to prioritize our search.
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
    return SearchSpace.EDIT_DISTANCE_COST_SCALE * this.knownCost + this.inputSamplingCost;
  }

  buildInsertionEdges(): SearchNode[] {
    let edges: SearchNode[] = [];

    for(let lexicalChild of this.currentTraversal.children()) {
      let traversal = lexicalChild.traversal();
      let matchToken = {
        key: lexicalChild.char,
        traversal: traversal
      }

      let childCalc = this.calculation.addMatchChar(matchToken);

      let searchChild = new SearchNode(this);
      searchChild.calculation = childCalc;
      searchChild.priorInput = this.priorInput;
      searchChild.currentTraversal = traversal;

      edges.push(searchChild);
    }

    return edges;
  }

  buildDeletionEdges(inputDistribution: Distribution<Transform>): SearchNode[] {
    let edges: SearchNode[] = [];

    /*
      * If the probability of an input is less than the highest probability * the base edit-distance likelihood,
      * don't build an edge for it; just rely on edits from the highest-probability edge.
      *
      * We may be able to be stricter, but this should be a decent start.
      *
      * Note:  thanks to ModelCompositor.predict, we know the distribution is pre-sorted.
      */
    for(let probMass of inputDistribution) {
      if(probMass.p < inputDistribution[0].p * Math.exp(-SearchSpace.EDIT_DISTANCE_COST_SCALE)) {
        // Again, we're pre-sorted.  All further entries will be too low-cost to consider.
        break;
      }

      let edgeCalc = this.calculation;
      let transform = probMass.sample;
      if(transform.deleteLeft) {
        edgeCalc = edgeCalc.getSubset(edgeCalc.inputSequence.length - transform.deleteLeft, edgeCalc.matchSequence.length);
      }

      // TODO:  transform.deleteRight currently not supported.

      let inputPath = this.priorInput.slice(0);
      inputPath.push(probMass);
      // Tokenize and iterate over input chars, adding them into the calc.
      for(let i=0; i < transform.insert.length; i++) {
        let char = transform.insert[i];
        if(isHighSurrogate(char)) {
          i++;
          char = char + transform.insert[i];
        }

        // In case of NFD input, filter out any empty-strings that may arise
        // when 'keying' raw diacritics.
        let keyedChar = this.toKey(char);
        if(keyedChar) {
          edgeCalc = edgeCalc.addInputChar({key: keyedChar});
        }
      }

      let childEdge = new SearchNode(this);
      childEdge.calculation = edgeCalc;
      childEdge.priorInput = inputPath;

      edges.push(childEdge);
    }

    return edges;
  }

  // While this may SEEM to be unnecessary, note that sometimes substitutions (which are computed
  // via insert + delete) may be lower cost than both just-insert and just-delete.
  buildSubstitutionEdges(inputDistribution: Distribution<Transform>): SearchNode[] {
    // Handles the 'input' component.
    let intermediateEdges = this.buildDeletionEdges(inputDistribution);
    let edges: SearchNode[] = [];

    for(let lexicalChild of this.currentTraversal.children()) {
      for(let edge of intermediateEdges) {
        let traversal = lexicalChild.traversal();
        let matchToken = {
          key: lexicalChild.char,
          traversal: traversal
        }

        let childCalc = edge.calculation.addMatchChar(matchToken);

        let searchChild = new SearchNode(this);
        searchChild.calculation = childCalc;
        searchChild.priorInput = edge.priorInput;
        searchChild.currentTraversal = traversal;

        edges.push(searchChild);
      }
    }

    return edges;
  }

  get mapKey(): string {
    let inputString = this.priorInput.map((value) => '+' + value.sample.insert + '-' + value.sample.deleteLeft).join('');
    let matchString =  this.calculation.matchSequence.map((value) => value.key).join('');

    // TODO:  might should also track diagonalWidth.
    return inputString + SENTINEL_CODE_UNIT + matchString;
  }

  get isFullReplacement(): boolean {
    // If the known edit-distance cost is equal to the input length, this means
    // that literally every input has been full-on replaced.  Thus, this is
    // likely not a good 'root' to use for predictions.
    //
    // Logic exception:  0 cost, 0 length != a "replacement".
    return this.knownCost && this.knownCost == this.priorInput.length;
  }
}

class SearchSpaceTier {
  correctionQueue: PriorityQueue<SearchNode>;
  processed: SearchNode[] = [];
  index: number;

  constructor(instance: SearchSpaceTier);
  constructor(index: number, initialEdges?: SearchNode[]);
  constructor(arg1: number | SearchSpaceTier, initialEdges?: SearchNode[]) {
    if(typeof arg1 == 'number') {
      this.index = arg1;
      this.correctionQueue = new PriorityQueue<SearchNode>(QUEUE_NODE_COMPARATOR, initialEdges);
      return;
    } else {
      this.index = arg1.index;
      this.processed = [].concat(arg1.processed);
      this.correctionQueue = new PriorityQueue(arg1.correctionQueue);
    }
  }

  increaseMaxEditDistance() {
    // By extracting the entries from the priority queue and increasing distance outside of it as a batch job,
    // we get an O(N) implementation, rather than the O(N log N) that would result from maintaining the original queue.
    let entries = this.correctionQueue.toArray();

    entries.forEach(function(edge) { edge.calculation = edge.calculation.increaseMaxDistance(); });

    // Since we just modified the stored instances, and the costs may have shifted, we need to re-heapify.
    this.correctionQueue = new PriorityQueue<SearchNode>(QUEUE_NODE_COMPARATOR, entries);
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

  get matchSequence(): TraversableToken<USVString>[] {
    return this.resultNode.calculation.matchSequence;
  };

  get matchString(): USVString {
    return this.matchSequence.map(value => value.key).join('');
  }

  get knownCost(): number {
    return this.resultNode.knownCost;
  }

  get inputSamplingCost(): number {
    return this.resultNode.inputSamplingCost;
  }

  get totalCost(): number {
    return this.resultNode.currentCost;
  }

  get finalTraversal(): LexiconTraversal {
    return this.resultNode.currentTraversal;
  }
}

type NullPath = {
  type: 'none'
}

type IntermediateSearchPath = {
  type: 'intermediate',
  cost: number
}

type CompleteSearchPath = {
  type: 'complete',
  cost: number,
  finalNode: SearchNode
}

type PathResult = NullPath | IntermediateSearchPath | CompleteSearchPath;

// The set of search spaces corresponding to the same 'context' for search.
// Whenever a wordbreak boundary is crossed, a new instance should be made.
export class SearchSpace {
  private QUEUE_SPACE_COMPARATOR: Comparator<SearchSpaceTier>;

  // p = 1 / (e^4) = 0.01831563888.  This still exceeds many neighboring keys!
  // p = 1 / (e^5) = 0.00673794699.  Strikes a good balance.
  // Should easily give priority to neighboring keys before edit-distance kicks in (when keys are a bit ambiguous)
  static readonly EDIT_DISTANCE_COST_SCALE = 5;
  static readonly MIN_KEYSTROKE_PROBABILITY = 0.0001;
  static readonly DEFAULT_ALLOTTED_CORRECTION_TIME_INTERVAL = 33; // in milliseconds.

  private tierOrdering: SearchSpaceTier[] = [];
  private selectionQueue: PriorityQueue<SearchSpaceTier>;
  private inputSequence: Distribution<Transform>[] = [];
  private minInputCost: number[] = [];
  private rootNode: SearchNode;

  // We use an array and not a PriorityQueue b/c batch-heapifying at a single point in time
  // is cheaper than iteratively building a priority queue.
  private completedPaths: SearchNode[];

  // Marks all results that have already been returned since the last input was received.
  private returnedValues: {[mapKey: string]: SearchNode} = {};

  // Signals that the edge has already been processed.
  private processedEdgeSet: {[mapKey: string]: boolean} = {};

  /**
   * Clone constructor.  Deep-copies its internal queues, but not search nodes.
   * @param instance
   */
  constructor(instance: SearchSpace);
  /**
   * Constructs a fresh SearchSpace instance for used in predictive-text correction
   * and suggestion searches.
   * @param model
   */
  constructor(model: LexicalModel);
  constructor(arg1: SearchSpace|LexicalModel) {
    // Constructs the priority-queue comparator-closure needed for determining which
    // tier should be searched next.
    this.buildQueueSpaceComparator();

    if(arg1 instanceof SearchSpace) {
      this.inputSequence = [].concat(arg1.inputSequence);
      this.minInputCost = [].concat(arg1.minInputCost);
      this.rootNode = arg1.rootNode;
      this.completedPaths = [].concat(arg1.completedPaths);
      this.returnedValues = {...arg1.returnedValues};
      this.processedEdgeSet = {...arg1.processedEdgeSet};

      this.tierOrdering   = arg1.tierOrdering.map((tier) => new SearchSpaceTier(tier));
      this.selectionQueue = new PriorityQueue(this.QUEUE_SPACE_COMPARATOR, this.tierOrdering);
      return;
    }

    const model = arg1;
    if(!model) {
      throw "The LexicalModel parameter must not be null / undefined.";
    } else if(!model.traverseFromRoot) {
      throw "The provided model does not implement the `traverseFromRoot` function, which is needed to support robust correction searching.";
    }

    this.selectionQueue = new PriorityQueue<SearchSpaceTier>(this.QUEUE_SPACE_COMPARATOR);
    this.rootNode = new SearchNode(model.traverseFromRoot(), model.toKey ? model.toKey.bind(model) : null);

    this.completedPaths = [this.rootNode];

    // Adds a base level queue to handle initial insertions.
    // Start with _just_ the root node.  Necessary for proper empty-token, empty-input handling!
    let baseTier = new SearchSpaceTier(0, [this.rootNode]);
    this.tierOrdering.push(baseTier);
    this.selectionQueue.enqueue(baseTier);
  }

  private buildQueueSpaceComparator() {
    let searchSpace = this;

    this.QUEUE_SPACE_COMPARATOR = function(space1, space2) {
      let node1 = space1.correctionQueue.peek();
      let node2 = space2.correctionQueue.peek();

      let index1 = space1.index;
      let index2 = space2.index;

      let tierMinCost: number = 0;
      let sign = 1;

      if(index2 < index1) {
        let temp = index2;
        index2 = index1;
        index1 = temp;

        sign = -1;
      }

      // Boost the cost of the lower tier by the minimum cost possible for the missing inputs between them.
      // In essence, compare the nodes as if the lower tier had the most likely input appended for each such
      // input missing at the lower tier.
      //
      // A 100% admissible heuristic to favor a deeper search, since the added cost is guaranteed if the path
      // is traversed further.
      //
      // Remember, tier index i's last used input was from input index i-1.
      // As a result, i is the first needed input index, with index2 - 1 the last entry needed to match them.
      for(let i=index1; i < index2; i++) {
        tierMinCost = tierMinCost + searchSpace.minInputCost[i];
      }

      // Guards, just in case one of the search spaces ever has an empty node.
      if(node1 && node2) {
        // If node1 is lower-tier, node1 is the one in need of boosted cost.
        // `sign` flips it when node2 is lower tier.
        return node1.currentCost - node2.currentCost + sign * tierMinCost;
      } else if(node2) {
        return 1;
      } else {
        return -1;
      }
    }
  }

  increaseMaxEditDistance() {
    this.tierOrdering.forEach(function(tier) { tier.increaseMaxEditDistance() });
  }

  addInput(inputDistribution: Distribution<Transform>) {
    this.inputSequence.push(inputDistribution);

    // Assumes that `inputDistribution` is already sorted.
    this.minInputCost.push(-Math.log(inputDistribution[0].p));

    // With a newly-available input, we can extend new input-dependent paths from
    // our previously-reached 'extractedResults' nodes.
    let newlyAvailableEdges: SearchNode[] = [];
    let batches = this.completedPaths.map(function(node) {
      let deletions = node.buildDeletionEdges(inputDistribution);
      let substitutions = node.buildSubstitutionEdges(inputDistribution);

      return deletions.concat(substitutions);
    });

    // Don't forget to reset the array; the contained nodes no longer reach the search's end.
    this.completedPaths = [];
    this.returnedValues = {};

    batches.forEach(function(batch) {
      newlyAvailableEdges = newlyAvailableEdges.concat(batch);
    });

    // Now that we've built the new edges, we can efficiently construct the new search tier.
    let tier = new SearchSpaceTier(this.tierOrdering.length, newlyAvailableEdges);
    this.tierOrdering.push(tier);
    this.selectionQueue.enqueue(tier);
  }

  // TODO: will want eventually for reversions and/or backspaces
  removeLastInput() {
    // 1.  truncate all entries from that search tier; we need to 'restore' extractedResults to match
    //     the state that would have existed without the last search tier.
    // 2.  remove the last search tier.  Which may necessitate reconstructing the tier queue, but oh well.
  }

  private hasNextMatchEntry(): boolean {
    let topQueue = this.selectionQueue.peek();
    if(topQueue) {
      return topQueue.correctionQueue.count > 0;
    } else {
      return false;
    }
  }

  private handleNextNode(): PathResult {
    if(!this.hasNextMatchEntry()) {
      return { type: 'none' };
    }

    let bestTier = this.selectionQueue.dequeue();
    let currentNode = bestTier.correctionQueue.dequeue();

    let unmatchedResult: IntermediateSearchPath = {
      type: 'intermediate',
      cost: currentNode.currentCost
    }

    // Have we already processed a matching edge?  If so, skip it.
    // We already know the previous edge is of lower cost.
    if(this.processedEdgeSet[currentNode.mapKey]) {
      this.selectionQueue.enqueue(bestTier);
      return unmatchedResult;
    } else {
      this.processedEdgeSet[currentNode.mapKey] = true;
    }

    // Stage 1:  filter out nodes/edges we want to prune

    // Forbid a raw edit-distance of greater than 2.
    // Note:  .knownCost is not scaled, while its contribution to .currentCost _is_ scaled.
    let substitutionsOnly = false;
    if(currentNode.knownCost > 2) {
      return unmatchedResult;
    } else if(currentNode.knownCost == 2) {
      // Hard restriction:  no further edits will be supported.  This helps keep the search
      // more narrowly focused.
      substitutionsOnly = true;
    }

    let tierMinCost = 0;
    for(let i = 0; i <= bestTier.index; i++) {
      tierMinCost += this.minInputCost[i];
    }

    // Thresholds _any_ path, partially based on currently-traversed distance.
    // Allows a little 'wiggle room' + 2 "hard" edits.
    // Can be important if needed characters don't actually exist on the keyboard
    // ... or even just not the then-current layer of the keyboard.
    if(currentNode.currentCost > tierMinCost + 2.5 * SearchSpace.EDIT_DISTANCE_COST_SCALE) {
      return unmatchedResult;
    }

    // Stage 2:  build remaining edges

    // Always possible, as this does not require any new input.
    if(!substitutionsOnly) {
      let insertionEdges = currentNode.buildInsertionEdges();
      bestTier.correctionQueue.enqueueAll(insertionEdges);
    }

    if(bestTier.index == this.tierOrdering.length - 1) {
      // It was the final tier - store the node for future reference.
      this.completedPaths.push(currentNode);

      // Since we don't modify any other tier, we may simply reinsert the removed tier.
      this.selectionQueue.enqueue(bestTier);

      return {
        type: 'complete',
        cost: currentNode.currentCost,
        finalNode: currentNode
      };
    } else {
      // Time to construct new edges for the next tier!
      let nextTier = this.tierOrdering[bestTier.index+1];

      let inputIndex = nextTier.index;

      let deletionEdges: SearchNode[] = [];
      if(!substitutionsOnly) {
        deletionEdges       = currentNode.buildDeletionEdges(this.inputSequence[inputIndex-1]);
      }
      let substitutionEdges = currentNode.buildSubstitutionEdges(this.inputSequence[inputIndex-1]);

      // Note:  we're live-modifying the tier's cost here!  The priority queue loses its guarantees as a result.
      nextTier.correctionQueue.enqueueAll(deletionEdges.concat(substitutionEdges));

      // So, we simply rebuild the selection queue.
      this.selectionQueue = new PriorityQueue<SearchSpaceTier>(this.QUEUE_SPACE_COMPARATOR, this.tierOrdering);

      // We didn't reach an end-node, so we just end the iteration and continue the search.
    }

    // If we've somehow fully exhausted all search options, indicate that none remain.
    return unmatchedResult;
  }

  // Current best guesstimate of how compositor will retrieve ideal corrections.
  async *getBestMatches(waitMillis?: number): AsyncGenerator<SearchResult[]> {
    let maxTime: number;
    if(waitMillis == 0) {
      maxTime = Infinity;
    } else if(waitMillis == undefined || Number.isNaN(waitMillis)) { // also covers null.
      maxTime = SearchSpace.DEFAULT_ALLOTTED_CORRECTION_TIME_INTERVAL;
    } else  {
      maxTime = waitMillis;
    }

    /**
     * This inner class is designed to help the algorithm detect its active execution time.
     * While there's no official JS way to do this, we can approximate it by polling the
     * current system time (in ms) after each iteration of a short-duration loop.  Unusual
     * spikes in system time for a single iteration is likely to indicate that an OS
     * context switch occurred at some point during the iteration's execution.
     */
    class ExecutionTimer {
      /**
       * The system time when this instance was created.
       */
      private start: number;

      /**
       * Marks the system time at the start of the currently-running loop, as noted
       * by a call to the `startLoop` function.
       */
      private loopStart: number;

      private maxExecutionTime: number;
      private maxTrueTime: number;

      private executionTime: number;

      /**
       * Used to track intervals in which potential context swaps by the OS may
       * have occurred.  Context switches generally seem to pause threads for
       * at least 16 ms, while we expect each loop iteration to complete
       * within just 1 ms.  So, any possible context switch should have the
       * longest observed change in system time.
       *
       * See `updateOutliers` for more details.
       */
      private largestIntervals: number[] = [0];

      constructor(maxExecutionTime: number, maxTrueTime: number) {
        // JS measures time by the number of milliseconds since Jan 1, 1970.
        this.loopStart = this.start = Date.now();
        this.maxExecutionTime = maxExecutionTime;
        this.maxTrueTime = maxTrueTime;
      }

      startLoop() {
        this.loopStart = Date.now();
      }

      markIteration() {
        const now = Date.now();
        const delta = now - this.loopStart;
        this.executionTime += delta;

        /**
         * Update the list of the three longest system-time intervals observed
         * for execution of a single loop iteration.
         *
         * Ignore any zero-ms length intervals; they'd make the logic much
         * messier than necessary otherwise.
         */
        if(delta && delta > this.largestIntervals[0]) {
          // If the currently-observed interval is longer than the shortest of the 3
          // previously-observed longest intervals, replace it.
          if(this.largestIntervals.length > 2) {
            this.largestIntervals[0] = delta;
          } else {
            this.largestIntervals.push(delta);
          }

          // Puts the list in ascending order.  Shortest of the list becomes the head,
          // longest one the tail.
          this.largestIntervals.sort();

          // Then, determine if we need to update our outlier-based tweaks.
          this.updateOutliers();
        }
      }

      updateOutliers() {
        /* Base assumption:  since each loop of the search should evaluate within ~1ms,
          *                   notably longer execution times are probably context switches.
          *
          * Base assumption:  OS context switches generally last at least 16ms.  (Based on
          *                   a window.setTimeout() usually not evaluating for at least
          *                   that long, even if set to 1ms.)
          *
          * To mitigate these assumptions:  we'll track the execution time of every loop
          * iteration.  If the longest observation somehow matches or exceeds the length of
          * the next two almost-longest observations twice over... we have a very strong
          * 'context switch' candidate.
          *
          * Or, in near-formal math/stats:  we expect a very low variance in execution
          * time among the iterations of the search's loops.  With a very low variance,
          * ANY significant proportional spikes in execution time are outliers - outliers
          * likely caused by an OS context switch.
          *
          * Rather than do intensive math, we use a somewhat lazy approach below that
          * achieves the same net results given our assumptions, even when relaxed somewhat.
          *
          * The logic below relaxes the base assumptions a bit to be safe:
          * - [2ms, 2ms, 8ms]  will cause 8ms to be seen as an outlier.
          * - [2ms, 3ms, 10ms] will cause 10ms to be seen as an outlier.
          *
          * Ideally:
          * - [1ms, 1ms, 4ms] will view 4ms as an outlier.
          *
          * So we can safely handle slightly longer average intervals and slightly shorter
          * OS context-switch time intervals.
          */
        if(this.largestIntervals.length > 2) {
          // Precondition:  the `largestIntervals` array is sorted in ascending order.
          // Shortest entry is at the head, longest at the tail.
          if(this.largestIntervals[2] >= 2 * (this.largestIntervals[0] + this.largestIntervals[1])) {
            this.executionTime -= this.largestIntervals[2];
            this.largestIntervals.pop();
          }
        }
      }

      shouldTimeout(): boolean {
        const now = Date.now();
        if(now - this.start > this.maxTrueTime) {
          return true;
        }

        return this.executionTime > this.maxExecutionTime;
      }

      resetOutlierCheck() {
        this.largestIntervals = [];
      }
    }

    let batcher = new SearchBatcher(this.returnedValues);

    const timer = new ExecutionTimer(maxTime*1.5, maxTime);

    // Stage 1 - if we already have extracted results, build a queue just for them and iterate over it first.
    let returnedValues = Object.values(this.returnedValues);
    if(returnedValues.length > 0) {
      let preprocessedQueue = new PriorityQueue<SearchNode>(QUEUE_NODE_COMPARATOR, returnedValues);

      // Build batches of same-cost entries.
      timer.startLoop();
      while(preprocessedQueue.count > 0) {
        let entry = preprocessedQueue.dequeue();

        // Is the entry a reasonable result?
        if(entry.isFullReplacement) {
          // If the entry's 'match' fully replaces the input string, we consider it
          // unreasonable and ignore it.
          continue;
        }

        let batch = batcher.checkAndAdd(entry);
        timer.markIteration();

        if(batch) {
          // Do not track yielded time.
          yield batch;
        }
      }

      // As we only return a batch once all entries of the same cost have been processed, we can safely
      // finalize the last preprocessed group without issue.
      let batch = batcher.tryFinalize();
      if(batch) {
        // Do not track yielded time.
        yield batch;
      }
    }

    // Stage 2:  the fun part; actually searching!
    timer.resetOutlierCheck();
    timer.startLoop();
    let timedOut = false;
    do {
      let newResult: PathResult;

      // Search for a 'complete' path, skipping all partial paths as long as time remains.
      do {
        newResult = this.handleNextNode();
        timer.markIteration();

        if(timer.shouldTimeout()) {
          timedOut = true;
        }
      } while(!timedOut && newResult.type == 'intermediate')

      // TODO:  check 'cost' on intermediate, running it through batcher to early-detect cost changes.
      let batch: SearchResult[];
      if(newResult.type == 'none') {
        break;
      } else if(newResult.type == 'complete') {
        // Is the entry a reasonable result?
        if(newResult.finalNode.isFullReplacement) {
          // If the entry's 'match' fully replaces the input string, we consider it
          // unreasonable and ignore it.  Also, if we've reached this point...
          // we can(?) assume that everything thereafter is as well.
          break;
        }
        batch = batcher.checkAndAdd(newResult.finalNode);
      }

      if(batch) {
        yield batch;
      }
    } while(!timedOut && this.hasNextMatchEntry());

    // If we _somehow_ exhaust all search options, make sure to return the final results.
    let batch = batcher.tryFinalize();
    if(batch) {
      yield batch;
    }

    return null;
  }
}
