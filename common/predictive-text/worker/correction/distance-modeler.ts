/// <reference path="classical-calculation.ts" />

namespace correction {
  type RealizedInput = ProbabilityMass<Transform>[];  // NOT Distribution - they're masses from separate distributions.

  export type TraversableToken<TUnit> = {
    key: TUnit,
    traversal: LexiconTraversal
  }

  export const QUEUE_NODE_COMPARATOR: models.Comparator<SearchNode> = function(arg1, arg2) {
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

    constructor(rootTraversal: LexiconTraversal, toKey?: (USVString) => USVString);
    constructor(node: SearchNode);
    constructor(rootTraversal: LexiconTraversal | SearchNode, toKey?: (USVString) => USVString) {
      toKey = toKey || (x => x);

      if(rootTraversal instanceof SearchNode) {
        let priorNode = rootTraversal;
        this.calculation = priorNode.calculation;
        this.currentTraversal = priorNode.currentTraversal;
        this.priorInput = priorNode.priorInput;
        this.toKey = priorNode.toKey;
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

        // TODO:  Check against cache(s) & cache results.
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

        let inputPath = Array.from(this.priorInput);
        inputPath.push(probMass);
        // Tokenize and iterate over input chars, adding them into the calc.
        for(let i=0; i < transform.insert.length; i++) {
          let char = transform.insert[i];
          if(models.isHighSurrogate(char)) {
            i++;
            char = char + transform.insert[i];
          }

          // TODO:  Check against cache, write results to that cache.
          edgeCalc = edgeCalc.addInputChar({key: this.toKey(char)});
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
  
          // TODO:  Check against cache(s), cache results.
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
      return inputString + models.SENTINEL_CODE_UNIT + matchString;
    }
  }

  class SearchSpaceTier {
    correctionQueue: models.PriorityQueue<SearchNode>;
    processed: SearchNode[] = [];
    index: number;

    constructor(index: number, initialEdges?: SearchNode[]) {
      this.index = index;
      this.correctionQueue = new models.PriorityQueue<SearchNode>(QUEUE_NODE_COMPARATOR, initialEdges);
    }

    increaseMaxEditDistance() {
      // By extracting the entries from the priority queue and increasing distance outside of it as a batch job,
      // we get an O(N) implementation, rather than the O(N log N) that would result from maintaining the original queue.
      let entries = this.correctionQueue.toArray();

      entries.forEach(function(edge) { edge.calculation = edge.calculation.increaseMaxDistance(); });

      // Since we just modified the stored instances, and the costs may have shifted, we need to re-heapify.
      this.correctionQueue = new models.PriorityQueue<SearchNode>(QUEUE_NODE_COMPARATOR, entries);
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

  // The set of search spaces corresponding to the same 'context' for search.
  // Whenever a wordbreak boundary is crossed, a new instance should be made.
  export class SearchSpace {
    private QUEUE_SPACE_COMPARATOR: models.Comparator<SearchSpaceTier>;
    
    // p = 1 / (e^4) = 0.01831563888.  This still exceeds many neighboring keys!
    // p = 1 / (e^5) = 0.00673794699.  Strikes a good balance.
    // Should easily give priority to neighboring keys before edit-distance kicks in (when keys are a bit ambiguous)
    static readonly EDIT_DISTANCE_COST_SCALE = 5;
    static readonly MIN_KEYSTROKE_PROBABILITY = 0.0001;

    private tierOrdering: SearchSpaceTier[] = [];
    private selectionQueue: models.PriorityQueue<SearchSpaceTier>;
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

    constructor(model: LexicalModel) {
      if(!model) {
        throw "The LexicalModel parameter must not be null / undefined.";
      } else if(!model.traverseFromRoot) {
        throw "The provided model does not implement the `traverseFromRoot` function, which is needed to support robust correction searching.";
      }

      // Constructs the comparator needed for the following line.
      this.buildQueueSpaceComparator();
      this.selectionQueue = new models.PriorityQueue<SearchSpaceTier>(this.QUEUE_SPACE_COMPARATOR);
      this.rootNode = new SearchNode(model.traverseFromRoot(), model.toKey ? model.toKey.bind(model) : null);

      this.completedPaths = [this.rootNode];

      // Adds a base level queue to handle initial insertions.
      let baseTier = new SearchSpaceTier(0, this.rootNode.buildInsertionEdges());
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

        // TODO:  rework this as a constant property of each tier.  We can simply update the costs as new inputs arrive,
        //        which will also reduce the number of needed calculations.
        //
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

    findNextMatch(): SearchNode {
      while(this.hasNextMatchEntry()) {
        let bestTier = this.selectionQueue.dequeue();
        let currentNode = bestTier.correctionQueue.dequeue();

        // Have we already processed a matching edge?  If so, skip it.
        // We already know the previous edge is of lower cost.
        if(this.processedEdgeSet[currentNode.mapKey]) {
          this.selectionQueue.enqueue(bestTier);
          continue;
        } else {
          this.processedEdgeSet[currentNode.mapKey] = true;
        }

        // Stage 1:  filter out nodes/edges we want to prune
        
        // Forbid a raw edit-distance of greater than 2.
        // Note:  .knownCost is not scaled, while its contribution to .currentCost _is_ scaled.
        let substitutionsOnly = false;
        if(currentNode.knownCost > 2) {
          continue;
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
          continue;
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

          return currentNode;
        } else {
          // Time to construct new edges for the next tier!
          let nextTier = this.tierOrdering[bestTier.index+1];
          
          let inputIndex = nextTier.index;

          let deletionEdges = [];
          if(!substitutionsOnly) {
            deletionEdges       = currentNode.buildDeletionEdges(this.inputSequence[inputIndex-1]);
          }     
          let substitutionEdges = currentNode.buildSubstitutionEdges(this.inputSequence[inputIndex-1]);

          // Note:  we're live-modifying the tier's cost here!  The priority queue loses its guarantees as a result.
          nextTier.correctionQueue.enqueueAll(deletionEdges.concat(substitutionEdges));

          // So, we simply rebuild the selection queue.
          this.selectionQueue = new models.PriorityQueue<SearchSpaceTier>(this.QUEUE_SPACE_COMPARATOR, this.tierOrdering);

          // We didn't reach an end-node, so we just end the iteration and continue the search.
        }
      }

      // If we've somehow fully exhausted all search options, indicate that none remain.
      return null;
    }

    // Current best guesstimate of how compositor will retrieve ideal corrections.
    *getBestMatches(): Generator<SearchResult[]> { 
      // might should also include a 'base cost' parameter of sorts?
      let searchSpace = this;
      let currentReturns: {[mapKey: string]: SearchNode} = {};

      class BatchingAssistant {
        currentCost = Number.MIN_SAFE_INTEGER;
        entries: SearchResult[] = [];

        checkAndAdd(entry: SearchNode): SearchResult[] | null {
          var result: SearchResult[] = null;

          if(entry.currentCost > this.currentCost) {
            result = this.tryFinalize();

            this.currentCost = entry.currentCost;
          }

          // Filter out any duplicated match sequences.  The same match sequence may be reached via
          // different input sequences, after all.
          let outputMapKey = entry.calculation.matchSequence.map(value => value.key).join('');

          // First, ensure the edge has an existing 'shared' cache entry.
          if(!searchSpace.returnedValues[outputMapKey]) {
            searchSpace.returnedValues[outputMapKey] = entry;
          }

          // Check the generator's local returned-value cache - this determines whether or not we
          // need to add a new 'return' to the batch.
          if(!currentReturns[outputMapKey]) {
            this.entries.push(new SearchResult(entry));
            currentReturns[outputMapKey] = entry;
          }

          return result;
        }

        tryFinalize(): SearchResult[] | null {
          var result: SearchResult[] = null;
          if(this.entries.length > 0) {
            result = this.entries;
            this.entries = [];
          }

          return result;
        }
      }

      let batcher = new BatchingAssistant();
      let batch: SearchResult[];

      // Stage 1 - if we already have extracted results, build a queue just for them and iterate over it first.
      let returnedValues = Object.values(this.returnedValues);
      if(returnedValues.length > 0) {
        let preprocessedQueue = new models.PriorityQueue<SearchNode>(QUEUE_NODE_COMPARATOR, returnedValues);

        // Build batches of same-cost entries.
        while(preprocessedQueue.count > 0) {
          let entry = preprocessedQueue.dequeue();
          batch = batcher.checkAndAdd(entry);

          if(batch) {
            yield batch;
          }
        }

        // As we only return a batch once all entries of the same cost have been processed, we can safely
        // finalize the last preprocessed group without issue.
        batch = batcher.tryFinalize();
        if(batch) {
          yield batch;
        }
      }

      // Stage 2:  the fun part; actually searching!
      do {
        let newResult = this.findNextMatch();
        if(!newResult) {
          break;
        }
        batch = batcher.checkAndAdd(newResult);

        if(batch) {
          yield batch;
        }
      } while(this.hasNextMatchEntry());

      // If we _somehow_ exhaust all search options, make sure to return the final results.
      batch = batcher.tryFinalize();
      if(batch) {
        yield batch;
      }

      return null;
    }
  }
}
